module TypeDocumentationComment exposing (rule)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import List.Extra
import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "TypeDocumentationComment" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Rule.Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.CustomTypeDeclaration typeInfo ->
            case typeInfo.documentation of
                Just docNode ->
                    let
                        docString =
                            Node.value docNode

                        typeDefinition =
                            getTypeDefinitionFromDoc docString
                    in
                    case typeDefinition of
                        Nothing ->
                            [ Rule.error
                                { message = "型のドキュメントに型定義が含まれていません"
                                , details =
                                    [ "カスタム型のドキュメントには、型定義を含める必要があります。"
                                    , "ドキュメントコメント内に型定義を追加してください。"
                                    ]
                                }
                                (Node.range docNode)
                            ]

                        Just defString ->
                            case parseTypeDefinition defString of
                                Ok parsedType ->
                                    if compareTypeDeclarations parsedType typeInfo then
                                        []

                                    else
                                        [ Rule.error
                                            { message = "型のドキュメントが実際の型定義と一致していません"
                                            , details =
                                                [ "ドキュメントの型定義は、実際の型定義と完全に一致する必要があります。"
                                                , "ドキュメントの型定義を更新して、実装と一致させてください。"
                                                ]
                                            }
                                            (Node.range docNode)
                                        ]

                                Err error ->
                                    [ Rule.error
                                        { message = "型のドキュメント内の型定義が不正です"
                                        , details =
                                            [ "ドキュメント内の型定義をパースできませんでした。"
                                            , "正しい型定義の形式になっているか確認してください。"
                                            , "エラー: " ++ error
                                            ]
                                        }
                                        (Node.range docNode)
                                    ]

                Nothing ->
                    [ Rule.error
                        { message = "カスタム型のドキュメントが不足しています"
                        , details =
                            [ "すべてのカスタム型にはドキュメントが必要です。"
                            , "型定義を含むドキュメントコメントを追加してください。"
                            ]
                        }
                        (Node.range node)
                    ]

        _ ->
            []


{-| ドキュメントから型定義部分を抽出する。
型定義は必ずドキュメントコメントの最後に配置されることを前提とする。

    -- 以下のような形式のコメントから型定義部分を抽出する：

    {-| これは説明文です。
    いくつかの段落があります。

    type MyType
    = A
    | B Int

    -}

-}
getTypeDefinitionFromDoc : String -> Maybe String
getTypeDefinitionFromDoc doc =
    let
        lines =
            String.lines doc
                |> List.map String.trimRight

        -- 最後の型定義ブロックを見つける
        findLastTypeBlock : List String -> Maybe (List String)
        findLastTypeBlock inputLines =
            let
                -- 型定義の開始行を見つける
                isTypeDefLine line =
                    String.startsWith "type" (String.trimLeft line)

                -- 型定義ブロックを収集する
                collectTypeBlock : List String -> ( List String, List String )
                collectTypeBlock remainingLines =
                    case remainingLines of
                        [] ->
                            ( [], [] )

                        line :: rest ->
                            if isTypeDefLine line then
                                collectLinesUntilEmpty (line :: []) rest

                            else
                                case collectTypeBlock rest of
                                    ( block, trailing ) ->
                                        ( block, line :: trailing )

                -- 空行が来るまで、もしくは最後まで行を収集する
                collectLinesUntilEmpty : List String -> List String -> ( List String, List String )
                collectLinesUntilEmpty accumulated remainingLines =
                    case remainingLines of
                        [] ->
                            ( List.reverse accumulated, [] )

                        line :: rest ->
                            if String.trim line == "" then
                                ( List.reverse accumulated, rest )

                            else
                                collectLinesUntilEmpty (line :: accumulated) rest
            in
            case collectTypeBlock inputLines of
                ( [], _ ) ->
                    Nothing

                ( block, _ ) ->
                    Just block

        -- インデントを正規化する
        normalizeIndentation : List String -> String
        normalizeIndentation typeLines =
            let
                -- 各行の先頭の空白の数を数える（タブは4つの空白として扱う）
                countLeadingSpaces : String -> Int
                countLeadingSpaces line =
                    if String.trim line == "" then
                        0

                    else
                        String.length line - String.length (String.trimLeft line)

                -- 最小のインデントを見つける（空行は除く）
                minIndent =
                    typeLines
                        |> List.filter ((/=) "" << String.trim)
                        |> List.map countLeadingSpaces
                        |> List.minimum
                        |> Maybe.withDefault 0

                -- 各行から最小のインデントを取り除く
                removeIndent : String -> String
                removeIndent line =
                    if String.trim line == "" then
                        ""

                    else
                        String.dropLeft minIndent line
            in
            typeLines
                |> List.map removeIndent
                |> String.join "\n"
    in
    lines
        |> findLastTypeBlock
        |> Maybe.map normalizeIndentation


{-| 型定義文字列をパースしてASTに変換する
-}
parseTypeDefinition : String -> Result String Type
parseTypeDefinition typeDefString =
    let
        moduleString =
            String.join "\n"
                [ "module Temp exposing (..)"
                , ""
                , typeDefString
                ]
    in
    case Elm.Parser.parse moduleString of
        Ok rawFile ->
            let
                file =
                    Elm.Processing.process Elm.Processing.init rawFile

                maybeTypeDecl =
                    file.declarations
                        |> List.head
                        |> Maybe.map Node.value
            in
            case maybeTypeDecl of
                Just (Declaration.CustomTypeDeclaration typeDecl) ->
                    Ok typeDecl

                _ ->
                    Err "The string was not a valid type definition"

        Err _ ->
            Err "Failed to parse the type definition"


compareTypeDeclarations : Type -> Type -> Bool
compareTypeDeclarations typeA typeB =
    (Node.value typeA.name == Node.value typeB.name)
        && (List.map Node.value typeA.generics == List.map Node.value typeB.generics)
        && (List.length typeA.constructors == List.length typeB.constructors)
        && (List.map2 compareValueConstructors
                (List.map Node.value typeA.constructors)
                (List.map Node.value typeB.constructors)
                |> List.all identity
           )


{-| 値コンストラクタを比較する
-}
compareValueConstructors : ValueConstructor -> ValueConstructor -> Bool
compareValueConstructors constructorA constructorB =
    (Node.value constructorA.name == Node.value constructorB.name)
        && (List.map2 compareTypeAnnotations
                (List.map Node.value constructorA.arguments)
                (List.map Node.value constructorB.arguments)
                |> List.all identity
           )


{-| 型注釈を比較する
-}
compareTypeAnnotations : TypeAnnotation -> TypeAnnotation -> Bool
compareTypeAnnotations typeA typeB =
    case ( typeA, typeB ) of
        -- 既存のケース
        ( TypeAnnotation.GenericType nameA, TypeAnnotation.GenericType nameB ) ->
            nameA == nameB

        ( TypeAnnotation.Typed nodeA argsA, TypeAnnotation.Typed nodeB argsB ) ->
            (Node.value nodeA == Node.value nodeB)
                && List.length argsA
                == List.length argsB
                && List.all identity (List.map2 (\a b -> compareTypeAnnotations (Node.value a) (Node.value b)) argsA argsB)

        ( TypeAnnotation.Unit, TypeAnnotation.Unit ) ->
            True

        ( TypeAnnotation.FunctionTypeAnnotation fromA toA, TypeAnnotation.FunctionTypeAnnotation fromB toB ) ->
            compareTypeAnnotations (Node.value fromA) (Node.value fromB)
                && compareTypeAnnotations (Node.value toA) (Node.value toB)

        -- レコード型の比較を修正
        ( TypeAnnotation.Record fieldsA, TypeAnnotation.Record fieldsB ) ->
            compareRecordFields fieldsA fieldsB

        -- その他のケースは不一致とみなす
        _ ->
            False


compareRecordFields : List (Node ( Node String, Node TypeAnnotation )) -> List (Node ( Node String, Node TypeAnnotation )) -> Bool
compareRecordFields fieldsA fieldsB =
    let
        sortedFieldsA =
            fieldsA
                |> List.map Node.value
                |> List.sortBy (Tuple.first >> Node.value)

        sortedFieldsB =
            fieldsB
                |> List.map Node.value
                |> List.sortBy (Tuple.first >> Node.value)
    in
    List.length sortedFieldsA
        == List.length sortedFieldsB
        && List.all identity (List.map2 compareField sortedFieldsA sortedFieldsB)


compareField : ( Node String, Node TypeAnnotation ) -> ( Node String, Node TypeAnnotation ) -> Bool
compareField ( nameA, typeA ) ( nameB, typeB ) =
    Node.value nameA == Node.value nameB && compareTypeAnnotations (Node.value typeA) (Node.value typeB)

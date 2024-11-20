module DocumentationRule exposing (rule)

{-|


## ドキュメンテーションルール

Elm モジュールのドキュメント規則を強制します。


### 要件

  - モジュールドキュメント: 詳細な説明と適切な見出しレベル
  - 関数・型のドキュメント: 短くても良いが必須

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "DocumentationRule" ()
        |> Rule.withModuleDocumentationVisitor moduleDocumentationVisitor
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


moduleDocumentationVisitor : Maybe (Node String) -> context -> ( List (Rule.Error {}), context )
moduleDocumentationVisitor maybeDocComment context =
    case maybeDocComment of
        Nothing ->
            ( [ Rule.error
                    { message = "Missing module documentation"
                    , details =
                        [ "モジュールの目的と機能を説明するドキュメントが必要です。"
                        , "以下のフォーマットで記述してください："
                        , """例:
{-|
## モジュール名

モジュールの説明

### 主な機能
- 機能1
- 機能2

### 使用方法
使用例など
-}"""
                        ]
                    }
                    { start = { row = 1, column = 1 }, end = { row = 1, column = 1 } }
              ]
            , context
            )

        Just docNode ->
            let
                doc =
                    Node.value docNode

                docLength =
                    String.length (String.trim doc)

                headerValidation =
                    validateHeaders doc
            in
            case headerValidation of
                Err headerError ->
                    ( [ Rule.error
                            { message = "モジュールドキュメントのヘッダー形式が不正です"
                            , details = headerError
                            }
                            (Node.range docNode)
                      ]
                    , context
                    )

                Ok () ->
                    if docLength < 100 then
                        ( [ Rule.error
                                { message = "モジュールのドキュメントが不十分です"
                                , details =
                                    [ "モジュールのドキュメントは最低100文字以上必要です。現在: " ++ String.fromInt docLength ++ "文字"
                                    , "以下の内容を含めてください："
                                    , "- モジュールの目的"
                                    , "- 主な機能"
                                    , "- 使用方法"
                                    ]
                                }
                                (Node.range docNode)
                          ]
                        , context
                        )

                    else
                        ( [], context )


declarationVisitor : Node Declaration -> List (Rule.Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { documentation, declaration } ->
            case documentation of
                Nothing ->
                    [ Rule.error
                        { message = "Missing function documentation"
                        , details =
                            [ "関数 `" ++ Node.value (Node.value declaration).name ++ "` にドキュメントが必要です。"
                            , "短い説明でも構いませんので、関数の目的を説明してください。"
                            , """例:
{-| ユーザー名を検証する
-}"""
                            ]
                        }
                        (Node.range node)
                    ]

                Just _ ->
                    []

        Declaration.CustomTypeDeclaration { documentation, name } ->
            case documentation of
                Nothing ->
                    [ Rule.error
                        { message = "Missing type documentation"
                        , details =
                            [ "型 `" ++ Node.value name ++ "` にドキュメントが必要です。"
                            , "短い説明でも構いませんので、型の目的を説明してください。"
                            , """例:
{-| ユーザーの状態を表す
-}"""
                            ]
                        }
                        (Node.range node)
                    ]

                Just _ ->
                    []

        Declaration.AliasDeclaration { documentation, name } ->
            case documentation of
                Nothing ->
                    [ Rule.error
                        { message = "Missing type alias documentation"
                        , details =
                            [ "型エイリアス `" ++ Node.value name ++ "` にドキュメントが必要です。"
                            , "短い説明でも構いませんので、型の目的を説明してください。"
                            , """例:
{-| ユーザー情報を格納する
-}"""
                            ]
                        }
                        (Node.range node)
                    ]

                Just _ ->
                    []

        _ ->
            []


validateHeaders : String -> Result (List String) ()
validateHeaders doc =
    let
        lines =
            String.lines doc

        hasLevel2Header =
            List.any (String.startsWith "## ") lines

        hasInvalidHeaders =
            List.any (\line -> String.startsWith "# " line || String.startsWith "## " line && not (String.startsWith "## " (String.trimLeft line))) lines

        hasWrongLevelHeaders =
            List.any
                (\line ->
                    String.startsWith "#" line
                        && not (String.startsWith "## " (String.trimLeft line))
                        && not (String.startsWith "### " (String.trimLeft line))
                        && not (String.startsWith "#### " (String.trimLeft line))
                )
                lines
    in
    if not hasLevel2Header then
        Err
            [ "モジュールドキュメントは## (見出し2) で始める必要があります"
            , "正しい例:"
            , """## モジュール名

### セクション1
内容..."""
            ]

    else if hasInvalidHeaders then
        Err
            [ "見出し2 (##) は最初の見出しとしてのみ使用してください"
            , "モジュール内の他のセクションでは ### か #### を使用してください"
            ]

    else if hasWrongLevelHeaders then
        Err
            [ "不適切な見出しレベルが使用されています"
            , "- モジュール名には ## を使用"
            , "- その他のセクションには ### か #### を使用"
            ]

    else
        Ok ()

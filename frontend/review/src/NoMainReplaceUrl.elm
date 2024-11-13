module NoMainReplaceUrl exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Exposing as Exposing
import Review.Rule as Rule exposing (Rule)

type alias Context =
    { replaceUrlInScope : Bool
    , navigationModules : List (List String)
    }

rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMainReplaceUrl" initialContext
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema
        |> Rule.filterErrorsForFiles (\path -> path == "src/Main.elm")

initialContext : Context
initialContext =
    { replaceUrlInScope = False
    , navigationModules = [ [ "Browser", "Navigation" ] ]
    }

importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
importVisitor node context =
    let
        import_ : Import
        import_ =
            Node.value node

        moduleName : List String
        moduleName =
            Node.value import_.moduleName

        isNavigationImport : Bool
        isNavigationImport =
            List.member moduleName context.navigationModules

        checkExposing : Exposing.Exposing -> Bool
        checkExposing exposing_ =
            case exposing_ of
                Exposing.All _ ->
                    True

                Exposing.Explicit exposedList ->
                    List.any
                        (\exposed ->
                            case Node.value exposed of
                                Exposing.FunctionExpose name ->
                                    name == "replaceUrl"

                                _ ->
                                    False
                        )
                        exposedList

        hasReplaceUrlExposed : Bool
        hasReplaceUrlExposed =
            import_.exposingList
                |> Maybe.map Node.value
                |> Maybe.map checkExposing
                |> Maybe.withDefault False

        aliasModules : List (List String)
        aliasModules =
            import_.moduleAlias
                |> Maybe.map Node.value
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    if isNavigationImport then
        ( []
        , { context
            | replaceUrlInScope = context.replaceUrlInScope || hasReplaceUrlExposed
            , navigationModules = aliasModules ++ context.navigationModules
          }
        )
    else
        ( [], context )

expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue moduleName "replaceUrl" ->
            if moduleName == [] && context.replaceUrlInScope then
                ( [ error node ], context )
            else if List.member moduleName context.navigationModules then
                ( [ error node ], context )
            else
                ( [], context )

        _ ->
            ( [], context )

error : Node Expression -> Rule.Error {}
error node =
    Rule.error
        { message = "Do not use Browser.Navigation.replaceUrl in Main.elm"
        , details =
            [ "画面表示不正による画面リダイレクトを行う処理は Page/ 配下のモジュールで実装してください。"
            , "Main モジュールでの Browser.Navigation の直接使用は避け、適切なページモジュールに遷移ロジックを移動してください。"
            ]
        }
        (Node.range node)
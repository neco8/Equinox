module Pages.SettingsPage exposing
    ( Model, Msg
    , init
    , update
    , view
    , noOp
    )

{-|


## Settings

このモジュールは、設定画面を表示するためのモジュールです。


### 型

@docs Model, Msg


### 初期化関数

@docs init


### アップデート

@docs update


### ビュー

@docs view

-}

import Browser.Navigation as Nav
import Html exposing (Html, button, div, h2, p, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Icon
import Nav
import Route exposing (Route(..))
import View exposing (View)


{-| モデル
-}
type alias Model =
    { step : Step
    }


{-| 初期化関数
-}
init : () -> ( Model, Cmd Msg )
init _ =
    ( { step = Settings
      }
    , Cmd.none
    )


{-| メッセージ
-}
type Msg
    = NavigateToRoute Route
    | GoToNotYetImplemented { title : String }
    | GoBackToSettings
    | GoBack
    | NoOp


{-| ステップ
-}
type Step
    = Settings
    | NotYetImplemented { title : String }


{-| メッセージ: NoOp

画面更新用

-}
noOp : Msg
noOp =
    NoOp


{-| アップデート
-}
update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route) )

        GoToNotYetImplemented to ->
            ( { model | step = NotYetImplemented to }, Cmd.none )

        GoBackToSettings ->
            ( { model | step = Settings }, Cmd.none )

        GoBack ->
            ( model, Nav.back key 1 )


{-| 設定項目のビュー
-}
viewSettingItem : { goToItem : Msg, isDanger : Bool, label : String } -> Html Msg
viewSettingItem { goToItem, isDanger, label } =
    button
        [ onClick goToItem
        , class "w-full flex items-center justify-between px-6 py-4 hover:bg-gray-50 transition-colors duration-200"
        ]
        [ span
            [ class "text-sm"
            , if isDanger then
                class "text-red-500"

              else
                class "text-gray-900"
            ]
            [ text label
            ]
        , div [ class "flex items-center" ]
            [ Icon.view Icon.ChevronRight
            ]
        ]


{-| 設定項目一覧のビュー
-}
viewSettingsGroup : { group | title : String } -> List (Html Msg) -> Html Msg
viewSettingsGroup { title } items =
    div [ class "mb-8" ]
        [ h2 [ class "px-6 py-3 text-sm font-medium text-gray-500" ] [ text title ]
        , div [ class "bg-white rounded-xl overflow-hidden shadow-md shadow-blue-100 divide-y divide-gray-100" ] items
        ]


{-| 設定グループ一覧の設定値
-}
settingGroups : List { title : String, items : List { goToItem : Msg, isDanger : Bool, label : String } }
settingGroups =
    [ { title = "総合"
      , items =
            [ { goToItem = GoToNotYetImplemented { title = "ヘルスケア連携" }
              , label = "ヘルスケア連携"
              , isDanger = False
              }
            , { goToItem = GoToNotYetImplemented { title = "クーポン" }
              , label = "クーポン"
              , isDanger = False
              }
            , { goToItem = GoToNotYetImplemented { title = "テーマ設定" }
              , label = "テーマ設定"
              , isDanger = False
              }
            , { goToItem = GoToNotYetImplemented { title = "データのエクスポート" }
              , label = "データのエクスポート"
              , isDanger = False
              }

            -- , { goToItem = GoToNotYetImplemented
            --   , label = "通知設定"
            --   , isDanger = False
            --   }
            -- , { goToItem = GoToNotYetImplemented
            --   , label = "言語設定"
            --   , isDanger = False
            --   }
            ]
      }
    , { title = "重要"
      , items =
            [ { goToItem = GoToNotYetImplemented { title = "アプリの初期化" }
              , label = "アプリの初期化"
              , isDanger = True
              }
            ]
      }
    , { title = "情報"
      , items =
            [ { goToItem = GoToNotYetImplemented { title = "利用規約" }
              , label = "利用規約"
              , isDanger = False
              }
            , { goToItem = GoToNotYetImplemented { title = "プライバシーポリシー" }
              , label = "プライバシーポリシー"
              , isDanger = False
              }
            , { goToItem = GoToNotYetImplemented { title = "このアプリについて" }
              , label = "このアプリについて"
              , isDanger = False
              }
            ]
      }
    ]


{-| ビュー
-}
view : Model -> View Msg
view model =
    { nav =
        Just
            (Nav.BackNav
                { goBack =
                    case model.step of
                        Settings ->
                            GoBack

                        _ ->
                            GoBackToSettings
                , title =
                    case model.step of
                        Settings ->
                            "設定"

                        NotYetImplemented { title } ->
                            title
                }
            )
    , footer = False
    , view =
        div
            [ attribute "role" "settings"
            , class "pt-4 pb-20 px-4 max-w-2xl mx-auto"
            ]
        <|
            case model.step of
                Settings ->
                    List.map
                        (\group ->
                            viewSettingsGroup group <|
                                List.map viewSettingItem group.items
                        )
                        settingGroups

                NotYetImplemented { title } ->
                    [ div [ class "flex flex-col items-center justify-center px-6 py-12 text-center" ]
                        [ div [ class "flex justify-center space-x-2 text-2xl" ]
                            [ span [ class "animate-pulse" ] [ text "✨" ]
                            , span [ class "animate-spin" ] [ text "🔧" ]
                            , span [ class "animate-pulse" ] [ text "✨" ]
                            ]
                        , div [ class "space-y-3" ]
                            [ h2
                                [ class "text-2xl font-bold text-gray-900" ]
                                [ text "Coming soon!" ]
                            , p
                                [ class "text-gray-600 text-lg" ]
                                [ text "この機能は絶賛開発中です"
                                ]
                            ]
                        , div [ class "flex justify-center space-x-3 text-2xl pt-4" ]
                            [ span [ class "animate-pulse" ] [ text "🚀" ]
                            , span [ class "animate-bounce" ] [ text "⚡" ]
                            , span [ class "animate-pulse" ] [ text "🎯" ]
                            ]
                        , div [ class "mt-12 p-4 bg-blue-50 rounded-xl max-w-sm" ]
                            [ p [ class "text-blue-600 flex items-center justify-center space-x-2" ]
                                [ span [] [ text <| "もうすぐ" ++ title ++ "が完成します" ]
                                , span [ class "text-xl" ] [ text "💪" ]
                                ]
                            ]
                        ]
                    ]
    }

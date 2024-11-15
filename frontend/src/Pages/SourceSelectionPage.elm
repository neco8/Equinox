module Pages.SourceSelectionPage exposing
    ( view
    , update, Model, Msg
    , noOp, init
    )

{-|


## Source Selection Page

このモジュールは、ソース選択ページのビューと更新ロジックを提供します。


### ビュー

@docs view

このモジュールには、ユーザーがソースを選択するためのページのビューと更新ロジックが含まれています。

@docs update, Model, Msg

@docs noOp, init

-}

import API.OnlineBreathingMethod exposing (OnlineBreathingMethod)
import Browser.Navigation as Nav
import Config exposing (Config)
import Html exposing (Html, button, div, h1, h2, header, p, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import Icon
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Task
import Time
import Types.BreathingMethod exposing (fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, fromName)
import Uuid
import View exposing (View)


{-| ソース選択の状態を表す型
-}
type SourceSelection
    = SourceSelection
    | OnlineList (RemoteData API.OnlineBreathingMethod.Error (List OnlineBreathingMethod))


{-| モデル
-}
type alias Model =
    { sourceSelection : SourceSelection
    }


{-| 初期化関数
-}
init : () -> ( Model, Cmd Msg )
init _ =
    ( { sourceSelection = SourceSelection
      }
    , Cmd.none
    )


{-| メッセージ
-}
type Msg
    = OpenOnlineList
    | OpenManualInput
    | GotOnlineBreathingMethods (Result API.OnlineBreathingMethod.Error (List OnlineBreathingMethod))
    | NavigateToRoute Route
    | NoOp


{-| メッセージ: NoOp

画面更新用に利用される。

-}
noOp : Msg
noOp =
    NoOp


{-| アップデート関数
-}
update : Config -> Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update config key msg model =
    case msg of
        OpenOnlineList ->
            ( { model | sourceSelection = OnlineList Loading }
            , API.OnlineBreathingMethod.getOnlineBreathingMethods config GotOnlineBreathingMethods
            )

        OpenManualInput ->
            ( model
            , NavigateToRoute
                (Route.BreathingMethodAddRoute Nothing Nothing Nothing Nothing Nothing)
                |> always
                |> Task.perform
                |> (|>) Time.now
            )

        GotOnlineBreathingMethods result ->
            case ( result, model.sourceSelection ) of
                ( Ok onlineBreathingMethods, OnlineList _ ) ->
                    ( { model | sourceSelection = OnlineList <| RemoteData.Success onlineBreathingMethods }
                    , Cmd.none
                    )

                ( Err error, OnlineList _ ) ->
                    ( { model | sourceSelection = OnlineList <| RemoteData.Failure error }
                    , Cmd.none
                    )

                ( _, SourceSelection ) ->
                    -- ソース選択画面ではオンラインリストを取得するかわからない状態なので、何もしない
                    ( model, Cmd.none )

        NavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route) )

        NoOp ->
            ( model, Cmd.none )


{-| オンラインの呼吸法を表示する
-}
viewOnlineBreathingMethod : Bool -> OnlineBreathingMethod -> Html Msg
viewOnlineBreathingMethod showUsers { name, id, inhale, inhaleHold, exhale, exhaleHold } =
    let
        users =
            0
    in
    button
        [ attribute "aria-label" (fromName name)
        , attribute "data-id" (Uuid.toString id)
        , onClick (NavigateToRoute (Route.BreathingMethodAddRoute (Just name) (Just inhale) (Just inhaleHold) (Just exhale) (Just exhaleHold)))
        , class "w-full p-4 text-left bg-white rounded-lg border shadow-sm hover:bg-blue-50 hover:border-blue-200 transition-all group"
        ]
        [ div [ class "flex items-start justify-between mb-2" ]
            [ h2 [ class "font-medium text-lg group-hover:text-blue-700 transition-colors" ] [ text <| fromName name ]
            , span [ class "opacity-0 hover:opacity-100 transition-opacity" ]
                [ Icon.view Icon.Cloud
                ]
            ]
        , div [ class "flex items-center gap-4 text-sm text-gray-600" ]
            [ if showUsers then
                div [ class "flex items-center gap-1" ]
                    [ Icon.view Icon.Users
                    , span [] [ text <| String.fromInt users ++ "人が実践中" ]
                    ]

              else
                text ""
            , div [ class "flex items-center gap-1" ]
                [ Icon.view Icon.Timer
                , span []
                    [ text <|
                        (String.fromInt <| fromInhaleDuration inhale)
                            ++ "-"
                            ++ (String.fromInt <| fromInhaleHoldDuration inhaleHold)
                            ++ "-"
                            ++ (String.fromInt <| fromExhaleDuration exhale)
                            ++ "-"
                            ++ (String.fromInt <| fromExhaleHoldDuration exhaleHold)
                    ]
                ]
            ]
        ]


{-| オンラインリストを表示する
-}
viewOnlineList : RemoteData API.OnlineBreathingMethod.Error (List OnlineBreathingMethod) -> Html Msg
viewOnlineList m =
    div
        [ attribute "role" "online-list"
        , class "h-full flex flex-col"
        ]
    <|
        --               <header className="mb-6">
        --     <div className="flex items-center gap-2 mb-2">
        --       <Globe className="h-5 w-5 text-blue-500" />
        --       <h1 className="text-xl font-semibold">オンライン呼吸法ライブラリ</h1>
        --     </div>
        --     <p className="text-gray-600">世界中のユーザーが実践している呼吸法を探索できます</p>
        --   </header>
        header [ class "mb-6" ]
            [ div [ class "flex items-center gap-2 mb-2" ]
                [ Icon.view Icon.Globe
                , h1 [ class "text-xl font-semibold" ] [ text "オンライン呼吸法ライブラリ" ]
                ]
            , p [ class "text-gray-600" ] [ text "みんなが実践している呼吸法を探索してみましょう" ]
            ]
            :: (case m of
                    Success onlineBreathingMethods ->
                        [ ul
                            [ attribute "aria-label" "online-list"
                            , class "space-y-3"
                            ]
                            (List.map (viewOnlineBreathingMethod False) onlineBreathingMethods)
                        ]

                    NotAsked ->
                        [ text "Not Asked" ]

                    Loading ->
                        [ text "Loading..." ]

                    Failure error ->
                        [ div [ class "border border-red-100 bg-red-50 rounded-lg p-4 mb-6" ]
                            [ div [ class "flex gap-2 items-start" ]
                                [ Icon.view Icon.Alert
                                , div []
                                    [ h2 [ class "font-medium text-red-900 mb-1" ]
                                        [ text "オンラインソースへ接続できません"
                                        ]
                                    , p [ class "text-sm text-red-700" ]
                                        [ text "まだ実装していない機能です！今しばらくお待ち下さい…🖊️"
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "bg-white rounded-lg border-2 border-dashed border-gray-200 p-8 flex flex-col items-center justify-center text-center" ]
                            [ div [ class "p-4 rounded-full bg-gray-100 mb-4" ] [ Icon.view Icon.Globe ]
                            , p [ class "text-gray-600 mb-6" ] [ text "オンラインライブラリを利用できません" ]
                            , div [ class "space-y-3 w-64" ]
                                [ button
                                    [ class "w-full inline-flex items-center justify-center gap-2 px-4 py-2.5 rounded-lg bg-blue-600 text-white hover:bg-blue-700 transition-colors disabled:opacity-30"
                                    , disabled True
                                    ]
                                    [ Icon.view Icon.Refresh
                                    , span [] [ text "再試行" ]
                                    ]
                                , button
                                    [ class "w-full inline-flex items-center justify-center gap-2 px-4 py-2.5 rounded-lg border border-gray-300 bg-white hover:bg-gray-50 transition-colors"
                                    , onClick (NavigateToRoute (Route.BreathingMethodAddRoute Nothing Nothing Nothing Nothing Nothing))
                                    ]
                                    [ Icon.view Icon.Pencil
                                    , span [] [ text "手入力" ]
                                    ]
                                ]
                            ]
                        ]
               )


{-| ソース選択画面を表示する
-}
viewSourceSelection : Html Msg
viewSourceSelection =
    div
        [ attribute "role" "source-selection"
        , class "h-full flex items-center justify-center"
        ]
        [ div [ class "w-full max-w-md space-y-4 px-4" ]
            [ button
                [ attribute "aria-label" "manual-source-selection-button"
                , onClick OpenManualInput
                , class "w-full flex items-center justify-center gap-3 p-3 rounded-lg border border-gray-400 bg-white hover:bg-gray-50 transition-colors"
                ]
                [ Icon.view Icon.Pencil
                , span [ class "text-lg" ] [ text "手入力" ]
                ]
            , button
                [ attribute "aria-label" "online-source-selection-button"
                , onClick OpenOnlineList
                , class "w-full flex items-center justify-center gap-3 p-3 rounded-lg bg-blue-500 hover:bg-blue-600 transition-colors text-white shadow-lg"
                ]
                [ Icon.view Icon.Globe
                , span [ class "text-lg" ]
                    [ text "オンラインソースへ"
                    ]
                ]
            ]
        ]


{-| ビュー
-}
view : Model -> View Msg
view model =
    { nav = False
    , footer = False
    , view =
        case model.sourceSelection of
            OnlineList onlineList ->
                viewOnlineList onlineList

            SourceSelection ->
                viewSourceSelection
    }

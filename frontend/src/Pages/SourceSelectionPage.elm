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
import Html exposing (Html, button, div, text, ul)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Task
import Time
import Types.BreathingMethod exposing (fromName)
import Uuid


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
viewOnlineBreathingMethod : OnlineBreathingMethod -> Html Msg
viewOnlineBreathingMethod { name, id, inhale, inhaleHold, exhale, exhaleHold } =
    button
        [ attribute "aria-label" (fromName name)
        , attribute "data-id" (Uuid.toString id)
        , onClick (NavigateToRoute (Route.BreathingMethodAddRoute (Just name) (Just inhale) (Just inhaleHold) (Just exhale) (Just exhaleHold)))
        ]
        [ text (fromName name) ]


{-| オンラインリストを表示する
-}
viewOnlineList : RemoteData API.OnlineBreathingMethod.Error (List OnlineBreathingMethod) -> Html Msg
viewOnlineList m =
    div [ attribute "role" "online-list" ]
        [ case m of
            Success onlineBreathingMethods ->
                ul [ attribute "aria-label" "online-list" ]
                    (List.map viewOnlineBreathingMethod onlineBreathingMethods)

            NotAsked ->
                text "Not Asked"

            Loading ->
                text "Loading..."

            Failure error ->
                text (API.OnlineBreathingMethod.errorToString error)
        ]


{-| ソース選択画面を表示する
-}
viewSourceSelection : Html Msg
viewSourceSelection =
    div [ attribute "role" "source-selection" ]
        [ text "ソース選択画面"
        , button
            [ attribute "aria-label" "manual-source-selection-button"
            , onClick OpenManualInput
            ]
            [ text "手動入力" ]
        , button
            [ attribute "aria-label" "online-source-selection-button"
            , onClick OpenOnlineList
            ]
            [ text "オンラインソースへ" ]
        ]


{-| ビュー
-}
view : Model -> Html Msg
view model =
    case model.sourceSelection of
        OnlineList onlineList ->
            viewOnlineList onlineList

        SourceSelection ->
            viewSourceSelection

module API.OnlineBreathingMethod exposing
    ( Error, OnlineBreathingMethod
    , checkDuplicate, errorToString, getOnlineBreathingMethods
    )

{-|


## Online Breathing Method API

このモジュールは、オンラインの呼吸法を取得するためのAPIを提供します。

オンラインの呼吸法には、カテゴリと作成日時が存在しません。


### 型

@docs Error, OnlineBreathingMethod

-}

import Config exposing (Config)
import Http
import JS.Codec exposing (encodeBreathingMethod)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as DE
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, Name)
import Uuid


{-| オンラインの呼吸法の型

オンライン呼吸法には、カテゴリと作成日時が存在しません。

-}
type alias OnlineBreathingMethod =
    { id : BreathingMethodId
    , name : Name
    , inhale : InhaleDuration
    , inhaleHold : InhaleHoldDuration
    , exhale : ExhaleDuration
    , exhaleHold : ExhaleHoldDuration
    }


{-| オンライン呼吸法のデコーダー
-}
onlineBreathingMethodDecoder : Decoder OnlineBreathingMethod
onlineBreathingMethodDecoder =
    D.succeed OnlineBreathingMethod
        |> DE.andMap (D.field "id" Uuid.decoder)
        |> DE.andMap (D.field "name" JS.Codec.nameDecoder)
        |> DE.andMap (D.field "inhale" JS.Codec.inhaleDecoder)
        |> DE.andMap (D.field "inhale-hold" JS.Codec.inhaleHoldDecoder)
        |> DE.andMap (D.field "exhale" JS.Codec.exhaleDecoder)
        |> DE.andMap (D.field "exhale-hold" JS.Codec.exhaleHoldDecoder)


{-| オンライン呼吸法を取得する際のデコーダー
-}
onlineBreathingMethodsDecoder : Decoder (List OnlineBreathingMethod)
onlineBreathingMethodsDecoder =
    D.field "breathing-methods" (D.list onlineBreathingMethodDecoder)


{-| 重複チェックのデコーダー
-}
duplicateDecoder : Decoder Bool
duplicateDecoder =
    D.field "duplicate" D.bool


{-| エラー型

  - [ ] TODO: 後でこちらを共通のエラー型に変更する

-}
type Error
    = HttpError Http.Error
    | DecodeError String
    | NetworkError
    | ServerError Int -- Status code
    | Timeout


{-| エラーを文字列に変換する
-}
errorToString : Error -> String
errorToString error =
    case error of
        HttpError httpError ->
            case httpError of
                Http.BadUrl url ->
                    "Invalid URL: " ++ url

                Http.Timeout ->
                    "Request timed out"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus status ->
                    "Server error: " ++ String.fromInt status

                Http.BadBody message ->
                    "Data error: " ++ message

        DecodeError message ->
            "Failed to decode response: " ++ message

        NetworkError ->
            "Network connection error"

        ServerError status ->
            "Server returned error: " ++ String.fromInt status

        Timeout ->
            "Request timed out"


{-| JSONとしてResponseを受け取るためのExpect
-}
expectJson : (Result Error a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (HttpError (Http.BadUrl url))

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (ServerError metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (DecodeError (D.errorToString err))


{-| オンライン呼吸法を取得するAPI
-}
getOnlineBreathingMethods : Config -> (Result Error (List OnlineBreathingMethod) -> msg) -> Cmd msg
getOnlineBreathingMethods config toMsg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = config.apiEndPoint ++ "/breathing-methods"
        , body = Http.emptyBody
        , expect = expectJson toMsg onlineBreathingMethodsDecoder
        , timeout = Just 10000
        , tracker = Nothing
        }


{-| 重複チェックを行うAPI
-}
checkDuplicate : Config -> BreathingMethod -> (Result Error Bool -> msg) -> Cmd msg
checkDuplicate config method toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = config.apiEndPoint ++ "/breathing-methods/check-duplicate"
        , body = Http.jsonBody (encodeBreathingMethod method)
        , expect = expectJson toMsg duplicateDecoder
        , timeout = Just 5000
        , tracker = Nothing
        }

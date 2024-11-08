module JS.Codec exposing
    ( encodePosix
    , encodeCategory, encodeCategories
    , encodeBreathingMethod, encodeBreathingMethods
    , encodeSession, encodeSessions
    , posixDecoder
    , categoryDecoder, categoriesDecoder
    , breathingMethodDecoder, breathingMethodsDecoder
    , sessionDecoder, sessionsDecoder
    )

{-| このモジュールはカテゴリー・呼吸法・セッションのCodecを提供します。

このモジュールが目的とすることは、Javascriptとのデータのやり取りを、型安全に行うためです。
ですので、Javascriptに関連する値はこちらに集約されます。


### エンコーダー

@docs encodePosix
@docs encodeCategory, encodeCategories
@docs encodeBreathingMethod, encodeBreathingMethods
@docs encodeSession, encodeSessions


### デコーダー

@docs posixDecoder
@docs categoryDecoder, categoriesDecoder
@docs breathingMethodDecoder, breathingMethodsDecoder
@docs sessionDecoder, sessionsDecoder


### TODO

  - [ ] TODO: Opaque Typesが追加された際、適切なcodecをこちらに配置する

-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as DE
import Json.Encode as E
import Time exposing (Posix)
import Types.BreathingMethod exposing (BreathingMethod, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, Name, fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, fromName, toExhaleDuration, toExhaleHoldDuration, toInhaleDuration, toInhaleHoldDuration, toName)
import Types.Category exposing (Category, Title, fromTitle, toTitle)
import Types.Session exposing (Session)
import Uuid


{-| Posixをエンコードします。

JSではEpochからのミリ秒で表現します。

-}
encodePosix : Posix -> E.Value
encodePosix posix =
    E.int (Time.posixToMillis posix)


{-| カテゴリーをエンコードします。

JSでは以下のように表現されます。:

    { "id": "00000000-0000-0000-0000-000000000000"
    , "title": "カテゴリー名"
    }

-}
encodeCategory : Category -> E.Value
encodeCategory category =
    E.object
        [ ( "id", Uuid.encode category.id )
        , ( "title", E.string <| fromTitle category.title )
        ]


{-| カテゴリーのリストをエンコードします。
-}
encodeCategories : List Category -> E.Value
encodeCategories categories =
    E.list encodeCategory categories


{-| 呼吸法をエンコードします。

JSでは以下のように表現されます。:

    { "id": "00000000-0000-0000-0000-000000000000"
    , "name": "呼吸法名"
    , "category-id": "00000000-0000-0000-0000-000000000000"
    , "created-at": 1610000000000
    , "inhale": 4
    , "inhale-hold": 4
    , "exhale": 4
    , "exhale-hold": 4
    }

-}
encodeBreathingMethod : BreathingMethod -> E.Value
encodeBreathingMethod breathingMethod =
    E.object
        [ ( "id", Uuid.encode breathingMethod.id )
        , ( "name", E.string <| fromName breathingMethod.name )
        , ( "category-id", Uuid.encode breathingMethod.categoryId )
        , ( "created-at", encodePosix breathingMethod.createdAt )
        , ( "inhale", E.int <| fromInhaleDuration breathingMethod.inhaleDuration )
        , ( "inhale-hold", E.int <| fromInhaleHoldDuration breathingMethod.inhaleHoldDuration )
        , ( "exhale", E.int <| fromExhaleDuration breathingMethod.exhaleDuration )
        , ( "exhale-hold", E.int <| fromExhaleHoldDuration breathingMethod.exhaleHoldDuration )
        ]


{-| 呼吸法のリストをエンコードします。
-}
encodeBreathingMethods : List BreathingMethod -> E.Value
encodeBreathingMethods breathingMethods =
    E.list encodeBreathingMethod breathingMethods


{-| セッションをエンコードします。

JSでは以下のように表現されます。:

    { "id": "00000000-0000-0000-0000-000000000000"
    , "inhale": 4
    , "inhale-hold": 4
    , "exhale": 4
    , "exhale-hold": 4
    , "breathing-method-id": "00000000-0000-0000-0000-000000000000"
    , "breathing-method-name": "呼吸法名"
    , "duration": 4
    , "created-at": 1610000000000
    }

-}
encodeSession : Session -> E.Value
encodeSession session =
    E.object
        [ ( "id", Uuid.encode session.id )
        , ( "inhale", E.int <| fromInhaleDuration session.inhaleDuration )
        , ( "inhale-hold", E.int <| fromInhaleHoldDuration session.inhaleHoldDuration )
        , ( "exhale", E.int <| fromExhaleDuration session.exhaleDuration )
        , ( "exhale-hold", E.int <| fromExhaleHoldDuration session.exhaleHoldDuration )
        , ( "breathing-method-id", Uuid.encode session.breathingMethodId )
        , ( "breathing-method-name", E.string <| fromName session.breathingMethodName )
        , ( "duration", E.int session.duration )
        , ( "created-at", encodePosix session.createdAt )
        ]


{-| セッションのリストをエンコードします。
-}
encodeSessions : List Session -> E.Value
encodeSessions sessions =
    E.list encodeSession sessions


{-| Posixをデコードし、Elmの型へ変換します。

JSでの表現は、Epochからのミリ秒です。

-}
posixDecoder : Decoder Posix
posixDecoder =
    D.int
        |> D.map Time.millisToPosix


{-| タイトルをデコードし、Elmの型へ変換します。
-}
titleDecoder : Decoder Title
titleDecoder =
    D.string
        |> D.andThen
            (\s ->
                case toTitle s of
                    Just t ->
                        D.succeed t

                    Nothing ->
                        D.fail ("Invalid title: " ++ s)
            )


{-| カテゴリーをデコードし、Elmの型へ変換します。

JSでの表現は以下のようになります。:

    { "id": "00000000-0000-0000-0000-000000000000"
    , "title": "カテゴリー名"
    }

-}
categoryDecoder : Decoder Category
categoryDecoder =
    D.map2 Category
        (D.field "id" Uuid.decoder)
        (D.field "title" titleDecoder)


{-| カテゴリーのリストをデコードし、Elmの型へ変換します。
-}
categoriesDecoder : Decoder (List Category)
categoriesDecoder =
    D.list categoryDecoder


{-| 呼吸法の名前をデコードし、Elmの型へ変換します。
-}
nameDecoder : Decoder Name
nameDecoder =
    D.string
        |> D.andThen
            (\s ->
                case toName s of
                    Just n ->
                        D.succeed n

                    Nothing ->
                        D.fail ("Invalid name: " ++ s)
            )


{-| 呼吸法の吸気時間をデコードし、Elmの型へ変換します。
-}
inhaleDecoder : Decoder InhaleDuration
inhaleDecoder =
    D.int
        |> D.andThen
            (\i ->
                case toInhaleDuration i of
                    Just id ->
                        D.succeed id

                    Nothing ->
                        D.fail ("Invalid inhale duration: " ++ String.fromInt i)
            )


{-| 呼吸法の吸気保持時間をデコードし、Elmの型へ変換します。
-}
inhaleHoldDecoder : Decoder InhaleHoldDuration
inhaleHoldDecoder =
    D.int
        |> D.andThen
            (\i ->
                case toInhaleHoldDuration i of
                    Just ihd ->
                        D.succeed ihd

                    Nothing ->
                        D.fail ("Invalid inhale hold duration: " ++ String.fromInt i)
            )


{-| 呼吸法の呼気時間をデコードし、Elmの型へ変換します。
-}
exhaleDecoder : Decoder ExhaleDuration
exhaleDecoder =
    D.int
        |> D.andThen
            (\i ->
                case toExhaleDuration i of
                    Just ed ->
                        D.succeed ed

                    Nothing ->
                        D.fail ("Invalid exhale duration: " ++ String.fromInt i)
            )


{-| 呼吸法の呼気保持時間をデコードし、Elmの型へ変換します。
-}
exhaleHoldDecoder : Decoder ExhaleHoldDuration
exhaleHoldDecoder =
    D.int
        |> D.andThen
            (\i ->
                case toExhaleHoldDuration i of
                    Just ehd ->
                        D.succeed ehd

                    Nothing ->
                        D.fail ("Invalid exhale hold duration: " ++ String.fromInt i)
            )


{-| 呼吸法をデコードし、Elmの型へ変換します。

JSでの表現は以下のようになります。:

    { "id": "00000000-0000-0000-0000-000000000000"
    , "name": "呼吸法名"
    , "category-id": "00000000-0000-0000-0000-000000000000"
    , "created-at": 1610000000000
    , "inhale": 4
    , "inhale-hold": 4
    , "exhale": 4
    , "exhale-hold": 4
    }

-}
breathingMethodDecoder : Decoder BreathingMethod
breathingMethodDecoder =
    D.map8 BreathingMethod
        (D.field "id" Uuid.decoder)
        (D.field "name" nameDecoder)
        (D.field "category-id" Uuid.decoder)
        (D.field "created-at" posixDecoder)
        (D.field "inhale" inhaleDecoder)
        (D.field "inhale-hold" inhaleHoldDecoder)
        (D.field "exhale" exhaleDecoder)
        (D.field "exhale-hold" exhaleHoldDecoder)


{-| 呼吸法のリストをデコードし、Elmの型へ変換します。
-}
breathingMethodsDecoder : Decoder (List BreathingMethod)
breathingMethodsDecoder =
    D.list breathingMethodDecoder


{-| セッションをデコードし、Elmの型へ変換します。

JSでの表現は以下のようになります。:

    { "id": "00000000-0000-0000-0000-000000000000"
    , "inhale": 4
    , "inhale-hold": 4
    , "exhale": 4
    , "exhale-hold": 4
    , "breathing-method-id": "00000000-0000-0000-0000-000000000000"
    , "breathing-method-name": "呼吸法名"
    , "duration": 4
    , "created-at": 1610000000000
    }

-}
sessionDecoder : Decoder Session
sessionDecoder =
    D.succeed Session
        |> DE.andMap (D.field "id" Uuid.decoder)
        |> DE.andMap (D.field "inhale" inhaleDecoder)
        |> DE.andMap (D.field "inhale-hold" inhaleHoldDecoder)
        |> DE.andMap (D.field "exhale" exhaleDecoder)
        |> DE.andMap (D.field "exhale-hold" exhaleHoldDecoder)
        |> DE.andMap (D.field "breathing-method-id" Uuid.decoder)
        |> DE.andMap (D.field "breathing-method-name" nameDecoder)
        |> DE.andMap (D.field "duration" D.int)
        |> DE.andMap (D.field "created-at" posixDecoder)


{-| セッションのリストをデコードし、Elmの型へ変換します。
-}
sessionsDecoder : Decoder (List Session)
sessionsDecoder =
    D.list sessionDecoder

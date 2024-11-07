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
import Types.BreathingMethod exposing (BreathingMethod)
import Types.Category exposing (Category)
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
        , ( "title", E.string category.title )
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
        , ( "name", E.string breathingMethod.name )
        , ( "category-id", Uuid.encode breathingMethod.categoryId )
        , ( "created-at", encodePosix breathingMethod.createdAt )
        , ( "inhale", E.int breathingMethod.inhaleDuration )
        , ( "inhale-hold", E.int breathingMethod.inhaleHoldDuration )
        , ( "exhale", E.int breathingMethod.exhaleDuration )
        , ( "exhale-hold", E.int breathingMethod.exhaleHoldDuration )
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
        , ( "inhale", E.int session.inhaleDuration )
        , ( "inhale-hold", E.int session.inhaleHoldDuration )
        , ( "exhale", E.int session.exhaleDuration )
        , ( "exhale-hold", E.int session.exhaleHoldDuration )
        , ( "breathing-method-id", Uuid.encode session.breathingMethodId )
        , ( "breathing-method-name", E.string session.breathingMethodName )
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
        (D.field "title" D.string)


{-| カテゴリーのリストをデコードし、Elmの型へ変換します。
-}
categoriesDecoder : Decoder (List Category)
categoriesDecoder =
    D.list categoryDecoder


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
        (D.field "name" D.string)
        (D.field "category-id" Uuid.decoder)
        (D.field "created-at" posixDecoder)
        (D.field "inhale" D.int)
        (D.field "inhale-hold" D.int)
        (D.field "exhale" D.int)
        (D.field "exhale-hold" D.int)


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
        |> DE.andMap (D.field "inhale" D.int)
        |> DE.andMap (D.field "inhale-hold" D.int)
        |> DE.andMap (D.field "exhale" D.int)
        |> DE.andMap (D.field "exhale-hold" D.int)
        |> DE.andMap (D.field "breathing-method-id" Uuid.decoder)
        |> DE.andMap (D.field "breathing-method-name" D.string)
        |> DE.andMap (D.field "duration" D.int)
        |> DE.andMap (D.field "created-at" posixDecoder)


{-| セッションのリストをデコードし、Elmの型へ変換します。
-}
sessionsDecoder : Decoder (List Session)
sessionsDecoder =
    D.list sessionDecoder

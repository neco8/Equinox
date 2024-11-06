module Uuid exposing
    ( Uuid, Generator, Effect, Msg
    , initialModel
    , update
    , subscriptions
    , uuidGenerate, effectToCmd
    , encode, decoder
    , toString, fromString
    )

{-| UUIDの生成とパースを提供します。


### 型

@docs Uuid, Generator, Effect, Msg


### Model

@docs initialModel


### Update

@docs update


### Subscriptions

@docs subscriptions


### Cmd

@docs uuidGenerate, effectToCmd


### Codec

@docs encode, decoder


### Helper関数

@docs toString, fromString

-}

import Char exposing (isHexDigit)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Parser exposing ((|.), (|=), Parser, Step(..), chompIf, getChompedString, loop, succeed, symbol)
import Time exposing (Month(..))
import Url exposing (fromString)


{-| UUID。内部表現は文字列。
-}
type Uuid
    = Uuid String


{-| UUIDの生成器。UUIDの生成リクエストを管理する。
-}
type alias Generator msg =
    { pendingCount : Int
    , onGenerate : Uuid -> msg
    }


{-| UUIDの生成の結果外部に送出されるEffect。
-}
type Effect
    = GenerateUuid
    | NoEffect


{-| EffectからCmdに変換する関数。
-}
effectToCmd : Cmd msg -> Effect -> Cmd msg
effectToCmd generateUuidValue effect =
    case effect of
        GenerateUuid ->
            generateUuidValue

        NoEffect ->
            Cmd.none


{-| UUID生成の内部で使われるメッセージ。
-}
type Msg msg
    = GotUuid String
    | RequestUuid (Uuid -> msg)


{-| 初期値となるモデル。
-}
initialModel : Generator msg
initialModel =
    let
        -- 不動点を使って再帰的に初期化
        infiniteRequest : Uuid -> msg
        infiniteRequest _ =
            let
                fixpoint : Msg msg
                fixpoint =
                    RequestUuid infiniteRequest
            in
            -- 型システムを満足させるためのダミー
            -- 実際には使われない
            unsafeDummyMsg fixpoint

        -- 型システムを満足させるためのダミー
        -- 実際には使われないことが保証されている
        unsafeDummyMsg : Msg msg -> msg
        unsafeDummyMsg _ =
            infiniteRequest (Uuid "dummy")
    in
    { pendingCount = 0
    , onGenerate = infiniteRequest
    }


{-| UUID Generatorを更新する関数。
-}
update : Msg msg -> Generator msg -> ( Generator msg, Effect, Maybe msg )
update msg model =
    case msg of
        RequestUuid callback ->
            ( { model
                | pendingCount = model.pendingCount + 1
                , onGenerate = callback
              }
            , GenerateUuid
            , Nothing
            )

        GotUuid uuidString ->
            case fromString uuidString of
                Just uuid ->
                    ( { model
                        | pendingCount = model.pendingCount - 1
                      }
                    , NoEffect
                    , Just (model.onGenerate uuid)
                    )

                Nothing ->
                    ( model, NoEffect, Nothing )


{-| UUID生成の結果を受け取るためのSubscriptions。
-}
subscriptions : ((String -> msg) -> Sub msg) -> (Msg msg -> msg) -> Sub msg
subscriptions subscribeToUuid toMsg =
    subscribeToUuid (toMsg << GotUuid)


{-| UUID生成のリクエストのMsgを生成する関数。
-}
uuidGenerate : (Uuid -> msg) -> Msg msg
uuidGenerate callback =
    RequestUuid callback


{-| UUIDの文字列表現を取得する。
-}
toString : Uuid -> String
toString (Uuid str) =
    str


{-| 16進数の文字列のパーサー。
-}
hex : Int -> Parser String
hex n =
    let
        hexDigitHelper : Int -> Int -> Parser (Step Int ())
        hexDigitHelper targetLength currentLength =
            if currentLength >= targetLength then
                succeed (Done ())

            else
                succeed (Loop (currentLength + 1))
                    |. chompIf isHexDigit
    in
    succeed identity
        |. loop 0 (hexDigitHelper n)
        |> getChompedString


{-| UUIDパーサー。
-}
uuidParser : Parser Uuid
uuidParser =
    succeed (\a b c d e -> Uuid (String.join "-" [ a, b, c, d, e ]))
        |= hex 8
        |. symbol "-"
        |= hex 4
        |. symbol "-"
        |= hex 4
        |. symbol "-"
        |= hex 4
        |. symbol "-"
        |= hex 12


{-| UUIDを文字列からパースし取得する。
-}
fromString : String -> Maybe Uuid
fromString str =
    Result.toMaybe (Parser.run uuidParser str)


{-| UUIDのエンコーダー。
-}
encode : Uuid -> E.Value
encode (Uuid str) =
    E.string str


{-| UUIDのデコーダー。
-}
decoder : Decoder Uuid
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just uuid ->
                        D.succeed uuid

                    Nothing ->
                        D.fail "Invalid UUID format"
            )

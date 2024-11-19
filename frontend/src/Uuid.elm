module Uuid exposing
    ( Uuid, fromString, toString
    , Registry, initialRegistry, Tag
    , Effect, effectToCmd, uuidGenerate
    , decoder, encode
    , Msg
    , update
    , subscriptions
    , map
    )

{-|


## UUID

このモジュールは、UUIDを生成するためのモジュールです。
中央集権的にMainコンポーネント下にUUID生成後のコールバックレジストリを保管しておく。

そして、UUID生成のときにコールバック識別のためのタグを渡しておき、それに見合ったコールバックを、UUID返却時に利用する。


### 使い方

説明が必要な使い方の特徴は、中央集権的に管理しているregistryを更新し、再度中央のregistryを新しいものへと更新することである。


### 型

@docs Uuid, fromString, toString
@docs Registry, initialRegistry, Tag


### Cmd

@docs Effect, effectToCmd, uuidGenerate


### Codec

@docs decoder, encode


### Msg

@docs Msg


### update

@docs update


### subscriptions

@docs subscriptions

-}

import Char exposing (isHexDigit)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Parser exposing ((|.), (|=), Parser, Step(..), chompIf, getChompedString, loop, succeed, symbol)


{-| Uuidの型

Opaque Typesとなっているため、fromStringでのみ生成可能

-}
type Uuid
    = Uuid String


{-| UUID生成のハンドラーを管理するレジストリ
-}
type alias Registry msg =
    { handlers : Dict String (Uuid -> msg) -- tag -> handler
    }


{-| UUID生成レジストリの中身を修正する
-}
map : (a -> b) -> Registry a -> Registry b
map f registry =
    { handlers = Dict.map (always ((<<) f)) registry.handlers
    }


{-| いずれCmdへと変換される効果
-}
type Effect
    = GenerateUuid String
    | NoEffect


{-| UUID生成後のコールバックを識別するために必要なタグ文字列

エイリアスにて作成

-}
type alias Tag =
    String


{-| メッセージ

UUIDを生成するために出力するMsgと、UUIDを生成した後に受け取るためのMsgが存在する

-}
type Msg msg
    = GotUuid Tag String
    | RequestUuid Tag (Uuid -> msg)


{-| 初期レジストリを生成
-}
initialRegistry : Registry msg
initialRegistry =
    { handlers = Dict.empty
    }


{-| レジストリの更新

(Msgによってコールバックの数が増減したレジストリ, Cmdを生成するためのEffect, TEA内に送信するべきメッセージ (あったら))

TEA内に送信するべきメッセージというのが、すなわちレジストリに登録されているコールバックのことである。例えば、「UUIDを生成した後にオブジェクトを保存する」などのMsgが保管されており、それを実行してもらうためにMsgを返却する

-}
update : Msg msg -> Registry msg -> ( Registry msg, Effect, Maybe msg )
update msg registry =
    case msg of
        RequestUuid tag callback ->
            ( { registry
                | handlers = Dict.insert tag callback registry.handlers
              }
            , GenerateUuid tag
            , Nothing
            )

        GotUuid tag uuidString ->
            case ( fromString uuidString, Dict.get tag registry.handlers ) of
                ( Just uuid, Just handler ) ->
                    ( { registry
                        | handlers = Dict.remove tag registry.handlers
                      }
                    , NoEffect
                    , Just (handler uuid)
                    )

                _ ->
                    ( registry, NoEffect, Nothing )


{-| UUID生成リクエスト
-}
uuidGenerate : Tag -> (Uuid -> msg) -> Msg msg
uuidGenerate tag callback =
    RequestUuid tag callback


{-| Subscription
-}
subscriptions : ((( String, String ) -> msg) -> Sub msg) -> (Msg msg -> msg) -> Sub msg
subscriptions subscribeToUuid toMsg =
    subscribeToUuid
        (\( tag, uuid ) ->
            toMsg (GotUuid tag uuid)
        )


{-| Effect を Cmd に変換するための関数

実際にはPortsを呼び出している。

-}
effectToCmd : (String -> Cmd msg) -> Effect -> Cmd msg
effectToCmd generateUuidValue effect =
    case effect of
        GenerateUuid tag ->
            generateUuidValue tag

        NoEffect ->
            Cmd.none


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

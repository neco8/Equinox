module Config exposing (Config, Environment, config, defaultEnvironment, environmentDecoder)

{-|


## Config

このモジュールは静的な設定についてを扱うモジュールです。

Settingsと明確に違うことを注意したい。Configは、コンパイル時に変更できる設定値です。例えば、開発環境かテスト環境かで変更したいようなものをこちらへ保存しておくことができます。

-}

import Json.Decode


{-| 環境の型

環境は以下の3つの値を取ります。

    type Environment
        = Production
        | Development
        | Test

-}
type Environment
    = Production
    | Development
    | Test


{-| デフォルトの環境

デフォルトの環境はProductionとしておく。なぜなら、パースに失敗した場合にProductionとして扱い、重要な情報を取得できないようにするためである。

-}
defaultEnvironment : Environment
defaultEnvironment =
    Production


{-| 環境をデコードしElmの型に変換する
-}
environmentDecoder : Json.Decode.Decoder Environment
environmentDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "production" ->
                        Json.Decode.succeed Production

                    "development" ->
                        Json.Decode.succeed Development

                    "test" ->
                        Json.Decode.succeed Test

                    _ ->
                        Json.Decode.fail "Invalid environment"
            )


{-| 設定の型
-}
type alias Config =
    { environment : Environment
    , apiEndPoint : String
    }


{-| 環境によって設定値を出し分ける
-}
config : Environment -> Config
config environment =
    case environment of
        Development ->
            { environment = environment
            , apiEndPoint = "http://localhost:3001/api/v1"
            }

        Production ->
            { environment = environment
            , apiEndPoint = "未定"
            }

        Test ->
            { environment = environment
            , apiEndPoint = "未定"
            }

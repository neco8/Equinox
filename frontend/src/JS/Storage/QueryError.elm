module JS.Storage.QueryError exposing (..)

{-|


## Query Error

このモジュールは、ストレージ操作中に発生しうるエラーを表現するための型とデコードするための関数を定義します。

このモジュールは、特にJSONを介したストレージアクセスで、エラーを適切にハンドリングしやすくすることを目的としています。


### 型

@docs QueryError


### デコーダー

@docs queryErrorDecoder

-}

import Json.Decode as D


{-| `QueryError`は、ストレージ操作中に発生する可能性があるエラーを表す型です。

    type QueryError
        = NotFound
        | DecodingError String
        | QueryError String
        | UnknownError String

-}
type QueryError
    = NotFound
    | DecodingError String
    | QueryError String
    | UnknownError String


{-| `queryErrorDecoder` は、JavaScriptからのJSONエラー情報を `QueryError` 型にデコードするためのデコーダーです。
JSONオブジェクトの `"type"` の値に基づいて対応する `QueryError` を生成します。

    - "NotFound": { "type": "NotFound" } というオブジェクトを `NotFound` エラーとしてデコードします。
    - "DecodingError": { "type": "DecodingError", "message": "エラーメッセージ" } というオブジェクトを `DecodingError` エラーとしてデコードします。
    - "QueryError": { "type": "QueryError", "message": "エラーメッセージ" } というオブジェクトを `QueryError` エラーとしてデコードします。
    - "UnknownError": { "type": "UnknownError", "message": "エラーメッセージ" } というオブジェクトを `UnknownError` エラーとしてデコードします。
    - 不明な値が `"type"` フィールドに含まれている場合、`UnknownError`として `"Unknown error"` のメッセージを付加してデコードされます。

-}
queryErrorDecoder : D.Decoder QueryError
queryErrorDecoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "NotFound" ->
                        D.succeed NotFound

                    "DecodingError" ->
                        D.field "message" D.string
                            |> D.map DecodingError

                    "QueryError" ->
                        D.field "message" D.string
                            |> D.map QueryError

                    "UnknownError" ->
                        D.field "message" D.string
                            |> D.map UnknownError

                    _ ->
                        D.succeed (UnknownError "Unknown error")
            )

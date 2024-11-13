module JS.Storage.QueryResult exposing
    ( Entity(..), QueryResult(..)
    , decodeQueryResult
    )

{-|


## Query Result

このモジュールは、クエリ結果のデータ構造とデコード機能を提供します。

クエリの結果として返されるデータを各エンティティ（カテゴリ、呼吸法、セッション）に変換するための型とデコーダーが含まれています。


### 型

@docs Entity, QueryResult


### デコーダー

@docs decodeQueryResult, decodeEntitySingleResult

-}

import JS.Codec as Codec
import Json.Decode as D exposing (Decoder)
import Types.BreathingMethod exposing (BreathingMethod)
import Types.Category exposing (Category)
import Types.Session exposing (Session)


{-| `QueryResult` 型は、クエリ結果を表現するための型です。
各エンティティの単一結果、もしくはカテゴリ、呼吸法、セッションのリストを含む結果を返します。

@deprecated

-}
type QueryResult
    = EntitySingleResult Entity
    | CategoryListResult (List Category)
    | BreathingMethodListResult (List BreathingMethod)
    | SessionListResult (List Session)


{-| `Entity` 型は、各エンティティ（カテゴリ、呼吸法、セッション）を表現するための型です。
`QueryResult` の単一エンティティ結果に含まれます。

@deprecated

-}
type Entity
    = CategoryEntity Category
    | BreathingMethodEntity BreathingMethod
    | SessionEntity Session



-- DECODERS


{-| `decodeQueryResult` は、JSON オブジェクトを `QueryResult` 型にデコードします。
JSON の `type` フィールドに基づき、クエリ結果の種類を識別します。

@deprecated


### 使用例

デコードする JSON 構造体は以下の形式です：
{ "type": "category-list", "data": [ ... ] }
{ "type": "breathing-method-list", "data": [ ... ] }
{ "type": "session-list", "data": [ ... ] }
{ "type": "entity-single", "entity": "category", "data": { ... } }
{ "type": "entity-single", "entity": "breathing-method", "data": { ... } }
{ "type": "entity-single", "entity": "session", "data": { ... } }

-}
decodeQueryResult : Decoder QueryResult
decodeQueryResult =
    D.field "type" D.string
        |> D.andThen
            (\resultType ->
                case resultType of
                    "entity-single" ->
                        decodeEntitySingleResult

                    "category-list" ->
                        D.field "data" (D.list Codec.categoryDecoder)
                            |> D.map CategoryListResult

                    "breathing-method-list" ->
                        D.field "data" (D.list Codec.breathingMethodDecoder)
                            |> D.map BreathingMethodListResult

                    "session-list" ->
                        D.field "data" (D.list Codec.sessionDecoder)
                            |> D.map SessionListResult

                    _ ->
                        D.fail ("Unknown result type: " ++ resultType)
            )


{-| `decodeEntitySingleResult` は、JSON オブジェクトを単一エンティティの `QueryResult` 型にデコードします。
`entity` フィールドの値に基づき、どのエンティティのデコーダーを使用するかを決定します。

@deprecated


### 使用例

デコードする JSON 構造体は以下の形式です：
{ "entity": "category", "data": { ... } }
{ "entity": "breathing-method", "data": { ... } }
{ "entity": "session", "data": { ... } }

-}
decodeEntitySingleResult : Decoder QueryResult
decodeEntitySingleResult =
    D.field "entity" D.string
        |> D.andThen
            (\entityType ->
                D.field "data"
                    (case entityType of
                        "category" ->
                            D.map (CategoryEntity >> EntitySingleResult) Codec.categoryDecoder

                        "breathing-method" ->
                            D.map (BreathingMethodEntity >> EntitySingleResult) Codec.breathingMethodDecoder

                        "session" ->
                            D.map (SessionEntity >> EntitySingleResult) Codec.sessionDecoder

                        _ ->
                            D.fail ("Unknown entity type: " ++ entityType)
                    )
            )

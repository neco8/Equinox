port module JS.Ports exposing
    ( loadQuery
    , saveBreathingMethodValue
    , saveCategoryValue
    , saveSessionValue
    , subscribeToQueryResults, subscribeToQueryErrors
    , subscribeToUuid
    , deleteBreathingMethodValue
    , generateUuidValue, subscribeToDeleteBreathingMethodResult
    )

{-|


## Ports

このモジュールはJSへのポートをまとめて定義するモジュールです。portとなっている関数は、Elmの世界にはexposeしない。


### Cmd


#### クエリを送信する

@docs queryStorage, loadQuery


#### 保存する

@docs saveBreathingMethod, saveBreathingMethodValue
@docs saveCategory, saveCategoryValue
@docs saveSession, saveSessionValue


#### UUID

@docs generateUuid


### Sub


#### 送信したクエリの結果を受け取る

@docs receiveQueryResult, receiveQueryError, subscribeToQueryResults, subscribeToQueryErrors


#### UUID

@docs receiveUuid, subscribeToUuid


#### 削除

@docs deleteBreathingMethodValue, subscribeDeleteResult

-}

import JS.Codec exposing (encodeBreathingMethod, encodeCategory, encodeSession)
import JS.Storage.QueryError as QueryError exposing (QueryError(..))
import JS.Storage.QueryResult as QueryResult exposing (QueryResult)
import JS.Storage.StorageQueryDSL as Query exposing (Query)
import Json.Decode as D
import Json.Encode as E
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId)
import Types.Category exposing (Category)
import Types.Session exposing (Session)
import Uuid


{-| JS.Storage.StorageQueryDSL.Queryによるクエリをストレージ層に送信します。

クエリの結果は`receiveQueryResult`ポートを通じて受け取ります。

StorageQueryDSLは、リファクタリングされて将来的に使われなくなる可能性があります。

  - [ ] TODO: StorageQueryDSLをリファクタリングする

@deprecated

-}
port queryStorage : E.Value -> Cmd msg


{-| StorageQueryDSLを利用してcmdを生成するラッパーです。
-}
loadQuery : Query -> Cmd msg
loadQuery query =
    queryStorage (Query.encode query)


{-| queryStorageの結果を受け取るためのポートです。
-}
port receiveQueryResult : (D.Value -> msg) -> Sub msg


{-| queryStorageの結果、取得しきれなかったクエリエラーを受け取るためのポートです。

  - [ ] TODO: クエリエラーのみ受け取るというポートは設計がまずいと思うので、リファクタリングする

@deprecated

-}
port receiveQueryError : (D.Value -> msg) -> Sub msg


{-| クエリ結果を受け取るためのSubを生成するラッパーです。
-}
subscribeToQueryResults : (Result QueryError QueryResult -> msg) -> Sub msg
subscribeToQueryResults toMsg =
    receiveQueryResult
        (\value ->
            case D.decodeValue QueryResult.decodeQueryResult value of
                Ok result ->
                    toMsg (Ok result)

                Err error ->
                    toMsg (Err (DecodingError (D.errorToString error)))
        )


{-| クエリエラーを受け取るためのSubを生成するラッパーです。
-}
subscribeToQueryErrors : (QueryError -> msg) -> Sub msg
subscribeToQueryErrors toMsg =
    receiveQueryError
        (\value ->
            case D.decodeValue QueryError.queryErrorDecoder value of
                Ok error ->
                    toMsg error

                Err error ->
                    toMsg (DecodingError (D.errorToString error))
        )


{-| 呼吸法を保存します。
-}
port saveBreathingMethod : E.Value -> Cmd msg


{-| 呼吸法を保存するCmdを生成するラッパーです。
-}
saveBreathingMethodValue : BreathingMethod -> Cmd msg
saveBreathingMethodValue breathingMethod =
    saveBreathingMethod (encodeBreathingMethod breathingMethod)


{-| カテゴリを保存します。
-}
port saveCategory : E.Value -> Cmd msg


{-| カテゴリを保存するCmdを生成するラッパーです。
-}
saveCategoryValue : Category -> Cmd msg
saveCategoryValue category =
    saveCategory (encodeCategory category)


{-| セッションを保存します。
-}
port saveSession : E.Value -> Cmd msg


{-| セッションを保存するCmdを生成するラッパーです。
-}
saveSessionValue : Session -> Cmd msg
saveSessionValue session =
    saveSession (encodeSession session)


{-| UUID生成のリクエストを送信します。
-}
port generateUuid : String -> Cmd msg


{-| UUID生成のリクエストを送信するCmdを生成するラッパーです。

UUIDを生成した後に、どちらに登録されているコールバックを実行すればいいだけである。

-}
generateUuidValue : String -> Cmd msg
generateUuidValue tag =
    generateUuid tag


{-| UUID生成の結果を受け取るポートです。
-}
port receiveUuid : (( String, String ) -> msg) -> Sub msg


{-| UUID生成の結果を受け取るSubを生成するラッパーです。
-}
subscribeToUuid : (( String, String ) -> msg) -> Sub msg
subscribeToUuid toMsg =
    receiveUuid toMsg


{-| 呼吸法を削除します。
-}
port deleteBreathingMethod : E.Value -> Cmd msg


{-| 呼吸法を削除するラッパーです。
-}
deleteBreathingMethodValue : BreathingMethodId -> Cmd msg
deleteBreathingMethodValue breathingMethodId =
    deleteBreathingMethod (Uuid.encode breathingMethodId)


{-| 呼吸法を削除した結果を受け取るポートです。
-}
port receiveDeleteBreathingMethodResult : (Bool -> msg) -> Sub msg


subscribeToDeleteBreathingMethodResult : (Bool -> msg) -> Sub msg
subscribeToDeleteBreathingMethodResult toMsg =
    receiveDeleteBreathingMethodResult toMsg

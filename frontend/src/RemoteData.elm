module RemoteData exposing
    ( RemoteData(..)
    , fromResult
    )

{-|


## RemoteData

このモジュールは、APIによって取得するデータを表現する型を提供します。


### 型

@docs RemoteData


### ヘルパー

@docs fromResult

-}


{-| APIによって取得するデータを表現する型です。
-}
type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


{-| `Result`から`RemoteData`に変換します。
-}
fromResult : Result e a -> RemoteData e a
fromResult result =
    case result of
        Err e ->
            Failure e

        Ok a ->
            Success a

module RemoteData exposing
    ( RemoteData(..)
    , fromResult, map
    )

{-|


## RemoteData

このモジュールは、APIによって取得するデータを表現する型を提供します。


### 型

@docs RemoteData


### ヘルパー

@docs fromResult, map

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


{-| map関数
-}
map : (a -> b) -> RemoteData e a -> RemoteData e b
map f remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure e ->
            Failure e

        Success a ->
            Success (f a)

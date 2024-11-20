module RemoteData exposing
    ( RemoteData(..)
    , fromResult, map, andMap
    )

{-|


## RemoteData

このモジュールは、APIによって取得するデータを表現する型を提供します。


### 型

@docs RemoteData


### ヘルパー

@docs fromResult, map, andMap

-}


{-| APIによって取得するデータを表現する型です。

    type RemoteData e a
        = NotAsked
        | Loading
        | Failure e
        | Success a

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


{-| andMap関数
-}
andMap : RemoteData e a -> RemoteData e (a -> b) -> RemoteData e b
andMap remoteData1 remoteFn =
    case remoteFn of
        Success fn ->
            map fn remoteData1

        Failure e ->
            Failure e

        NotAsked ->
            NotAsked

        Loading ->
            Loading

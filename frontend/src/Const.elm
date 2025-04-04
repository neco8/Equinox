module Const exposing (..)

{-|


## Const

定数を管理するモジュールです。

このモジュールで一括管理することで、更新をする際に編集する必要のある箇所を制限することができます。

主に外部の情報で Flags 等で動的に管理する必要のないものを管理します。

-}


{-| apiのパス
-}
apiPath : String
apiPath =
    "https://equinox-backend.neco8.workers.dev"

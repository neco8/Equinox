module JS.Storage.StorageQueryDSL exposing
    ( Query(..)
    , encode
    )

{-| このモジュールは、ストレージに対する検索クエリをDSL形式で定義・操作するための型と関数を提供します。
各クエリは、条件やフィルターを指定することが可能で、検索範囲の柔軟性を実現します。

ただし、このモジュールを使う際に利用するのはQuery型のみです。


### 型

@docs Query


### 関数

@docs encode


### 使い方

1.  `Query` 型の値を生成します。
2.  `encode` 関数を使って、JSON形式にエンコードします。
3.  JS.Ports を使って、エンコードしたJSONをJavaScriptに渡します。

-}

import Json.Encode as E
import Time
import Uuid exposing (Uuid)


{-| ストレージ内のエンティティを識別するためのキー。

    - `BreathingMethodsKey`: 呼吸法データ
    - `CategoriesKey`: カテゴリーデータ
    - `SessionsKey`: セッションデータ

-}
type StorageKey
    = BreathingMethodsKey
    | CategoriesKey
    | SessionsKey


{-| 与えられた `StorageKey` を文字列キーに変換します。
-}
storageKeyToString : StorageKey -> String
storageKeyToString key =
    case key of
        BreathingMethodsKey ->
            "EQUINOX_breathing-methods"

        CategoriesKey ->
            "EQUINOX_categories"

        SessionsKey ->
            "EQUINOX_sessions"


{-| ストレージからデータを取得するための検索クエリ。

    各クエリは、特定のフィルター、範囲、またはフィールドに基づいてデータを検索します。

-}
type Query
    = GetAllBreathingMethods
    | GetAllCategories
    | GetAllSessions
    | GetBreathingMethodById Uuid
    | GetCategoryById Uuid
    | GetSessionById Uuid
    | GetSessionByTimeRange Time.Posix Time.Posix
    | GetSessionRecentNDays Int Time.Posix
    | GetBreathingMethodByFieldValue String String
    | GetCategoryByFieldValue String String
    | GetSessionByFieldValue String String


{-| DSL形式の検索クエリを表現する型。

    - `storageKey`: 検索対象のエンティティ
    - `conditions`: 検索条件（フィルター）リスト

-}
type alias StorageQueryDSL =
    { storageKey : StorageKey
    , conditions : Maybe (List Condition)
    }


{-| クエリの条件を定義する型。

    - `field`: 検索対象フィールド名
    - `operator`: フィールドに適用する演算子
    - `value`: フィールド値のフィルター

@deprecated: Elmからはこういうふうに無理に抽象的にする必要はないかもしれない。

  - [ ] TODO: この型に変わる方策を考える

-}
type alias Condition =
    { field : String
    , operator : Operator
    , value : Value
    }


{-| 条件に使用する演算子。

    - `Eq`: 等しい
    - `Gt`: より大きい
    - `Lt`: より小さい
    - `Gte`: 以上
    - `Lte`: 以下

@deprecated

  - [ ] TODO: この型に変わる方策を考える

-}
type Operator
    = Eq
    | Gt
    | Lt
    | Gte
    | Lte


{-| クエリ条件で使用する値の型。

    - `StringValue`: 文字列
    - `IntValue`: 整数
    - `BoolValue`: 真偽値
    - `TimestampValue`: タイムスタンプ

@deprecated 微妙な解決策。なぜ微妙かもまだわからない

  - [ ] TODO: この型に変わる方策を考える

-}
type Value
    = StringValue String
    | IntValue Int
    | BoolValue Bool
    | TimestampValue Time.Posix


{-| 指定したクエリから `StorageQueryDSL` を生成します。
-}
toStorageQueryDSL : Query -> StorageQueryDSL
toStorageQueryDSL query =
    case query of
        GetAllBreathingMethods ->
            { storageKey = BreathingMethodsKey
            , conditions = Nothing
            }

        GetAllCategories ->
            { storageKey = CategoriesKey
            , conditions = Nothing
            }

        GetAllSessions ->
            { storageKey = SessionsKey
            , conditions = Nothing
            }

        GetBreathingMethodById uuid ->
            { storageKey = BreathingMethodsKey
            , conditions =
                Just
                    [ { field = "id"
                      , operator = Eq
                      , value = StringValue (Uuid.toString uuid)
                      }
                    ]
            }

        GetCategoryById uuid ->
            { storageKey = CategoriesKey
            , conditions =
                Just
                    [ { field = "id"
                      , operator = Eq
                      , value = StringValue (Uuid.toString uuid)
                      }
                    ]
            }

        GetSessionById uuid ->
            { storageKey = SessionsKey
            , conditions =
                Just
                    [ { field = "id"
                      , operator = Eq
                      , value = StringValue (Uuid.toString uuid)
                      }
                    ]
            }

        GetSessionByTimeRange start end ->
            { storageKey = SessionsKey
            , conditions =
                Just
                    [ { field = "created-at"
                      , operator = Gte
                      , value = TimestampValue start
                      }
                    , { field = "created-at"
                      , operator = Lte
                      , value = TimestampValue end
                      }
                    ]
            }

        GetSessionRecentNDays days now ->
            let
                daysInMillis =
                    days * 24 * 60 * 60 * 1000

                startTime =
                    Time.millisToPosix (Time.posixToMillis now - daysInMillis)
            in
            { storageKey = SessionsKey
            , conditions =
                Just
                    [ { field = "created-at"
                      , operator = Gte
                      , value = TimestampValue startTime
                      }
                    , { field = "created-at"
                      , operator = Lte
                      , value = TimestampValue now
                      }
                    ]
            }

        GetBreathingMethodByFieldValue field value ->
            { storageKey = BreathingMethodsKey
            , conditions =
                Just
                    [ { field = field
                      , operator = Eq
                      , value = StringValue value
                      }
                    ]
            }

        GetCategoryByFieldValue field value ->
            { storageKey = CategoriesKey
            , conditions =
                Just
                    [ { field = field
                      , operator = Eq
                      , value = StringValue value
                      }
                    ]
            }

        GetSessionByFieldValue field value ->
            { storageKey = SessionsKey
            , conditions =
                Just
                    [ { field = field
                      , operator = Eq
                      , value = StringValue value
                      }
                    ]
            }


{-| `Operator` をエンコードして、対応する文字列を取得します。
-}
encodeOperator : Operator -> String
encodeOperator op =
    case op of
        Eq ->
            "eq"

        Gt ->
            "gt"

        Lt ->
            "lt"

        Gte ->
            "gte"

        Lte ->
            "lte"


{-| `Value` 型の値をエンコードして、JSON形式の `E.Value` を生成します。
-}
encodeValue : Value -> E.Value
encodeValue value =
    case value of
        StringValue str ->
            E.string str

        IntValue int ->
            E.int int

        BoolValue bool ->
            E.bool bool

        TimestampValue time ->
            E.int (Time.posixToMillis time)


{-| クエリの条件 `Condition` をJSON形式の `E.Value` にエンコードします。
-}
encodeCondition : Condition -> E.Value
encodeCondition condition =
    E.object
        [ ( "field", E.string condition.field )
        , ( "operator", E.string (encodeOperator condition.operator) )
        , ( "value", encodeValue condition.value )
        ]


{-| `StorageQueryDSL` をJSON形式にエンコードします。
-}
encodeStorageQueryDSL : StorageQueryDSL -> E.Value
encodeStorageQueryDSL dsl =
    case dsl.conditions of
        Nothing ->
            E.object
                [ ( "storageKey", E.string (storageKeyToString dsl.storageKey) )
                ]

        Just conditions ->
            E.object
                [ ( "storageKey", E.string (storageKeyToString dsl.storageKey) )
                , ( "conditions", E.list encodeCondition conditions )
                ]


{-| 与えられた `Query` をJSON形式にエンコードします。
-}
encode : Query -> E.Value
encode query =
    query
        |> toStorageQueryDSL
        |> encodeStorageQueryDSL

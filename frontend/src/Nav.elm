module Nav exposing
    ( StreakModel, Config
    , initialConfig, withSettings, withTitle, withGoBack, withRightTop
    , map
    , view, viewHamburger
    )

{-|


## Nav

このモジュールは、ナビゲーションバーを作成するためのモジュールです。

戻るボタンが付いており、基本的に設定ページなどの一時的なモードに入り、戻る事があるときに使用されるBackNavと、Navが存在します。


### 基本型

@docs StreakModel, Config


### config

@docs initialConfig, withSettings, withTitle, withGoBack, withRightTop


### ヘルパー関数

@docs map


### ビュー

@docs view, viewHamburger

-}

import Html exposing (Html, button, div, h1, nav, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Icon


{-| 設定の型エイリアス
-}
type alias Config msg =
    { goBackMsg : Maybe msg
    , rightTop : List (Html msg)
    , title : Maybe String
    }


{-| 初期設定
-}
initialConfig : Config msg
initialConfig =
    { goBackMsg = Nothing
    , rightTop = []
    , title = Nothing
    }


{-| 設定を追加する
-}
withSettings : msg -> Maybe StreakModel -> Config msg -> Config msg
withSettings goToSettings streakModel config =
    { config | rightTop = viewSettings { goToSettings = goToSettings, streakModel = streakModel } }


{-| タイトルを追加する
-}
withTitle : String -> Config msg -> Config msg
withTitle title config =
    { config | title = Just title }


{-| 戻るボタンを追加する
-}
withGoBack : msg -> Config msg -> Config msg
withGoBack goBack config =
    { config | goBackMsg = Just goBack }


{-| 右上を追加する
-}
withRightTop : List (Html msg) -> Config msg -> Config msg
withRightTop rightTop config =
    { config | rightTop = rightTop }


{-| map関数
-}
map : (a -> b) -> Config a -> Config b
map f config =
    { title = config.title
    , goBackMsg = Maybe.map f config.goBackMsg
    , rightTop = List.map (Html.map f) config.rightTop
    }


{-| ビュー
-}
view : Config msg -> Html msg
view { goBackMsg, title, rightTop } =
    nav [ class "bg-white shadow-sm px-4 py-3" ]
        [ div [ class "max-w-2xl mx-auto items-center flex" ] <|
            List.filterMap identity
                ([ goBackMsg
                    |> Maybe.map
                        (\m ->
                            button
                                [ class "mr-4 p-2 h-10 hover:bg-gray-100 rounded-full aspect-square transition-colors duration-200"
                                , onClick m
                                ]
                                [ Icon.view Icon.ChevronLeft
                                ]
                        )
                 , title
                    |> Maybe.map
                        (\t ->
                            h1 [ class "text-xl font-semibold text-gray-900" ]
                                [ text t ]
                        )
                 ]
                    ++ List.map Just rightTop
                )
        ]


{-| ストリークのモデル
-}
type alias StreakModel =
    Int


{-| ストリークのビュー
-}
viewStreak : StreakModel -> Html msg
viewStreak streak =
    div [ class "flex items-center space-x-2" ]
        [ Icon.view Icon.Flame
        , span
            [ class "font-medium text-xs"
            ]
            [ text <| String.fromInt streak
            ]
        ]


{-| 設定ボタンのビュー
-}
viewSettings : { goToSettings : msg, streakModel : Maybe StreakModel } -> List (Html msg)
viewSettings config =
    [ div [ class "flex justify-end items-center space-x-4 w-full" ] <|
        List.filterMap identity
            [ Maybe.map viewStreak config.streakModel
            , Just <|
                button
                    [ attribute "aria-label" "settings"
                    , class "aspect-square h-10 p-2 hover:bg-gray-200 rounded-full"
                    , onClick config.goToSettings
                    ]
                    [ Icon.view Icon.Settings ]
            ]
    ]


viewHamburger : msg -> Html msg
viewHamburger clickHamburger =
    button
        [ class "aspect-square h-10 p-2 hover:bg-gray-200 rounded-full"
        , onClick clickHamburger
        ]
        [ Icon.view Icon.Hamburger ]

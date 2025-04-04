module Icon exposing (Icon(..), view)

{-|


## Icon

このモジュールはアイコンの表示を提供します。

このモジュールにアイコンを集約することで、アイコンの表示を一元管理できます。アイコンはいずれ自作したものに置き換えることを想定しています。

@docs Icon, view

-}

import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (class)


{-| アイコンの種類を定義します。

    type Icon
        = LocalFireDepartment
        | Settings
        | Add
        | Cottage
        | PlayArrow
        | Monitoring
        | Timer
        | Air
        | Pause
        | Stop
        | MilitaryTech
        | Stars2
        | Edit
        | Public
        | FilterDrama
        | Groups
        | Warning
        | Cached
        | Check
        | ChevronLeft
        | ChevronRight
        | Menu
        | Delete
        | Category
        | CalendarToday
        | Close
        | TravelExplore

-}
type Icon
    = LocalFireDepartment
    | Settings
    | Add
    | Cottage
    | PlayArrow
    | Monitoring
    | Timer
    | Air
    | Pause
    | Stop
    | MilitaryTech
    | Stars2
    | Edit
    | Public
    | FilterDrama
    | Groups
    | Warning
    | Cached
    | Check
    | ChevronLeft
    | ChevronRight
    | Menu
    | Delete
    | Category
    | CalendarToday
    | Close
    | TravelExplore


{-| ビュー
-}
view : { config | icon : Icon } -> List (Attribute msg) -> Html msg
view { icon } attrs =
    span (class "material-symbols-rounded" :: attrs) <|
        List.singleton <|
            text <|
                case icon of
                    LocalFireDepartment ->
                        "local_fire_department"

                    Settings ->
                        "settings"

                    Add ->
                        "add"

                    Cottage ->
                        "cottage"

                    PlayArrow ->
                        "play_arrow"

                    Monitoring ->
                        "monitoring"

                    Timer ->
                        "timer"

                    Air ->
                        "air"

                    Pause ->
                        "pause"

                    Stop ->
                        "stop"

                    MilitaryTech ->
                        "military_tech"

                    Stars2 ->
                        "stars_2"

                    Edit ->
                        "edit"

                    Public ->
                        "public"

                    FilterDrama ->
                        "filter_drama"

                    Groups ->
                        "groups"

                    Warning ->
                        "warning"

                    Cached ->
                        "cached"

                    Check ->
                        "check"

                    ChevronLeft ->
                        "chevron_left"

                    ChevronRight ->
                        "chevron_right"

                    Menu ->
                        "menu"

                    Delete ->
                        "delete"

                    Category ->
                        "category"

                    CalendarToday ->
                        "calendar_today"

                    Close ->
                        "close"

                    TravelExplore ->
                        "travel_explore"

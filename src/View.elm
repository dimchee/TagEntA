module View exposing (..)

import Html exposing (Html)
import Html.Attributes


font_size : String
font_size =
    "1.5em"


marginated : String -> Html msg
marginated text =
    Html.div [ Html.Attributes.style "margin" "10px" ] [ Html.text text ]


tag : String -> Html msg
tag text =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "50%"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        ]
        [ marginated text ]


entity : String -> Html msg
entity text =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        ]
        [ marginated text ]


search : String -> Html msg
search text =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "outline" "2px dashed white"
        , Html.Attributes.style "margin" "10px"
        , Html.Attributes.style "color" "#888"
        ]
        [ marginated <| "🔍 " ++ text ]


container : Html msg -> List (Html msg) -> Html msg
container ent tags =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        ]
        [ ent
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "background" "linear-gradient(#F00, #F00) no-repeat left/2px 100%"
            ]
            tags
        ]


new : Html msg
new =
    Html.div
        [ Html.Attributes.style "background" "linear-gradient(#888, #888) no-repeat left/2px 60%"
        , Html.Attributes.style "padding" "1% 2% 1% 2%"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "margin" "10px"
        , Html.Attributes.style "color" "#888"
        ]
        [ marginated "new..." ]


body : List (Html msg) -> Html msg
body =
    Html.div
        [ Html.Attributes.style "background-color" "#111"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "font-size" font_size
        ]


add : Html msg
add =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ Html.div
            [ Html.Attributes.style "margin" "10px"
            , Html.Attributes.style "font-size" "2em"
            ]
            [ Html.text "+" ]
        , Html.div
            [ Html.Attributes.style "background" "linear-gradient(#888, #888) no-repeat left/2px 60%"
            ]
            [ marginated "add entity..." ]
        ]

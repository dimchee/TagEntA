module View.Components exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import TagEnt exposing (Entity, Tag)
import Types exposing (Msg(..))


font_size : String
font_size =
    "1.5rem"


marginated : String -> Html msg
marginated text =
    Html.div [ Html.Attributes.style "margin" "10px" ] [ Html.text text ]


tag : Tag -> Html Msg
tag text =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "50%"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        , Html.Events.onClick <| SelectedTag text
        ]
        [ marginated text ]


entity : Entity -> Html Msg
entity text =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        , Html.Events.onClick <| SelectedEntity text
        ]
        [ marginated text ]


body : List (Html msg) -> Html msg
body =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "justify-content" "flex-start"
        , Html.Attributes.style "min-height" "100vh"
        , Html.Attributes.style "background-color" "#111"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "font-size" font_size
        , Html.Attributes.style "user-select" "none"
        ]

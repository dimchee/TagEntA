module View exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events


font_size : String
font_size =
    "1.5em"


type alias Entity =
    String


type alias Tag =
    String


type Msg
    = SelectedEntity Entity
    | SelectedTag Tag
    | AddTag Entity
    | AddEntity
    | BackToMain


marginated : String -> Html msg
marginated text =
    Html.div [ Html.Attributes.style "margin" "10px" ] [ Html.text text ]


tag : String -> Html Msg
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


entity : String -> Html Msg
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


search : String -> Html msg
search text =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        ]
        [ Html.div
            [ Html.Attributes.style "width" "60%"
            , Html.Attributes.style "border-radius" "0.5em"
            , Html.Attributes.style "outline" "2px dashed white"
            , Html.Attributes.style "margin" "10px"
            , Html.Attributes.style "color" "#888"
            ]
            [ marginated <| "🔍 " ++ text ]
        ]


container : Entity -> List Tag -> Html Msg
container ent tags =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        ]
        [ entity ent
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "background" "linear-gradient(#F00, #F00) no-repeat left/2px 100%"
            ]
          <|
            List.map tag tags
        , addTag ent
        ]


addTag : Entity -> Html Msg
addTag ent =
    Html.div
        [ Html.Attributes.style "background" "linear-gradient(#888, #888) no-repeat left/2px 60%"
        , Html.Attributes.style "padding" "1%"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "margin" "10px"
        , Html.Attributes.style "color" "#888"
        ]
        [ Html.div [ Html.Events.onClick <| AddTag ent ] [ Html.text "new..." ] ]


body : List (Html msg) -> Html msg
body =
    Html.div [ Html.Attributes.style "margin" "10%" ]
        >> List.singleton
        >> Html.div
            [ Html.Attributes.style "background-color" "#111"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "margin" "0"
            , Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "width" "100vw"
            , Html.Attributes.style "font-size" font_size
            , Html.Attributes.style "user-select" "none"
            ]


addEntity : Html Msg
addEntity =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ Html.div
            [ Html.Attributes.style "margin" "10px"
            , Html.Attributes.style "font-size" "2em"
            , Html.Events.onClick <| AddEntity
            ]
            [ Html.text "+" ]
        , Html.div
            [ Html.Attributes.style "background" "linear-gradient(#888, #888) no-repeat left/2px 60%"
            ]
            [ marginated "add entity..." ]
        ]


backToMain : Html.Html Msg
backToMain =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ Html.div
            [ Html.Attributes.style "margin" "10px"
            , Html.Attributes.style "font-size" "2em"
            , Html.Events.onClick <| BackToMain
            ]
            [ Html.text "⌂" ]
        , Html.div
            [ Html.Attributes.style "background" "linear-gradient(#888, #888) no-repeat left/2px 60%"
            ]
            [ marginated "Home" ]
        ]

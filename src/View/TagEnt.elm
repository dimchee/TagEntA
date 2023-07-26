module View.TagEnt exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import TagEnt exposing (Entity, Tag, TagEnt)
import Types exposing (Msg(..))
import View.Components


backToMain : Html Msg
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
            [ View.Components.marginated "Home" ]
        ]


tag : Tag -> TagEnt -> Html Msg
tag tag_ tagEnt =
    View.Components.body
        [ backToMain
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ View.Components.tag tag_
            , Html.div
                [ Html.Events.onClick <| DeleteTag tag_
                , Html.Attributes.style "font-size" "3rem"
                , Html.Attributes.style "color" "#888"
                ]
                [ Html.text "␡" ]
            ]
        , TagEnt.tagEntities tag_ tagEnt
            |> List.map View.Components.entity
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]


entity : Entity -> TagEnt -> Html Msg
entity entity_ tagEnt =
    View.Components.body
        [ backToMain
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ View.Components.entity entity_
            , Html.div
                [ Html.Events.onClick <| DeleteEntity entity_
                , Html.Attributes.style "font-size" "3rem"
                , Html.Attributes.style "color" "#888"
                ]
                [ Html.text "␡" ]
            ]
        , TagEnt.entityTags entity_ tagEnt
            |> List.map View.Components.tag
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]

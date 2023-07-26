module View.TagEnt exposing (..)

import Html exposing (Html)
import Html.Attributes
import TagEnt exposing (Entity, Tag, TagEnt)
import Types exposing (Msg(..))
import View.Components


tag : Tag -> TagEnt -> Html Msg
tag tag_ tagEnt =
    View.Components.body
        [ View.Components.goToMain
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ View.Components.tag tag_
            , View.Components.symbolButton "␡" <| DeleteTag tag_
            ]
        , TagEnt.tagEntities tag_ tagEnt
            |> List.map View.Components.entity
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]


entity : Entity -> TagEnt -> Html Msg
entity entity_ tagEnt =
    View.Components.body
        [ View.Components.goToMain
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ View.Components.entity entity_
            , View.Components.symbolButton "␡" <| DeleteEntity entity_
            ]
        , TagEnt.entityTags entity_ tagEnt
            |> List.map View.Components.tag
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]

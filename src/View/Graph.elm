module View.Graph exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes exposing (stroke, x1, x2, y1, y2)
import TagEnt exposing (TagEnt)
import Types exposing (Msg)
import View.Components


yStart : Int
yStart =
    110


yStep : Int
yStep =
    68


indexToY : Int -> String
indexToY ind =
    yStart + ind * yStep |> String.fromInt


type alias Index =
    Int


line : Index -> Index -> Svg.Svg msg
line from to =
    Svg.line [ x1 "90", indexToY from |> y1, x2 "310", indexToY to |> y2, stroke "red" ] []


view : TagEnt -> Html Msg
view tagEnt =
    let
        tags =
            TagEnt.tags tagEnt

        getTagInd tag =
            List.indexedMap (\i x -> ( i, x )) tags
                |> List.filterMap
                    (\( i, x ) ->
                        if x == tag then
                            Just i

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault 0
    in
    View.Components.body
        [ View.Components.goToMain
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "justify-content" "space-between"
            , Html.Attributes.style "width" "50%"
            ]
            [ TagEnt.entities tagEnt
                |> List.map View.Components.entity
                |> Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    ]
            , TagEnt.tags tagEnt
                |> List.map View.Components.tag
                |> Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    ]
            ]
        , tagEnt
            |> TagEnt.asTree
            |> List.indexedMap
                (\from ( _, tgs ) ->
                    List.map (\x -> line from <| getTagInd x) tgs
                )
            |> List.concat
            |> Svg.svg
                [ Svg.Attributes.width "100vw"
                , Svg.Attributes.height "100vh"
                , Svg.Attributes.style "margin: 0;"
                , Svg.Attributes.style "padding: 0;"
                , Svg.Attributes.style "position: absolute;"
                , Svg.Attributes.pointerEvents "none"
                ]
        ]

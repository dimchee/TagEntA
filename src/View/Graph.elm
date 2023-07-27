module View.Graph exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes exposing (d, fill, stroke, x1, x2, y1, y2)
import TagEnt exposing (TagEnt)
import Types exposing (..)
import View.Components


getX : Point -> String
getX =
    .x >> String.fromFloat


getY : Point -> String
getY =
    .y >> String.fromFloat


getXY : Point -> String
getXY p =
    getX p ++ " " ++ getY p


line : Point -> Point -> Svg.Svg msg
line from to =
    Svg.line [ getX from |> x1, getY from |> y1, getX to |> x2, getY to |> y2, stroke "red" ] []


changeX : (Float -> Float) -> Point -> Point
changeX f { x, y } =
    { x = f x, y = y }


sub : number -> number -> number
sub x y =
    y - x


difXProc : Point -> Point -> Float -> Float
difXProc from to proc =
    (to.x - from.x) * proc


getPath : Point -> Point -> Float -> String
getPath from to curviness =
    String.join " "
        [ "M"
        , getXY from
        , "C"
        , changeX ((+) <| difXProc from to curviness) from |> getXY
        , ","
        , changeX (sub <| difXProc from to curviness) to |> getXY
        , ","
        , getXY to
        ]


lineSmooth : Point -> Point -> Svg.Svg msg
lineSmooth from to =
    Svg.path
        [ d <| getPath from to 0.5
        , stroke "red"
        , fill "transparent"
        ]
        []


view : TagEnt -> Maybe (Dict Id LR) -> Html Msg
view tagEnt positions =
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
            |> List.concatMap (\( ent, tags ) -> List.map (\tag -> ( ent, tag )) tags)
            |> List.filterMap
                (\( ent, tag ) ->
                    Maybe.andThen
                        (\lrs ->
                            Maybe.map2 Tuple.pair
                                (Dict.get (View.Components.entityToId ent) lrs)
                                (Dict.get (View.Components.tagToId tag) lrs)
                        )
                        positions
                )
            |> List.map (\( { right }, { left } ) -> lineSmooth right left)
            |> Svg.svg
                [ Svg.Attributes.width "100vw"
                , Svg.Attributes.height "100vh"
                , Svg.Attributes.style "margin: 0;"
                , Svg.Attributes.style "padding: 0;"
                , Svg.Attributes.style "position: absolute;"
                , Svg.Attributes.pointerEvents "none"
                ]
        ]

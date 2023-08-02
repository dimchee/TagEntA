module View.Components exposing (..)

import Css
import Css.Global
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Styled exposing (toUnstyled)
import Html.Styled.Attributes
import Html.Styled.Events
import TagEnt exposing (Entity, Tag)
import Types exposing (Msg(..))


font_size : String
font_size =
    "1.5rem"


marginated : String -> Html msg
marginated text =
    Html.div [ Html.Attributes.style "margin" "10px" ] [ Html.text text ]


symbolButton : String -> msg -> Html msg
symbolButton symb msg =
    Html.div
        [ Html.Attributes.style "font-size" "3rem"
        , Html.Attributes.style "margin" "10px"
        , Html.Attributes.style "color" "#888"
        , Html.Events.onClick msg
        ]
        [ Html.text symb ]


entityToId : Entity -> String
entityToId ent_ =
    "entity_" ++ ent_


tagToId : Tag -> String
tagToId tag_ =
    "tag_" ++ tag_


tag : Tag -> Html Msg
tag tag_ =
    toUnstyled <|
        Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.displayFlex
                , Css.borderRadius <| Css.pct 50
                , Css.textAlign Css.center
                , Css.alignItems Css.center
                , Css.outline3 (Css.px 2) Css.solid <| Css.hex "ffffff"
                , Css.margin (Css.px 10)
                , Css.hover
                    [ Css.Global.children
                        [ Css.Global.id "delete_tag_x"
                            [ --Css.visibility Css.visible
                              Css.display Css.block
                            ]
                        ]
                    ]
                ]
            , Html.Styled.Attributes.id <| tagToId tag_
            ]
            [ Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.margin <| Css.px 10
                    ]
                , Html.Styled.Events.onClick <| SelectedTag tag_
                ]
                [ Html.Styled.text tag_ ]
            , Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.margin4 (Css.px 10) (Css.px 10) (Css.px 10) (Css.px 0)

                    -- , Css.visibility Css.hidden
                    , Css.display Css.none
                    ]
                , Html.Styled.Attributes.id "delete_tag_x"
                , Html.Styled.Events.onClick <| DeleteTag tag_
                ]
                [ Html.Styled.text "𝕏"
                ]
            ]


entity : Entity -> Html Msg
entity entity_ =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        , Html.Attributes.id <| entityToId entity_
        , Html.Events.onClick <| SelectedEntity entity_
        ]
        [ marginated entity_ ]


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


goToMain : Html Msg
goToMain =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ symbolButton "⌂" GoToMain
        , marginated "Home"
        ]


goToGraph : Html Msg
goToGraph =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ symbolButton "⤬" GoToGraph
        , marginated "Graph"
        ]

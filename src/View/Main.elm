module View.Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import TagEnt exposing (Entity, Tag, TagEnt)
import Types exposing (..)
import View.Components


type Pending
    = PendingTag Entity String
    | PendingEntity String
    | NoChange


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    Html.Events.on "keydown" (Decode.andThen isEnter Html.Events.keyCode)


add : String -> Msg -> (String -> Msg) -> Html.Html Msg
add val submit onChange =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ Html.div
            [ Html.Attributes.style "font-size" "3rem"
            , Html.Attributes.style "margin" "5px"
            , Html.Events.onClick submit
            ]
            [ Html.text "+" ]
        , Html.input
            [ Html.Events.onInput onChange
            , onEnter submit
            , Html.Attributes.value val
            , Html.Attributes.placeholder "new..."
            , Html.Attributes.style "background-color" "#0000"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "font-size" View.Components.font_size
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "outline" "none"
            , Html.Attributes.style "width" "10rem"
            ]
            []
        ]


newTag : Pending -> Entity -> Html Msg
newTag pending ent =
    case pending of
        PendingTag ent2 tag ->
            if ent == ent2 then
                add tag (AddTag ent tag) (InputTag ent)

            else
                add "" NoAction (InputTag ent)

        _ ->
            add "" NoAction (InputTag ent)


newEntity : Pending -> Html Msg
newEntity pending =
    case pending of
        PendingEntity ent ->
            add ent (AddEntity ent) InputEntity

        _ ->
            add "" NoAction InputEntity


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
            [ View.Components.marginated <| "🔍 " ++ text ]
        ]


container : Pending -> Entity -> List Tag -> Html Msg
container pending ent tags =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "min-width" "40rem"
        , Html.Attributes.style "align-items" "flex-start"
        , Html.Attributes.style "justify-content" "space-between"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        ]
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "flex-start"
            ]
            [ View.Components.entity ent
            , Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "background" "linear-gradient(#F00, #F00) no-repeat left/2px 100%"
                ]
              <|
                List.map View.Components.tag tags
            ]
        , newTag pending ent
        ]


view : TagEnt -> Pending -> Html Msg
view tagEnt pending =
    View.Components.body
        [ search "search..."
        , Html.div [] <| List.map (\( ent, ts ) -> container pending ent ts) <| TagEnt.asTree tagEnt
        , newEntity pending
        ]

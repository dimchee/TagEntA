module View.Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import TagEnt exposing (Entity, Tag, TagEnt)
import Types exposing (..)
import View.Components


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


add : String -> Id -> Msg -> (String -> Msg) -> Html.Html Msg
add val sugestions submit onChange =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        ]
        [ View.Components.symbolButton "+" submit
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
            , Html.Attributes.list sugestions
            ]
            []
        ]


searchHelper : Bool -> String -> Html Msg
searchHelper cSearch query =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "width" "60%"
            , Html.Attributes.style "border-radius" "0.5em"
            , Html.Attributes.style "outline" "2px dashed white"
            , Html.Attributes.style "margin" "10px"
            ]
            [ Html.div
                [ Html.Attributes.style "margin" "10px"
                , Html.Events.onClick <| Search query
                ]
                [ Html.text "🔍" ]
            , Html.input
                [ Html.Events.onInput ChangeQuery
                , onEnter <| Search query
                , Html.Attributes.value query
                , Html.Attributes.placeholder "search..."
                , Html.Attributes.style "background-color" "#0000"
                , Html.Attributes.style "border" "none"
                , Html.Attributes.style "font-size" View.Components.font_size
                , Html.Attributes.style "color" "white"
                , Html.Attributes.style "outline" "none"

                -- , Html.Attributes.type_ "search"
                -- , Html.Attributes.list "search_sugestions"
                ]
                []
            ]
        , Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "width" "20px"
            , Html.Events.onCheck ContinuousSearch
            , Html.Attributes.checked cSearch
            ]
            []
        , Html.label
            []
            [ Html.text "Continuous Search"
            ]
        ]


search : Bool -> Pending -> Html Msg
search cSearch pending =
    case pending of
        PendingSearch query ->
            searchHelper cSearch query

        _ ->
            searchHelper cSearch ""


newTag : Pending -> Entity -> Html Msg
newTag pending ent =
    case pending of
        PendingTag ent2 tag ->
            if ent == ent2 then
                add tag "sugestions_tag" (AddTag ent tag) (InputTag ent)

            else
                add "" "sugestions_tag" NoAction (InputTag ent)

        _ ->
            add "" "sugestions_tag" NoAction (InputTag ent)


newEntity : Pending -> Html Msg
newEntity pending =
    case pending of
        PendingEntity ent ->
            add ent "sugestions_entity" (AddEntity ent) InputEntity

        _ ->
            add "" "sugestions_entity" NoAction InputEntity


container : Pending -> Entity -> List Tag -> Html Msg
container pending ent tags =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "min-width" "40rem"
        , Html.Attributes.style "align-items" "center"
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


viewSugestions : TagEnt -> Html Msg
viewSugestions tagEnt =
    Html.div []
        [ TagEnt.entities tagEnt
            |> List.map (\x -> Html.option [ Html.Attributes.value x ] [])
            |> Html.datalist [ Html.Attributes.id "sugestions_entity" ]
        , TagEnt.tags tagEnt
            |> List.map (\x -> Html.option [ Html.Attributes.value x ] [])
            |> Html.datalist [ Html.Attributes.id "sugestions_tag" ]
        ]


view : TagEnt -> MainArgs -> Html Msg
view tagEnt { continuousSearch, query, pending } =
    View.Components.body
        [ search continuousSearch pending
        , TagEnt.asTree tagEnt
            |> List.filter (\( ent, _ ) -> String.contains (Maybe.withDefault "" query) ent)
            |> List.map (\( ent, ts ) -> container pending ent ts)
            |> Html.div []
        , newEntity pending
        , View.Components.symbolButton "⤬" GoToGraph
        , viewSugestions tagEnt
        ]

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
add val id submit onChange =
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
            , Html.Attributes.id id

            -- , sugestions |> Maybe.withDefault "" |> Html.Attributes.list
            ]
            []
        ]


searchHelper : String -> Html Msg
searchHelper query =
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
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.id "search"

                -- , Html.Attributes.type_ "search"
                -- , Html.Attributes.list "search_sugestions"
                ]
                []
            ]
        ]


search : Query -> Html Msg
search query =
    searchHelper <| Maybe.withDefault "" query


newTag : Id -> Pending -> Entity -> Html Msg
newTag id pending ent =
    case pending of
        PendingTag ent2 tag ->
            if ent == ent2 then
                add tag id (AddTag ent tag) (InputTag ent)

            else
                add "" id NoAction (InputTag ent)

        _ ->
            add "" id NoAction (InputTag ent)


newEntity : Id -> Pending -> Html Msg
newEntity id pending =
    case pending of
        PendingEntity ent ->
            add ent id (AddEntity ent) InputEntity

        _ ->
            add "" id NoAction InputEntity


container : Pending -> Entity -> List Tag -> Html Msg
container pending ent tags =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "min-width" "40rem"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "flex-start"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        , Html.Events.onClick <| Focus <| "new_tag_" ++ ent
        ]
        [ View.Components.entity ent
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "background" "linear-gradient(#F00, #F00) no-repeat left/2px 100%"
            ]
          <|
            List.map View.Components.tag tags
        , newTag ("new_tag_" ++ ent) pending ent
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
view tagEnt { query, pending } =
    View.Components.body
        [ search query
        , TagEnt.asTree tagEnt
            |> List.filter (\( ent, _ ) -> String.contains (Maybe.withDefault "" query) ent)
            |> List.map (\( ent, ts ) -> container pending ent ts)
            |> Html.div []
        , newEntity "new_entity" pending
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "color" "#888"
            ]
            [ View.Components.symbolButton "⤬" GoToGraph, Html.text "Graph" ]
        , viewSugestions tagEnt
        ]

module View exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events



-- TODO add submit on Enter key


font_size : String
font_size =
    "1.5rem"


type alias Rel =
    { tags : Dict Entity (List Tag)
    , ents : Dict Tag (List Entity)
    }


type Pending
    = PendingTag Entity String
    | PendingEntity String
    | NoChange


type alias Entity =
    String


type alias Tag =
    String


type Msg
    = SelectedEntity Entity
    | SelectedTag Tag
    | AddTag Entity Tag
    | AddEntity Entity
    | InputTag Entity String
    | InputEntity String
    | DeleteTag Tag
    | DeleteEntity Entity
    | BackToMain
    | NoAction


marginated : String -> Html msg
marginated text =
    Html.div [ Html.Attributes.style "margin" "10px" ] [ Html.text text ]


viewTag : String -> Html Msg
viewTag text =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "border-radius" "50%"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "outline" "2px solid white"
        , Html.Attributes.style "margin" "10px"
        , Html.Events.onClick <| SelectedTag text
        ]
        [ marginated text ]


viewEntity : String -> Html Msg
viewEntity text =
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


container : Pending -> Entity -> List Tag -> Html Msg
container pending ent tags =
    Html.div
        [ Html.Attributes.style "display" "flex"
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
            [ viewEntity ent
            , Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "background" "linear-gradient(#F00, #F00) no-repeat left/2px 100%"
                ]
              <|
                List.map viewTag tags
            ]
        , newTag pending ent
        ]


add : String -> Msg -> (String -> Msg) -> Html.Html Msg
add val submit onChange =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#888"
        ]
        [ Html.div
            [ Html.Attributes.style "font-size" "3rem"
            , Html.Events.onClick submit
            ]
            [ Html.text "+" ]
        , Html.div
            [ Html.Attributes.style "background" "linear-gradient(#888, #888) no-repeat left/2px 60%"
            , Html.Attributes.style "padding" "5px"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "margin" "10px"
            , Html.Attributes.style "color" "#888"
            ]
            [ Html.input
                [ Html.Events.onInput onChange
                , Html.Events.onSubmit submit
                , Html.Attributes.value val
                , Html.Attributes.placeholder "new..."
                , Html.Attributes.style "background-color" "#0000"
                , Html.Attributes.style "border" "none"
                , Html.Attributes.style "font-size" font_size
                , Html.Attributes.style "color" "white"
                , Html.Attributes.style "outline" "none"
                , Html.Attributes.style "width" "10rem"
                ]
                []
            ]
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


body : List (Html msg) -> Html msg
body =
    Html.div [ Html.Attributes.style "margin" "10%" ]
        >> List.singleton
        >> Html.div
            [ Html.Attributes.style "background-color" "#111"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "margin" "0"
            , Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "min-height" "100vh"
            , Html.Attributes.style "width" "100vw"
            , Html.Attributes.style "font-size" font_size
            , Html.Attributes.style "user-select" "none"
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


viewMainView : Rel -> Pending -> Html.Html Msg
viewMainView { tags } pending =
    body
        [ search "search..."
        , Html.div [] <| List.map (\( ent, ts ) -> container pending ent ts) <| Dict.toList tags
        , newEntity pending
        ]


viewTagView : Tag -> Rel -> Html.Html Msg
viewTagView tag { ents } =
    body
        [ backToMain
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ viewTag tag
            , Html.div
                [ Html.Events.onClick <| DeleteTag tag
                , Html.Attributes.style "font-size" "3rem"
                , Html.Attributes.style "color" "#888"
                ]
                [ Html.text "␡" ]
            ]
        , Dict.get tag ents
            |> Maybe.withDefault []
            |> List.map viewEntity
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]


viewEntityView : Entity -> Rel -> Html.Html Msg
viewEntityView ent { tags } =
    body
        [ backToMain
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ viewEntity ent
            , Html.div
                [ Html.Events.onClick <| DeleteEntity ent
                , Html.Attributes.style "font-size" "3rem"
                , Html.Attributes.style "color" "#888"
                ]
                [ Html.text "␡" ]
            ]
        , Dict.get ent tags
            |> Maybe.withDefault []
            |> List.map viewTag
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]

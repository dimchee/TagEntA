module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import View exposing (Entity, Msg(..), Tag)


type alias Rel =
    { tags : Dict Entity (List Tag)
    , ents : Dict Tag (List Entity)
    }


type View
    = Main
    | Tag Tag
    | Entity Entity -- | Graph


type alias Model =
    { rel : Rel
    , view : View
    }


viewMain : Rel -> Html.Html Msg
viewMain { tags } =
    View.body
        [ View.search "search..."
        , Html.div [] <| List.map (\( ent, ts ) -> View.container ent ts) <| Dict.toList tags
        , View.addEntity
        ]


viewTag : Tag -> Rel -> Html.Html Msg
viewTag tag { ents } =
    View.body
        [ View.backToMain
        , View.tag tag
        , Dict.get tag ents
            |> Maybe.withDefault []
            |> List.map View.entity
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]


viewEntity : Entity -> Rel -> Html.Html Msg
viewEntity ent { tags } =
    View.body
        [ View.backToMain
        , View.entity ent
        , Dict.get ent tags
            |> Maybe.withDefault []
            |> List.map View.tag
            |> Html.div [ Html.Attributes.style "display" "flex" ]
        ]



-- main : Program () Model String


testRel : Rel
testRel =
    { tags =
        Dict.fromList
            [ ( "ent1", [ "tag1", "tag2", "tag3" ] )
            , ( "ent2", [ "tag1", "tag4", "tag2" ] )
            ]
    , ents =
        Dict.fromList
            [ ( "tag1", [ "ent1", "ent2" ] )
            , ( "tag2", [ "ent1", "ent2" ] )
            , ( "tag3", [ "ent1" ] )
            , ( "tag4", [ "ent2" ] )
            ]
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { rel = testRel, view = Main }
        , view =
            \{ rel, view } ->
                case view of
                    Main ->
                        viewMain rel

                    Tag tag ->
                        viewTag tag rel

                    Entity entity ->
                        viewEntity entity rel
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedEntity ent ->
            { model | view = Entity ent }

        SelectedTag tag ->
            { model | view = Tag tag }

        BackToMain ->
            { model | view = Main }

        AddTag ent ->
            model

        AddEntity ->
            model

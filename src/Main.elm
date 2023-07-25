module Main exposing (..)

import Browser
import Dict exposing (Dict)
import View exposing (Entity, Msg(..), Tag)


type View
    = Main View.Pending
    | Tag Tag
    | Entity Entity -- | Graph


type alias Model =
    { rel : View.Rel
    , view : View
    }



-- main : Program () Model String


testRel : View.Rel
testRel =
    { tags =
        Dict.fromList
            [ ( "ent1", [ "tag1", "tag2", "tag3" ] )
            , ( "ent2", [ "tag1", "tag4", "tag2" ] )
            , ( "ent3", [] )
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
        { init = { rel = testRel, view = Main View.NoChange }
        , view =
            \{ rel, view } ->
                case view of
                    Main pending ->
                        View.viewMainView rel pending

                    Tag tag ->
                        View.viewTagView tag rel

                    Entity entity ->
                        View.viewEntityView entity rel
        , update = update
        }


appendTo : ( comparable, val ) -> Dict comparable (List val) -> Dict comparable (List val)
appendTo ( key, val ) =
    Dict.update key (Maybe.withDefault [] >> (::) val >> Just)


addToRel : ( Entity, Tag ) -> View.Rel -> View.Rel
addToRel ( ent, tag ) rel =
    { rel | ents = appendTo ( tag, ent ) rel.ents, tags = appendTo ( ent, tag ) rel.tags }


addEntityToRel : Entity -> View.Rel -> View.Rel
addEntityToRel ent rel =
    { rel | tags = Dict.insert ent [] rel.tags }


removeTag : Tag -> View.Rel -> View.Rel
removeTag tag rel =
    { rel
        | tags = Dict.map (\_ l -> List.filter ((/=) tag) l) rel.tags
        , ents = Dict.update tag (\_ -> Nothing) rel.ents
    }


removeEntity : Entity -> View.Rel -> View.Rel
removeEntity ent rel =
    { rel
        | ents = Dict.map (\_ l -> List.filter ((/=) ent) l) rel.ents
        , tags = Dict.update ent (\_ -> Nothing) rel.tags
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedEntity ent ->
            { model | view = Entity ent }

        SelectedTag tag ->
            { model | view = Tag tag }

        BackToMain ->
            { model | view = Main View.NoChange }

        InputTag ent s ->
            { model | view = Main <| View.PendingTag ent s }

        InputEntity s ->
            { model | view = Main <| View.PendingEntity s }

        AddTag ent tag ->
            { model | view = Main View.NoChange, rel = addToRel ( ent, tag ) model.rel }

        AddEntity ent ->
            { model | view = Main View.NoChange, rel = addEntityToRel ent model.rel }

        DeleteTag tag ->
            { model | view = Main View.NoChange, rel = removeTag tag model.rel }

        DeleteEntity ent ->
            { model | view = Main View.NoChange, rel = removeEntity ent model.rel }

        NoAction ->
            model

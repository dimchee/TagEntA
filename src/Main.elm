module Main exposing (..)

import Browser
import TagEnt exposing (..)
import View exposing (Msg(..))


type View
    = Main View.Pending
    | Tag Tag
    | Entity Entity -- | Graph


type alias Model =
    { rel : TagEnt
    , view : View
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { rel = TagEnt.example, view = Main View.NoChange }
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


validate : String -> String
validate s =
    if String.length s < 10 then
        s

    else
        String.slice 0 -1 s


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
            { model | view = Main <| View.PendingTag ent <| validate s }

        InputEntity s ->
            { model | view = Main <| View.PendingEntity <| validate s }

        AddTag ent tag ->
            { model | view = Main View.NoChange, rel = addEdge ( ent, tag ) model.rel }

        AddEntity ent ->
            { model | view = Main View.NoChange, rel = addEntity ent model.rel }

        DeleteTag tag ->
            { model | view = Main View.NoChange, rel = removeTag tag model.rel }

        DeleteEntity ent ->
            { model | view = Main View.NoChange, rel = removeEntity ent model.rel }

        NoAction ->
            model

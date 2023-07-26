module Main exposing (..)

import Browser
import TagEnt exposing (..)
import Types exposing (Msg(..))
import View.Main
import View.TagEnt


type View
    = Main View.Main.Pending
    | Tag Tag
    | Entity Entity -- | Graph


type alias Model =
    { rel : TagEnt
    , view : View
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { rel = TagEnt.example, view = Main View.Main.NoChange }
        , view =
            \{ rel, view } ->
                case view of
                    Main pending ->
                        View.Main.view rel pending

                    Tag tag ->
                        View.TagEnt.tag tag rel

                    Entity entity ->
                        View.TagEnt.entity entity rel
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
            { model | view = Main View.Main.NoChange }

        InputTag ent s ->
            { model | view = Main <| View.Main.PendingTag ent <| validate s }

        InputEntity s ->
            { model | view = Main <| View.Main.PendingEntity <| validate s }

        AddTag ent tag ->
            { model | rel = addEdge ( ent, tag ) model.rel } |> update BackToMain

        AddEntity ent ->
            { model | rel = addEntity ent model.rel } |> update BackToMain

        DeleteTag tag ->
            { model | rel = removeTag tag model.rel } |> update BackToMain

        DeleteEntity ent ->
            { model | rel = removeEntity ent model.rel } |> update BackToMain

        NoAction ->
            model

module Main exposing (..)

import Browser
import TagEnt exposing (..)
import Types exposing (Msg(..))
import View.Graph
import View.Main
import View.TagEnt


type View
    = Main View.Main.Pending
    | Tag Tag
    | Entity Entity
    | Graph


type alias Model =
    { tagEnt : TagEnt
    , view : View
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { tagEnt = TagEnt.example, view = Graph } --Main View.Main.NoChange }
        , view =
            \{ tagEnt, view } ->
                case view of
                    Main pending ->
                        View.Main.view tagEnt pending

                    Tag tag ->
                        View.TagEnt.tag tag tagEnt

                    Entity entity ->
                        View.TagEnt.entity entity tagEnt

                    Graph ->
                        View.Graph.view tagEnt
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

        GoToGraph ->
            { model | view = Graph }

        GoToMain ->
            { model | view = Main View.Main.NoChange }

        InputTag ent s ->
            { model | view = Main <| View.Main.PendingTag ent <| validate s }

        InputEntity s ->
            { model | view = Main <| View.Main.PendingEntity <| validate s }

        AddTag ent tag ->
            { model | tagEnt = addEdge ( ent, tag ) model.tagEnt } |> update GoToMain

        AddEntity ent ->
            { model | tagEnt = addEntity ent model.tagEnt } |> update GoToMain

        DeleteTag tag ->
            { model | tagEnt = removeTag tag model.tagEnt } |> update GoToMain

        DeleteEntity ent ->
            { model | tagEnt = removeEntity ent model.tagEnt } |> update GoToMain

        NoAction ->
            model

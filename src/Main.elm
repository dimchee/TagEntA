module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import TagEnt exposing (..)
import Task exposing (Task)
import Types exposing (..)
import View.Components
import View.Graph
import View.Main
import View.TagEnt


type View
    = Main View.Main.Pending
    | Tag Tag
    | Entity Entity
    | Graph (Maybe (Dict Id LR))


type alias Model =
    { tagEnt : TagEnt
    , view : View
    }


getLR : String -> Task Browser.Dom.Error LR
getLR id =
    let
        left { viewport, element } =
            { x = element.x - viewport.x, y = element.y - viewport.y + element.height / 2 }

        right { viewport, element } =
            { x = element.x - viewport.x + element.width, y = element.y - viewport.y + element.height / 2 }

        el =
            Browser.Dom.getElement id
    in
    Task.map2 (\l r -> { left = left l, right = right r }) el el


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                update GoToGraph <|
                    { tagEnt = TagEnt.example
                    , view = Main View.Main.NoChange
                    }
        , view =
            \{ tagEnt, view } ->
                case view of
                    Main pending ->
                        View.Main.view tagEnt pending

                    Tag tag ->
                        View.TagEnt.tag tag tagEnt

                    Entity entity ->
                        View.TagEnt.entity entity tagEnt

                    Graph lrs ->
                        View.Graph.view tagEnt lrs
        , update = update
        , subscriptions = \_ -> Browser.Events.onResize (\_ _ -> GetLRs)
        }


validate : String -> String
validate s =
    if String.length s < 10 then
        s

    else
        String.slice 0 -1 s


noCmd : a -> ( a, Cmd msg )
noCmd x =
    ( x, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedEntity ent ->
            { model | view = Entity ent } |> noCmd

        SelectedTag tag ->
            { model | view = Tag tag } |> noCmd

        GetLRs ->
            ( model
            , List.map View.Components.entityToId (TagEnt.entities model.tagEnt)
                ++ List.map View.Components.tagToId (TagEnt.tags model.tagEnt)
                |> List.map (\id -> Task.map (Tuple.pair id) <| getLR id)
                |> Task.sequence
                |> Task.map Dict.fromList
                |> Task.attempt Result.toMaybe
                |> Cmd.map GotLRs
            )

        GoToGraph ->
            { model | view = Graph Nothing } |> update GetLRs

        GoToMain ->
            { model | view = Main View.Main.NoChange } |> noCmd

        InputTag ent s ->
            { model | view = Main <| View.Main.PendingTag ent <| validate s } |> noCmd

        InputEntity s ->
            { model | view = Main <| View.Main.PendingEntity <| validate s } |> noCmd

        AddTag ent tag ->
            { model | tagEnt = addEdge ( ent, tag ) model.tagEnt } |> update GoToMain

        AddEntity ent ->
            { model | tagEnt = addEntity ent model.tagEnt } |> update GoToMain

        DeleteTag tag ->
            { model | tagEnt = removeTag tag model.tagEnt } |> update GoToMain

        DeleteEntity ent ->
            { model | tagEnt = removeEntity ent model.tagEnt } |> update GoToMain

        GotLRs lrs ->
            { model | view = Graph lrs } |> noCmd

        NoAction ->
            model |> noCmd

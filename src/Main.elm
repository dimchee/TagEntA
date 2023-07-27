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
    = Main MainArgs
    | Tag Tag
    | Entity Entity
    | Graph (Maybe (Dict Id LR))


defaultMainArgs : MainArgs
defaultMainArgs =
    { continuousSearch = True, query = Nothing, pending = PendingNothing }


mainArgs : View -> MainArgs
mainArgs view =
    case view of
        Main args ->
            args

        _ ->
            defaultMainArgs


withContinuousSearch : Bool -> MainArgs -> MainArgs
withContinuousSearch cs args =
    { args | continuousSearch = cs }


onContinuousSearch : (a -> a) -> View -> a -> a
onContinuousSearch f view =
    if mainArgs view |> .continuousSearch then
        f

    else
        identity


withQuery : Query -> MainArgs -> MainArgs
withQuery q args =
    { args | query = q }


withPending : Pending -> MainArgs -> MainArgs
withPending p args =
    { args | pending = p }


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
                { tagEnt = TagEnt.example
                , view = defaultMainArgs |> Main
                }
                    -- |> update GoToGraph
                    |> noCmd
        , view =
            \{ tagEnt, view } ->
                case view of
                    Main args ->
                        View.Main.view tagEnt args

                    Tag tag ->
                        View.TagEnt.tag tag tagEnt

                    Entity entity ->
                        View.TagEnt.entity entity tagEnt

                    Graph lrs ->
                        View.Graph.view tagEnt lrs
        , update = update
        , subscriptions =
            \{ view } ->
                case view of
                    Graph _ ->
                        Browser.Events.onResize (\_ _ -> GetLRs)

                    _ ->
                        Sub.none
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
            { model | view = mainArgs model.view |> Main } |> noCmd

        InputTag ent s ->
            { model | view = mainArgs model.view |> withPending (PendingTag ent <| validate s) |> Main } |> noCmd

        InputEntity s ->
            { model | view = mainArgs model.view |> withPending (PendingEntity <| validate s) |> Main } |> noCmd

        AddTag ent tag ->
            { model | tagEnt = addEdge ( ent, tag ) model.tagEnt, view = mainArgs model.view |> withPending PendingNothing |> Main }
                |> noCmd

        AddEntity ent ->
            { model | tagEnt = addEntity ent model.tagEnt, view = mainArgs model.view |> withPending PendingNothing |> Main }
                |> noCmd

        DeleteTag tag ->
            { model | tagEnt = removeTag tag model.tagEnt } |> update GoToMain

        DeleteEntity ent ->
            { model | tagEnt = removeEntity ent model.tagEnt } |> update GoToMain

        GotLRs lrs ->
            { model | view = Graph lrs } |> noCmd

        ChangeQuery query ->
            { model
                | view =
                    mainArgs model.view
                        |> onContinuousSearch (withQuery <| Just query) model.view
                        |> withPending (PendingSearch query)
                        |> Main
            }
                |> noCmd

        Search query ->
            { model | view = mainArgs model.view |> withQuery (Just query) |> withPending PendingNothing |> Main } |> noCmd

        ContinuousSearch cs ->
            { model | view = mainArgs model.view |> withContinuousSearch cs |> Main } |> noCmd

        NoAction ->
            model |> noCmd

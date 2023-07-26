module Types exposing (..)

import TagEnt exposing (..)


type Msg
    = SelectedEntity Entity
    | SelectedTag Tag
    | AddTag Entity Tag
    | AddEntity Entity
    | InputTag Entity String
    | InputEntity String
    | DeleteTag Tag
    | DeleteEntity Entity
    | GoToMain
    | GoToGraph
    | NoAction

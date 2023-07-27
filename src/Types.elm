module Types exposing (..)

import Dict exposing (Dict)
import TagEnt exposing (..)


type alias Point =
    { x : Float, y : Float }


type alias LR =
    { left : Point, right : Point }


type alias Id =
    String


type alias Query =
    Maybe String


type Msg
    = SelectedEntity Entity
    | SelectedTag Tag
    | AddTag Entity Tag
    | AddEntity Entity
    | InputTag Entity String
    | InputEntity String
    | GetLRs
    | GotLRs (Maybe (Dict Id LR))
    | DeleteTag Tag
    | DeleteEntity Entity
    | GoToMain
    | GoToGraph
    | ChangeQuery String
    | Search String
    | Focus Id
    | NoAction


type Pending
    = PendingTag Entity String
    | PendingEntity String
    | PendingNothing


type alias MainArgs =
    { query : Query, pending : Pending }

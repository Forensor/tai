module Tai exposing (..)

import Array exposing (Array)
import List exposing (concat)
import Main exposing (..)


type alias Nodes =
    List Model


getNodes : Model -> Nodes
getNodes model =
    if model.turn == Crosses then
        List.map (\c -> move X c model) allCoords

    else
        List.map (\c -> move O c model) allCoords

module Tai exposing (..)

import Main exposing (..)


type alias Nodes =
    List Board


gameEnded : Board -> Bool
gameEnded board =
    let
        inline ps =
            ps == [ Just X, Just X, Just X ] || ps == [ Just O, Just O, Just O ]

        possibilities =
            [ [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
            , [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
            , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
            , [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
            , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
            , [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
            , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
            , [ ( 0, 2 ), ( 1, 1 ), ( 2, 0 ) ]
            ]
    in
    List.any ((==) True) <| List.map (\cs -> inline (List.map (\c -> getPiece c board) cs)) possibilities


getNodes : Board -> Bool -> Nodes
getNodes board maximizing =
    let
        allCoords =
            [ ( 0, 0 )
            , ( 0, 1 )
            , ( 0, 2 )
            , ( 1, 0 )
            , ( 1, 1 )
            , ( 1, 2 )
            , ( 2, 0 )
            , ( 2, 1 )
            , ( 2, 2 )
            ]
    in
    if maximizing then
        List.map (\c -> placePiece c board X) allCoords

    else
        List.map (\c -> placePiece c board O) allCoords


alphabeta : Board -> Bool -> Int
alphabeta board maximizing =
    if gameEnded board then
        if maximizing then
            1

        else
            -1

    else if (List.length <| getNodes board maximizing) == 0 then
        0

    else
        0


takeMax : Nodes
takeMax =
    []

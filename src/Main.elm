module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)


type alias Board =
    List (List Piece)


type Piece
    = X
    | O
    | E


type PlayerType
    = Human
    | Tai


type Team
    = Crosses
    | Noughts


type alias Model =
    { board : Board
    , turn : Team
    , crosses : PlayerType
    , noughts : PlayerType
    }



-- A3-C1


type alias Coord =
    ( Int, Int )


init : Model
init =
    { board = List.repeat 3 [ E, E, E ]
    , turn = Crosses
    , crosses = Human
    , noughts = Human
    }


placePiece : Coord -> Board -> Piece -> Board
placePiece ( r, c ) b p =
    case List.take 1 <| List.drop r b of
        [ [] ] ->
            b

        [ ps ] ->
            List.take r b ++ placePiece2 c ps p :: List.drop (r + 1) b

        _ ->
            b


placePiece2 : Int -> List Piece -> Piece -> List Piece
placePiece2 n ps p =
    let
        newRow =
            List.take n ps ++ p :: List.drop (n + 1) ps
    in
    case List.length newRow of
        3 ->
            newRow

        _ ->
            ps


getPiece : Coord -> Board -> Maybe Piece
getPiece ( r, c ) b =
    case List.drop r <| List.take (r + 1) b of
        [ [] ] ->
            Nothing

        [ ps ] ->
            getPiece2 c ps

        _ ->
            Nothing


getPiece2 : Int -> List Piece -> Maybe Piece
getPiece2 n ps =
    case List.drop n <| List.take (n + 1) ps of
        [ p ] ->
            Just p

        _ ->
            Nothing


type GameType
    = HumanVsHuman
    | HumanVsTai
    | TaiVsHuman


type Msg
    = Move Piece Coord
    | ChangeGameType GameType


gameTypeToModel : GameType -> Model
gameTypeToModel gameType =
    case gameType of
        HumanVsHuman ->
            { init | crosses = Human, noughts = Human }

        HumanVsTai ->
            { init | crosses = Human, noughts = Tai }

        TaiVsHuman ->
            { init | crosses = Tai, noughts = Human }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGameType gameType ->
            gameTypeToModel gameType

        Move _ _ ->
            model


view : Model -> Html Msg
view model =
    div []
        [ text "Hey how you doin'"
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }

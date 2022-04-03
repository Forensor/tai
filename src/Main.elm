module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (id, src)
import Html.Events exposing (onClick)



-- Types ----------------------------------------------------------------------


type alias Board =
    List (List Piece)


type GameType
    = HumanVsHuman
    | HumanVsTai
    | TaiVsHuman


type Msg
    = Move Piece Coord
    | ChangeGameType GameType


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


type alias Coord =
    ( Int, Int )



-- Initialization -------------------------------------------------------------


init : Model
init =
    { board = List.repeat 3 [ E, E, E ]
    , turn = Crosses
    , crosses = Human
    , noughts = Human
    }


nextTurn : Team -> Team
nextTurn turn =
    case turn of
        Crosses ->
            Noughts

        Noughts ->
            Crosses


move : Piece -> Coord -> Model -> Model
move piece coord model =
    let
        newBoard =
            placePiece coord model.board piece
    in
    case getPiece coord model.board of
        Just E ->
            { model | board = newBoard, turn = nextTurn model.turn }

        _ ->
            model


placePiece : Coord -> Board -> Piece -> Board
placePiece ( row, col ) board piece =
    case List.take 1 <| List.drop row board of
        [ [] ] ->
            board

        [ ps ] ->
            List.take row board ++ placePiece2 col ps piece :: List.drop (row + 1) board

        _ ->
            board


placePiece2 : Int -> List Piece -> Piece -> List Piece
placePiece2 index pieces piece =
    let
        newRow =
            List.take index pieces ++ piece :: List.drop (index + 1) pieces
    in
    case List.length newRow of
        3 ->
            newRow

        _ ->
            pieces



-- Helpers --------------------------------------------------------------------


getPiece : Coord -> Board -> Maybe Piece
getPiece ( row, col ) board =
    case List.drop row <| List.take (row + 1) board of
        [ [] ] ->
            Nothing

        [ ps ] ->
            getPiece2 col ps

        _ ->
            Nothing


getPiece2 : Int -> List Piece -> Maybe Piece
getPiece2 index pieces =
    case List.drop index <| List.take (index + 1) pieces of
        [ p ] ->
            Just p

        _ ->
            Nothing


gameTypeToModel : GameType -> Model
gameTypeToModel gameType =
    case gameType of
        HumanVsHuman ->
            { init | crosses = Human, noughts = Human }

        HumanVsTai ->
            { init | crosses = Human, noughts = Tai }

        TaiVsHuman ->
            { init | crosses = Tai, noughts = Human }



-- Update ---------------------------------------------------------------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGameType gameType ->
            gameTypeToModel gameType

        Move piece coord ->
            move piece coord model



-- Views ----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ text "Hey how you doin'"
        , viewBoard model
        , text "Another text"
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        indexedRows =
            List.indexedMap Tuple.pair model.board

        rowDivs =
            case model.turn of
                Crosses ->
                    List.map (viewRow X) <| indexedRows

                Noughts ->
                    List.map (viewRow O) <| indexedRows
    in
    div [ id "board" ] <| List.concat rowDivs


viewRow : Piece -> ( Int, List Piece ) -> List (Html Msg)
viewRow playingPiece ( r, row ) =
    let
        indexedCols =
            List.indexedMap Tuple.pair row
    in
    List.map (viewPiece playingPiece r) indexedCols


viewPiece : Piece -> Int -> ( Int, Piece ) -> Html Msg
viewPiece playingPiece row ( col, piece ) =
    case piece of
        E ->
            div [ onClick <| Move playingPiece ( row, col ) ] [ text "E" ]

        X ->
            div [] [ text "X" ]

        O ->
            div [] [ text "O" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }

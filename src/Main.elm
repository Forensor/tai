module Main exposing (..)

import Array exposing (Array)
import Browser
import Debug exposing (toString)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (id, src)
import Html.Events exposing (onClick)



-- Types ----------------------------------------------------------------------


type alias Board =
    Array (Array Piece)


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
    { board = Array.repeat 3 <| Array.fromList [ E, E, E ]
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
    case Array.get row board of
        Just ps ->
            Array.set row (Array.set col piece ps) board

        Nothing ->
            board



-- Helpers --------------------------------------------------------------------


getPiece : Coord -> Board -> Maybe Piece
getPiece ( row, col ) board =
    case Array.get row board of
        Just ps ->
            Array.get col ps

        Nothing ->
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


allCoords : List ( Int, Int )
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



-- Update ---------------------------------------------------------------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGameType gameType ->
            gameTypeToModel gameType

        Move piece coord ->
            move piece coord model



-- Views ----------------------------------------------------------------------


teamToPiece : Team -> Piece
teamToPiece team =
    case team of
        Crosses ->
            X

        Noughts ->
            O


view : Model -> Html Msg
view model =
    div []
        [ text "Buttons to select gameplay"
        , viewBoard model
        , viewInfo model
        ]


viewInfo : Model -> Html Msg
viewInfo model =
    if gameWon model then
        text (toString (nextTurn model.turn) ++ " won!")
    else if noMoreMoves model then
        text "Draw"

    else
        text (toString model.turn)


gameWon : Model -> Bool
gameWon model =
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
    List.any ((==) True) <| List.map (\cs -> inline (List.map (\c -> getPiece c model.board) cs)) possibilities

noMoreMoves : Model -> Bool
noMoreMoves model = 
    List.all ((/=) (Just E)) <| List.map (\c -> getPiece c model.board) allCoords


viewBoard : Model -> Html Msg
viewBoard model =
    div [] (List.map (viewPiece model) allCoords)


viewPiece : Model -> Coord -> Html Msg
viewPiece model coord =
    case getPiece coord model.board of
        Just X ->
            div [] [ text "X" ]

        Just O ->
            div [] [ text "O" ]

        _ ->
            div
                (if gameWon model then
                    []

                 else
                    [ onClick <| Move (teamToPiece model.turn) coord ]
                )
                [ text "Empty Square" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }

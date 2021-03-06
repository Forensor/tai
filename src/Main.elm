module Main exposing (..)

import Array exposing (Array, fromList, get, repeat, set)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import List exposing (all, any, map)



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
    { board = repeat 3 <| fromList [ E, E, E ]
    , turn = Crosses
    , crosses = Human
    , noughts = Human
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }



-- Helpers --------------------------------------------------------------------


nextTurn : Team -> Team
nextTurn turn =
    case turn of
        Crosses ->
            Noughts

        Noughts ->
            Crosses


showTeam : Team -> String
showTeam team =
    case team of
        Crosses ->
            "Crosses"

        Noughts ->
            "Noughts"


teamToPiece : Team -> Piece
teamToPiece team =
    case team of
        Crosses ->
            X

        Noughts ->
            O


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



-- Game manipulation ----------------------------------------------------------


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


getPiece : Coord -> Board -> Maybe Piece
getPiece ( row, col ) board =
    get row board |> Maybe.andThen (get col)


placePiece : Coord -> Board -> Piece -> Board
placePiece ( row, col ) board piece =
    case get row board of
        Just ps ->
            set row (set col piece ps) board

        Nothing ->
            board



-- Game control ---------------------------------------------------------------


noMoreMoves : Model -> Bool
noMoreMoves model =
    all ((/=) (Just E)) <| map (\c -> getPiece c model.board) allCoords


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
    any ((==) True) <|
        map (\cs -> inline (map (\c -> getPiece c model.board) cs)) possibilities



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
    div [ id "app" ]
        [ viewPanel
        , viewBoard model
        , viewInfo model
        ]


viewPanel : Html Msg
viewPanel =
    div [ id "panel" ]
        [ button [ onClick <| ChangeGameType HumanVsHuman ]
            [ text "Human vs Human" ]
        , button [ onClick <| ChangeGameType HumanVsTai ]
            [ text "Human vs Tai" ]
        , button [ onClick <| ChangeGameType TaiVsHuman ]
            [ text "Tai vs Human" ]
        ]


viewInfo : Model -> Html Msg
viewInfo model =
    div [ id "info" ]
        ([]
            ++ (if gameWon model then
                    [ text (showTeam (nextTurn model.turn) ++ " won!") ]

                else if noMoreMoves model then
                    [ text "Draw" ]

                else
                    [ text (showTeam model.turn ++ "' turn") ]
               )
        )


viewBoard : Model -> Html Msg
viewBoard model =
    div [ id "board" ] (map (viewPiece model) allCoords)


viewPiece : Model -> Coord -> Html Msg
viewPiece model coord =
    case getPiece coord model.board of
        Just X ->
            div [ classList [ ( "square", True ), ( "x", True ) ] ] []

        Just O ->
            div [ classList [ ( "square", True ), ( "o", True ) ] ] []

        _ ->
            div
                (class "square"
                    :: (if gameWon model then
                            []

                        else
                            [ onClick <| Move (teamToPiece model.turn) coord ]
                       )
                )
                []

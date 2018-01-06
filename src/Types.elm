module Types exposing (..)

import Dict exposing (..)
import Set exposing (..)


type alias Board =
    Dict ( Int, Int ) Cell


type Position
    = Config
    | PieceSelection
    | TurnSelection
    | Playing
    | EndGame


type CellState
    = UnPlayable Col
    | Contain Piece
    | Empty


type Col
    = Grey
    | Rainbow String


type alias Piece =
    { value : Int
    , playerId : Int
    }


dummyPiece =
    { value = 0, playerId = -1 }


type alias Player =
    { deck : List Piece
    , name : String
    , id : Int
    , choice : Maybe Piece
    , turn : Maybe Int
    , score : Int
    }


type alias Cell =
    { xPos : Int
    , yPos : Int
    , state : CellState
    }


type alias Model =
    { boardSize : Int
    , nbrPlayers : Int
    , players : Dict Int Player
    , availableTurns : Set Int
    , turnSelectionOrder : List Int
    , playingOrder : List Int
    , board : Board
    , currentPlayer : Maybe Int
    , position : Position
    }


type Msg
    = SetPlayerNumber String
    | InitializePlayers
    | PickUpPiece Piece
    | SelectTurn Int
    | PutDownPiece ( Int, Int )

module Types exposing (..)

import Dict exposing (..)


type alias Board =
    Dict ( Int, Int ) Cell


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


type alias Player =
    { deck : List Piece
    , name : String
    , id : Int
    , choice : Maybe Piece
    , score : Int
    }


type alias Cell =
    { xPos : Int
    , yPos : Int
    , state : CellState
    }


type alias Model =
    { boardSize : Int
    , players : Dict Int Player
    , board : Board
    , currentPlayer : Maybe Int
    }


type Msg
    = SetPlayerNumber String
    | PickUpPiece Piece
    | PutDownPiece ( Int, Int )

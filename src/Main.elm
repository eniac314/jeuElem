module Main exposing (..)

import Array exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Types exposing (..)
import View exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


n =
    6


init =
    { boardSize = n
    , players = makePlayers 6
    , board =
        boardWithEdge n (hexaBoard n)

    --|> rainbowEdge n
    , currentPlayer = Just 1
    , currentPiece = Nothing
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayerNumber n ->
            case String.toInt n of
                Ok n ->
                    { model | players = makePlayers n, currentPlayer = Just 6 } ! []

                Err _ ->
                    model ! []

        PickUpPiece ({ value, playerId } as piece) ->
            let
                newPlayers =
                    Dict.update playerId
                        (\mv ->
                            case mv of
                                Nothing ->
                                    Nothing

                                Just ({ deck, name, id } as player) ->
                                    Just { player | deck = List.filter (\p -> p.value /= value) deck }
                        )
                        model.players
            in
            case model.currentPiece of
                Nothing ->
                    { model
                        | currentPiece = Just piece
                        , players = newPlayers
                    }
                        ! []

                Just _ ->
                    model ! []

        PutDownPiece ( xPos, yPos ) ->
            case ( model.currentPiece, Dict.get ( xPos, yPos ) model.board ) of
                ( Nothing, _ ) ->
                    model ! []

                ( Just ({ value, playerId } as piece), Just cell ) ->
                    case cell.state of
                        UnPlayable _ ->
                            model ! []

                        Contain _ ->
                            model ! []

                        _ ->
                            let
                                newCell =
                                    Cell xPos yPos (Contain piece)

                                newBoard =
                                    Dict.insert ( xPos, yPos ) newCell model.board
                            in
                            { model
                                | board = newBoard
                                , currentPlayer =
                                    Just <|
                                        if playerId == Dict.size model.players then
                                            1
                                        else
                                            playerId + 1
                                , currentPiece = Nothing
                            }
                                ! []

                _ ->
                    model ! []


subscriptions model =
    Sub.none


makePlayers n =
    let
        makeDeck id =
            List.range 0 14
                |> List.map (\v -> Piece v id)

        makePlayer id =
            Player (makeDeck id) ("toto " ++ toString id) id
    in
    List.map makePlayer (List.range 1 n)
        |> List.foldr (\p res -> Dict.insert p.id p res) Dict.empty


hexaBoard : Int -> Board
hexaBoard n =
    let
        makeRowTop i =
            List.map (\j -> ( j, i )) (List.range 0 (n + i))

        makeRowBottom i =
            List.map (\j -> ( j, i )) (List.range 0 (2 * n + (n - i)))

        topHalf =
            List.concatMap makeRowTop (List.range 0 n)

        bottomHalf =
            List.concatMap makeRowBottom (List.range (n + 1) (2 * n))
    in
    (topHalf ++ bottomHalf)
        |> List.foldr
            (\( x, y ) res ->
                Dict.insert ( x, y ) (Cell x y Empty) res
            )
            Dict.empty


neighbours : Int -> Int -> List ( Int, Int )
neighbours j i =
    [ ( j - 1, i - 1 )
    , ( j, i - 1 )
    , ( j - 1, i )
    , ( j + 1, i )
    , ( j, i + 1 )
    , ( j + 1, i + 1 )
    ]



-------------------------------------------------------------------------------


isEdge : Int -> Cell -> Bool
isEdge n { xPos, yPos, state } =
    xPos == 0 || yPos == 0 || yPos == 2 * n || (xPos == n + yPos) || (xPos + yPos == 3 * n)


boardWithEdge : Int -> Board -> Board
boardWithEdge n board =
    Dict.map
        (\key cell ->
            { cell
                | state =
                    if isEdge n cell then
                        UnPlayable Grey
                    else
                        cell.state
            }
        )
        board



-------------------------------------------------------------------------------


rainbowEdge : Int -> Board -> Board
rainbowEdge n board =
    let
        offset =
            2 * pi / (toFloat n * 6)

        cells =
            goAround n
    in
    List.foldr
        (\cell ( n, acc ) ->
            ( n + offset
            , Dict.update cell
                (\v ->
                    case v of
                        Nothing ->
                            Nothing

                        Just cell_ ->
                            case cell_.state of
                                UnPlayable _ ->
                                    Just { cell_ | state = UnPlayable (Rainbow (color (0.4 * n))) }

                                _ ->
                                    Just cell_
                )
                acc
            )
        )
        ( 0, board )
        cells
        |> Tuple.second


goAround n =
    let
        range_ a b =
            List.range b a
                |> List.reverse

        xs =
            List.range 0 (2 * n) ++ range_ (2 * n - 1) 0 ++ List.repeat (2 * n - 1) 0

        ys =
            List.repeat n 0 ++ List.range 0 (2 * n) ++ List.repeat n (2 * n) ++ range_ (2 * n - 1) 1
    in
    List.map2 (,) xs ys


color n =
    let
        red =
            sin n * 127 + 128

        green =
            sin (2 * pi * n / 3) * 127 + 128

        blue =
            sin (4 * pi * n / 3) * 127 + 128
    in
    "rgb(" ++ toString (round red) ++ ", " ++ toString (round green) ++ ", " ++ toString (round blue) ++ ")"

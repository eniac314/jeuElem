module Main exposing (..)

import Array exposing (..)
import Data exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Set exposing (..)
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
    , nbrPlayers = 0
    , players = Dict.empty
    , board =
        boardWithEdge n (hexaBoard n)
    , availableTurns = Set.empty
    , turnSelectionOrder = []
    , playingOrder = []

    --|> rainbowEdge n
    , currentPlayer = Nothing
    , position = Config
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayerNumber n ->
            case String.toInt n of
                Ok n ->
                    { model | nbrPlayers = n } ! []

                Err _ ->
                    model ! []

        InitializePlayers ->
            { model
                | players = makePlayers model.nbrPlayers
                , currentPlayer = Just 1
                , position = PieceSelection
                , availableTurns = Set.fromList (List.range 1 model.nbrPlayers)
            }
                ! []

        PickUpPiece ({ value, playerId } as piece) ->
            case
                Dict.get playerId model.players
                    |> Maybe.andThen .choice
            of
                -- le joueur n'a pas encore choisi sa piece
                Nothing ->
                    let
                        newPlayers =
                            Dict.update playerId
                                (\mv ->
                                    case mv of
                                        Nothing ->
                                            Nothing

                                        Just ({ deck, name, id } as player) ->
                                            Just
                                                { player
                                                    | deck = List.filter (\p -> p.value /= value) deck
                                                    , choice = Just piece
                                                }
                                )
                                model.players
                    in
                    if playerId == Dict.size model.players then
                        -- si dernier joueur qui choisi sa piece calcul des scores et passage a la
                        -- selection des tours
                        let
                            playersWithScore =
                                Dict.map
                                    (\_ p ->
                                        { p
                                            | score =
                                                List.foldr
                                                    (\p2 acc ->
                                                        let
                                                            piece1 =
                                                                Maybe.withDefault dummyPiece p.choice

                                                            piece2 =
                                                                Maybe.withDefault piece1 p2.choice
                                                        in
                                                        acc + shifumi piece1 piece2
                                                    )
                                                    0
                                                    players
                                        }
                                    )
                                    newPlayers

                            players =
                                Dict.values model.players

                            turnSelectionOrder =
                                Dict.values playersWithScore
                                    |> List.sortBy .score
                                    |> List.reverse
                                    |> List.map .id
                        in
                        { model
                            | position = TurnSelection
                            , turnSelectionOrder = turnSelectionOrder
                            , currentPlayer = List.head turnSelectionOrder
                            , players = playersWithScore
                        }
                            ! []
                    else
                        -- sinon passage au joueur suivant
                        { model
                            | players = newPlayers
                            , currentPlayer =
                                Just <|
                                    playerId
                                        + 1
                        }
                            ! []

                -- Le joueur en cours a deja une piece, cas impossible en theorie
                _ ->
                    model ! []

        SelectTurn n ->
            case
                model.currentPlayer
                    |> Maybe.andThen (\p -> Dict.get p model.players)
            of
                Nothing ->
                    model ! []

                Just cp ->
                    if Set.member n model.availableTurns then
                        let
                            newPlayers =
                                Dict.update cp.id
                                    (\mv ->
                                        case mv of
                                            Nothing ->
                                                Nothing

                                            Just ({ turn } as player) ->
                                                Just
                                                    { player
                                                        | turn = Just n
                                                    }
                                    )
                                    model.players

                            ( newPos, newCurrentPlayer, newTurnSelectionOrder, playingOrder ) =
                                case model.turnSelectionOrder of
                                    [] ->
                                        ( TurnSelection, Nothing, [], [] )

                                    x :: [] ->
                                        let
                                            plOrd =
                                                newPlayers
                                                    |> Dict.values
                                                    |> List.filterMap
                                                        (\p ->
                                                            .turn p
                                                                |> Maybe.andThen (\t -> Just ( .id p, t ))
                                                        )
                                                    |> List.sortBy Tuple.second
                                                    |> List.map Tuple.first
                                        in
                                        ( Playing
                                        , List.head plOrd
                                        , []
                                        , plOrd
                                        )

                                    x :: xs ->
                                        ( TurnSelection, List.head xs, xs, [] )
                        in
                        { model
                            | players = newPlayers
                            , availableTurns = Set.remove n model.availableTurns
                            , turnSelectionOrder = newTurnSelectionOrder
                            , currentPlayer = newCurrentPlayer
                            , position = newPos
                            , playingOrder = playingOrder
                        }
                            ! []
                    else
                        model ! []

        PutDownPiece ( xPos, yPos ) ->
            let
                currentPiece =
                    model.currentPlayer
                        |> Maybe.andThen
                            (\k ->
                                Dict.get k model.players
                            )
                        |> Maybe.andThen .choice
            in
            case ( currentPiece, Dict.get ( xPos, yPos ) model.board, model.position ) of
                ( Nothing, _, _ ) ->
                    model ! []

                ( Just ({ value, playerId } as piece), Just cell, Playing ) ->
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
                                        |> update_board newCell model.boardSize

                                newPlayers =
                                    Dict.update playerId
                                        (\mv ->
                                            case mv of
                                                Nothing ->
                                                    Nothing

                                                Just ({ deck, name, id } as player) ->
                                                    Just
                                                        { player
                                                            | choice = Nothing
                                                        }
                                        )
                                        model.players
                            in
                            case model.playingOrder of
                                x :: [] ->
                                    { model
                                        | board = newBoard
                                        , players = newPlayers
                                        , currentPlayer = Just 1
                                        , position = PieceSelection
                                        , availableTurns = Set.fromList (List.range 1 model.nbrPlayers)
                                        , turnSelectionOrder = []
                                        , playingOrder = []
                                    }
                                        ! []

                                x :: xs ->
                                    { model
                                        | board = newBoard
                                        , players = newPlayers
                                        , currentPlayer =
                                            List.tail model.playingOrder
                                                |> Maybe.andThen List.head
                                        , playingOrder = Maybe.withDefault [] (List.tail model.playingOrder)
                                    }
                                        ! []

                                [] ->
                                    model ! []

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
            Player (makeDeck id) ("toto " ++ toString id) id Nothing Nothing 0
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


neighbours : Cell -> Int -> List ( Int, Int )
neighbours c size =
    let
        x =
            c.xPos

        y =
            c.yPos

        always =
            [ ( x, y - 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x, y + 1 )
            ]
    in
    if y < size then
        ( x - 1, y - 1 ) :: ( x + 1, y + 1 ) :: always
    else if y == size then
        ( x - 1, y - 1 ) :: ( x - 1, y + 1 ) :: always
    else
        ( x + 1, y - 1 ) :: ( x - 1, y + 1 ) :: always


shifumi : Piece -> Piece -> Int
shifumi piece1 piece2 =
    let
        val1 =
            piece1.value

        val2 =
            piece2.value
    in
    Dict.get ( val1, val2 ) shifumiTable
        |> Maybe.withDefault 0



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


update_board : Cell -> Int -> Board -> Board
update_board c boardSize board =
    let
        update_board_helper board cells_todo cells_done =
            case cells_todo of
                [] ->
                    board

                c :: cs ->
                    case c.state of
                        Contain piece ->
                            if not (List.member c cells_done) then
                                let
                                    ( new_board, new_cells_todo ) =
                                        convert_neighbours piece (neighbours c boardSize) board cells_todo cells_done
                                in
                                update_board_helper new_board new_cells_todo (c :: cells_done)
                            else
                                update_board_helper board cs cells_done

                        _ ->
                            update_board_helper board cs cells_done

        convert_neighbours piece n_list board cells_todo cells_done =
            case n_list of
                [] ->
                    ( board, cells_todo )

                n :: ns ->
                    case Dict.get n board of
                        Just n_cell ->
                            case n_cell.state of
                                Contain n_piece ->
                                    if not (List.member n_cell cells_done) && (piece.playerId /= n_piece.playerId) then
                                        let
                                            new_n_piece =
                                                { n_piece | playerId = piece.playerId }

                                            new_n_cell =
                                                { n_cell | state = Contain new_n_piece }

                                            new_board =
                                                Dict.insert n new_n_cell board
                                        in
                                        case shifumi piece n_piece of
                                            1 ->
                                                convert_neighbours piece ns new_board cells_todo cells_done

                                            2 ->
                                                convert_neighbours piece ns new_board (cells_todo ++ [ new_n_cell ]) cells_done

                                            _ ->
                                                convert_neighbours piece ns board cells_todo cells_done
                                    else
                                        convert_neighbours piece ns board cells_todo cells_done

                                _ ->
                                    convert_neighbours piece ns board cells_todo cells_done

                        Nothing ->
                            convert_neighbours piece ns board cells_todo cells_done
    in
    update_board_helper board [ c ] []


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

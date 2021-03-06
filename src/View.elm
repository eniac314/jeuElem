module View exposing (..)

import Data exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import Types exposing (..)


view model =
    case model.position of
        Config ->
            configView model

        PieceSelection ->
            boardView model

        TurnSelection ->
            turnSelectionView model

        Playing ->
            boardView model

        EndGame ->
            endGameView model


configView model =
    div []
        [ label [ Attr.for "nbrPLayerInput" ]
            [ Html.text "How many players?" ]
        , br [] []
        , Html.input
            [ Attr.id "nbrPLayerInput"
            , onInput SetPlayerNumber
            ]
            []
        , br [] []
        , button [ onClick InitializePlayers ]
            [ Html.text "Go" ]
        ]


turnSelectionView model =
    let
        turn n =
            div
                [ onClick (SelectTurn n)
                , Attr.style
                    [ ( "padding", "1em" )
                    , ( "cursor", "pointer" )
                    ]
                ]
                [ Html.text <| "turn " ++ toString n ]
    in
    div []
        ([ Html.text <|
            "Current player: "
                ++ (model.currentPlayer
                        |> Maybe.andThen (\id -> Dict.get id model.players)
                        |> Maybe.map (toString << .id)
                        |> Maybe.withDefault "error"
                   )
         , br [] []
         , Html.text <|
            "Current player score: "
                ++ toString
                    (model.currentPlayer
                        |> Maybe.andThen (\id -> Dict.get id model.players)
                        |> Maybe.map (toString << .score)
                        |> Maybe.withDefault "error"
                    )
         , br [] []
         ]
            ++ Set.foldr (\n acc -> turn n :: acc) [] model.availableTurns
        )


boardView model =
    div [ Attr.style [ ( "width", "100%" ) ] ]
        [ hexaBoardSvg model.boardSize 35 model.board
        , br [] []
        , scores model
        , div
            [ Attr.style
                [ ( "margin", "auto" )
                , ( "width", "750px" )

                --, ( "background-color", "red" )
                ]
            ]
            [ div [ Attr.style [ ( "padding-bottom", "0.75em" ) ] ]
                [ Html.text <|
                    "player "
                        ++ (model.currentPlayer
                                |> Maybe.andThen (Just << toString)
                                |> Maybe.withDefault "Error"
                           )
                ]
            , deckSvg model
            ]
        , div
            [ Attr.style
                [ ( "width", "100%" )
                , ( "padding-top", "0.75em" )

                --, ( "background-color", "red" )
                ]
            ]
            [ selectedSvg model

            --, br [] []
            --, winLose piece
            ]
        ]


endGameView model =
    div [] []


hexaSvg : Float -> Float -> CellState -> Float -> Float -> Float -> List (Svg Msg)
hexaSvg x y state l u v =
    let
        offset =
            fromPolar ( l, pi / 3 )
                |> (\( x, y ) -> l - y)

        radius =
            l + offset

        points =
            [ ( radius, 0 )
            , ( radius, pi / 3 )
            , ( radius, 2 * pi / 3 )
            , ( radius, pi )
            , ( radius, 4 * pi / 3 )
            , ( radius, 5 * pi / 3 )
            ]
                |> List.map (\( l, a ) -> ( l, a + pi / 6 ))
                |> List.map fromPolar
                |> List.map (\( u, v ) -> ( u + x, v + y ))
                |> List.foldr (\( u, v ) acc -> acc ++ (toString u ++ ", " ++ toString v ++ " ")) ""
    in
    (case state of
        Contain { value, playerId } ->
            [ polygon
                [ SvgAttr.fill <| playerColor playerId
                , SvgAttr.points points
                ]
                []
            ]

        _ ->
            []
    )
        ++ [ polygon
                [ case state of
                    Empty ->
                        SvgAttr.fill "white"

                    UnPlayable Grey ->
                        SvgAttr.fill "grey"

                    UnPlayable (Rainbow c) ->
                        SvgAttr.fill c

                    Contain { value, playerId } ->
                        SvgAttr.fill <| "url(#piece" ++ toString value ++ ")"
                , SvgAttr.stroke "black"
                , SvgAttr.strokeWidth "2px"
                , SvgAttr.points points
                , SvgEvents.onClick (PutDownPiece ( round u, round v ))
                , case state of
                    Empty ->
                        SvgAttr.cursor "pointer"

                    _ ->
                        SvgAttr.cursor "default"
                ]
                []

           --, text_
           --     [ SvgAttr.x (toString <| x - l / 1.5)
           --     , SvgAttr.y (toString <| y)
           --     , SvgAttr.stroke "black"
           --     ]
           --     [ Svg.text ("(" ++ toString u ++ ", " ++ toString v ++ ")") ]
           ]


deckHexaSvg x y radius { value, playerId } =
    let
        points =
            [ ( radius, 0 )
            , ( radius, pi / 3 )
            , ( radius, 2 * pi / 3 )
            , ( radius, pi )
            , ( radius, 4 * pi / 3 )
            , ( radius, 5 * pi / 3 )
            ]
                |> List.map (\( l, a ) -> ( l, a + pi / 6 ))
                |> List.map fromPolar
                |> List.map (\( u, v ) -> ( u + x, v + y ))
                |> List.foldr (\( u, v ) acc -> acc ++ (toString u ++ ", " ++ toString v ++ " ")) ""
    in
    [ polygon
        [ SvgAttr.fill <| playerColor playerId
        , SvgAttr.points points
        ]
        []
    , polygon
        [ SvgAttr.fill <| "url(#piece" ++ toString value ++ ")"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2px"
        , SvgAttr.points points
        , SvgEvents.onClick (PickUpPiece (Piece value playerId))
        , SvgAttr.cursor "pointer"
        ]
        []

    --, text_
    --    [ SvgAttr.x (toString <| x - radius / 1.5)
    --    , SvgAttr.y (toString <| y)
    --    , SvgAttr.stroke "black"
    --    ]
    --    [ Svg.text (toString value) ]
    ]


selectedHexaSvg radius { value, playerId } =
    let
        points =
            [ ( radius, 0 )
            , ( radius, pi / 3 )
            , ( radius, 2 * pi / 3 )
            , ( radius, pi )
            , ( radius, 4 * pi / 3 )
            , ( radius, 5 * pi / 3 )
            ]
                |> List.map (\( l, a ) -> ( l, a + pi / 6 ))
                |> List.map fromPolar
                |> List.map (\( u, v ) -> ( u + 50, v + 50 ))
                |> List.foldr (\( u, v ) acc -> acc ++ (toString u ++ ", " ++ toString v ++ " ")) ""
    in
    [ polygon
        [ SvgAttr.fill <| playerColor playerId
        , SvgAttr.points points
        ]
        []
    , polygon
        [ SvgAttr.fill <| "url(#piece" ++ toString value ++ ")"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2px"
        , SvgAttr.points points
        ]
        []
    ]



-------------------------------------------------------------------------------


pieceSvg { value, playerId } =
    div []
        [ Html.text <| "value: " ++ toString value ++ "playerId: " ++ toString playerId ]


deckSvg : Model -> Html Msg
deckSvg model =
    case
        model.currentPlayer
            |> Maybe.andThen (\id -> Dict.get id model.players)
    of
        Nothing ->
            span [] []

        Just { deck, name, id } ->
            let
                nbrPieces =
                    List.length deck

                sizeX =
                    1000

                sizeY =
                    75

                l =
                    35

                offset =
                    sizeX / toFloat nbrPieces

                coords =
                    List.map (\n -> ( toFloat n * offset + l, sizeY / 2 ))
                        (List.range 0 (nbrPieces - 1))

                pieces =
                    List.map2 (,) coords deck
                        |> List.concatMap (\( ( x, y ), piece ) -> deckHexaSvg x y l piece)

                def =
                    defs []
                        piecesPatterns
            in
            svg
                [ SvgAttr.width "750"
                , SvgAttr.viewBox <| "0 0 " ++ toString sizeX ++ " " ++ toString sizeY
                ]
                (def
                    :: pieces
                )


hexaBoardSvg n l board =
    let
        coords =
            board
                |> Dict.values
                |> List.map (\{ xPos, yPos, state } -> ( xPos, yPos, state ))
                |> List.map (\( u, v, state ) -> ( toFloat u, toFloat v, state ))

        adjust v =
            if round v % 2 == 0 then
                0
            else
                0

        offset v =
            if v >= toFloat n + 1 then
                v - n
            else
                n - v

        yOffset x y =
            fromPolar ( l, pi / 3 )
                |> (\( x, y ) -> l - y)

        cells =
            List.concatMap
                (\( u, v, state ) ->
                    hexaSvg
                        (l + u * l * 2 + offset v * l)
                        (v * 2 * l + 2 * l - v * 2 * yOffset u v)
                        state
                        l
                        u
                        v
                )
                coords

        size =
            toString (l * 4 * n + 2 * l)

        def =
            defs []
                piecesPatterns
    in
    div
        [ Attr.style
            [ ( "width", "100%" )

            --, ( "background-color", "blue" )
            ]
        ]
        [ div
            [ Attr.style
                [ ( "margin", "auto" )

                --, ( "background-color", "red" )
                , ( "width", "50%" )
                , ( "max-width", "700px" )

                --, ( "height", "650px" )
                ]
            ]
            [ svg
                [ SvgAttr.width "100%"
                , SvgAttr.height "100%"
                , SvgAttr.viewBox <| "0 0 " ++ size ++ " " ++ size
                ]
                (def :: cells)
            ]

        --, br [] []
        --, Html.text <| "number of cells: " ++ (toString <| List.length cells)
        ]


selectedSvg model =
    let
        piece =
            model.currentPlayer
                |> Maybe.andThen
                    (\k ->
                        Dict.get k model.players
                    )
                |> Maybe.andThen .choice
    in
    case piece of
        Nothing ->
            span [] []

        --div
        --    [ Attr.style
        --        [ ( "width", "100px" )
        --        , ( "height", "100px" )
        --        , ( "margin", "auto" )
        --        , ( "border-style", "solid" )
        --        , ( "border-color", "black" )
        --        ]
        --    ]
        --    []
        Just piece ->
            case model.position of
                Playing ->
                    div []
                        [ div
                            [ Attr.style
                                [ ( "width", "100px" )
                                , ( "height", "100px" )
                                , ( "margin", "auto" )
                                , ( "border-style", "solid" )
                                , ( "border-color", "black" )
                                ]
                            ]
                            [ svg
                                [ SvgAttr.width "100"
                                , SvgAttr.height "100"
                                , SvgAttr.viewBox "0 0 100 100"
                                ]
                                (selectedHexaSvg 35 piece)
                            ]
                        , br [] []
                        , winLose piece
                        ]

                _ ->
                    span [] []



-------------------------------------------------------------------------------


piecesPatterns =
    --https://stackoverflow.com/questions/29442833/svg-image-inside-circle
    List.map
        (\n ->
            pattern
                [ SvgAttr.id ("piece" ++ toString n)
                , SvgAttr.x "0%"
                , SvgAttr.y "0%"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                , SvgAttr.viewBox "0 0 100 100"
                ]
                [ Svg.image
                    [ SvgAttr.x "10"
                    , SvgAttr.y "10"
                    , SvgAttr.height "80"
                    , SvgAttr.width "80"
                    , SvgAttr.xlinkHref <|
                        "images/pieces/piece"
                            ++ toString n
                            ++ ".png"
                    ]
                    []
                ]
        )
        (List.range 0 14)


playerColor : Int -> String
playerColor playerId =
    --color <| 0.7 * toFloat playerId
    if playerId == 1 then
        "#ff0000"
    else if playerId == 2 then
        "#ff7f00"
    else if playerId == 3 then
        "#ffff00"
    else if playerId == 4 then
        "#00ff00"
    else if playerId == 5 then
        "#0000ff"
    else if playerId == 6 then
        "#b400ff"
    else
        "white"


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


scores : Model -> Html Msg
scores model =
    let
        scoresDict =
            Dict.foldr
                (\k v acc ->
                    case v.state of
                        Contain { value, playerId } ->
                            Dict.update playerId
                                (\mv ->
                                    case mv of
                                        Nothing ->
                                            Just 1

                                        Just v ->
                                            Just (v + 1)
                                )
                                acc

                        _ ->
                            acc
                )
                Dict.empty
                model.board
    in
    table
        [ Attr.style
            [ ( "float", "right" )
            , ( "vertical-align", "top" )
            ]
        ]
        ([ tr []
            [ td [] [ Html.text "playerId" ]
            , td [] [ Html.text "score" ]
            ]
         ]
            ++ Dict.foldr
                (\k v acc ->
                    tr []
                        [ td [ Attr.style [ ( "background-color", color <| 0.7 * toFloat k ) ] ]
                            [ Html.text (toString k) ]
                        , td [] [ Html.text (toString v) ]
                        ]
                        :: acc
                )
                []
                scoresDict
        )


winLose : Piece -> Html Msg
winLose { value, playerId } =
    let
        ( losers, winners ) =
            Dict.foldr
                (\( v1, v2 ) res ( lAcc, wAcc ) ->
                    if v1 /= value || v1 == v2 then
                        ( lAcc, wAcc )
                    else if res > 0 then
                        ( lAcc, ( v2, res ) :: wAcc )
                    else
                        ( ( v2, res ) :: lAcc, wAcc )
                )
                ( [], [] )
                shifumiTable

        makeTd ( v, res ) =
            let
                color =
                    if res == 1 then
                        "lightGreen"
                    else if res == 2 then
                        "green"
                    else if res == -1 then
                        "gold"
                    else if res == -2 then
                        "darkOrange"
                    else
                        "white"
            in
            td
                [ Attr.style
                    [ ( "background-image"
                      , "url('images/pieces/piece"
                            ++ toString v
                            ++ ".png')"
                      )
                    , ( "background-color", color )
                    , ( "width", "50px" )
                    , ( "height", "50px" )
                    , ( "background-size", "contain" )
                    ]
                ]
                []

        losersTd =
            List.map makeTd losers

        winnersTd =
            List.map makeTd winners
    in
    table
        [ Attr.style
            [ ( "margin", "auto" )
            , ( "border-style", "solid" )
            , ( "border-color", "black" )
            ]
        ]
        [ tr []
            (td
                [ Attr.style []
                ]
                [ Html.text "loses to " ]
                :: losersTd
            )
        , tr []
            (td
                [ Attr.style []
                ]
                [ Html.text "wins against " ]
                :: winnersTd
            )
        ]

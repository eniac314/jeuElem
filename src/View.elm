module View exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import Types exposing (..)


view model =
    div [ Attr.style [ ( "width", "100%" ) ] ]
        [ hexaBoardSvg model.boardSize 35 model.board
        , br [] []
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
        ]


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
           --    [ SvgAttr.x (toString <| x - l / 1.5)
           --    , SvgAttr.y (toString <| y)
           --    , SvgAttr.stroke "black"
           --    ]
           --    [ Svg.text ("(" ++ toString u ++ ", " ++ toString v ++ ")") ]
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
                , ( "width", "700px" )
                , ( "height", "650px" )
                ]
            ]
            [ svg
                [ SvgAttr.width "700"
                , SvgAttr.height "700"
                , SvgAttr.viewBox <| "0 0 " ++ size ++ " " ++ size
                ]
                (def :: cells)
            ]

        --, br [] []
        --, Html.text <| "number of cells: " ++ (toString <| List.length cells)
        ]


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
                        "/images/pieces/piece"
                            ++ toString n
                            ++ ".png"
                    ]
                    []
                ]
        )
        (List.range 0 14)


playerColor : Int -> String
playerColor playerId =
    if playerId == 1 then
        "#f0f9e8"
    else if playerId == 2 then
        "#ccebc5"
    else if playerId == 3 then
        "#a8ddb5"
    else if playerId == 4 then
        "#7bccc4"
    else if playerId == 5 then
        "#43a2ca"
    else if playerId == 6 then
        "#0868ac"
    else
        "white"

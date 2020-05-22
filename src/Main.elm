module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress)
import Dict exposing (fromList)
import Html exposing (div, table, td, text, tr)
import Html.Attributes
import Html.Events
import Json.Decode
import List exposing (filter, foldl, map)
import Matrix exposing (Matrix, initialize, set)
import Random
import String exposing (fromInt)
import Tuple exposing (first, second)


type alias Tetromino =
    { id : Int
    , rot : Int
    , pos : ( Int, Int )
    }


type State
    = Falling
    | Landing
    | GameOver


type alias Model =
    { well : Matrix Int
    , pieceCounter : Int
    , score : Int
    , fallingTetromino : Tetromino
    , time : Float
    , state : State
    }


type Msg
    = Tick Float
    | KeyPress String
    | NewTetromino Int
    | NewGame


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscribe
        }


init =
    ( initModel, newTetrominoCmd )


initModel =
    { well = initialize 20 10 (\_ _ -> 0)
    , pieceCounter = 0
    , score = 0
    , fallingTetromino = Tetromino 0 0 ( 0, 0 )
    , time = 0
    , state = Falling
    }


subscribe _ =
    Sub.batch
        [ onKeyPress (Json.Decode.map KeyPress (Json.Decode.field "key" Json.Decode.string))
        , onAnimationFrameDelta Tick
        ]


update msg model =
    case msg of
        Tick t ->
            updateTick { model | time = model.time + t }

        KeyPress k ->
            updateKeyPress k model

        NewTetromino i ->
            updateNewTetromino i model

        NewGame ->
            init


newTetrominoCmd =
    Random.generate NewTetromino (Random.int 1 7)


updateNewTetromino i model =
    -- Put a new piece on the board.
    let
        oldT =
            Tetromino 1 0 ( -1, 4 )

        newT =
            Tetromino i 0 ( 0, 4 )
    in
    if canMove oldT newT model then
        ( { model
            | fallingTetromino = newT
            , pieceCounter = model.pieceCounter + 1
            , well = place (transformShape newT) newT.id model.well
          }
        , Cmd.none
        )

    else
        ( { model | state = GameOver }, Cmd.none )


updateKeyPress key model =
    let
        pc =
            model.fallingTetromino

        character =
            case String.toList key of
                [ c ] ->
                    Char.toLower c

                _ ->
                    '\u{0000}'
    in
    case character of
        'a' ->
            -- counter clockwise
            ( move (pc.rot + 1) pc.pos model, Cmd.none )

        's' ->
            -- clockwise
            ( move (pc.rot + 3) pc.pos model, Cmd.none )

        'j' ->
            -- left
            ( move pc.rot ( first pc.pos, second pc.pos - 1 ) model, Cmd.none )

        'k' ->
            -- right
            ( move pc.rot ( first pc.pos, second pc.pos + 1 ) model, Cmd.none )

        _ ->
            -- ignore everything else
            ( model, Cmd.none )


updateTick model =
    let
        dropPeriod =
            ((0.8 - 0.001 * toFloat (level model)) ^ toFloat (level model)) * 1000.0

        landingTimer =
            500

        oldT =
            model.fallingTetromino

        newT =
            Tetromino oldT.id oldT.rot ( first oldT.pos + 1, second oldT.pos )
    in
    case model.state of
        Falling ->
            if model.time < dropPeriod then
                ( model, Cmd.none )

            else if canMove oldT newT model then
                ( updateT oldT newT { model | time = 0 }, Cmd.none )

            else
                ( { model | time = 0, state = Landing }, Cmd.none )

        Landing ->
            if model.time < landingTimer then
                ( move newT.rot newT.pos model, Cmd.none )

            else
                ( collapse { model | time = 0, state = Falling }, newTetrominoCmd )

        _ ->
            ( model, Cmd.none )


get row col board =
    Maybe.withDefault -1 (Matrix.get row col board)


level model =
    model.pieceCounter // 70


place shape color board =
    foldl (\p a -> set (first p) (second p) color a) board shape


canMove old new model =
    let
        oldT =
            transformShape old

        newT =
            transformShape new

        newSpots =
            filter (\e -> not (List.member e oldT)) newT

        allEmpty spots =
            List.all (\e -> 0 == get (first e) (second e) model.well) spots
    in
    allEmpty newSpots


updateT oldT newT model =
    { model
        | fallingTetromino = newT
        , well =
            model.well
                |> place (transformShape oldT) 0
                |> place (transformShape newT) newT.id
    }


move toRot toPos model =
    let
        newT =
            Tetromino model.fallingTetromino.id toRot toPos
    in
    if canMove model.fallingTetromino newT model then
        updateT model.fallingTetromino newT model

    else
        model


completeRows board =
    -- list of completed rows, bottom-most first
    let
        isCompleteRow n b =
            foldl (\e a -> a && (0 < get n e b)) True (List.range 0 9)
    in
    List.reverse (filter (\e -> isCompleteRow e board) (List.range 0 19))


collapse model =
    let
        board =
            model.well

        copyRow from to start =
            foldl (\e a -> set to e (get from e a) a) start (List.range 0 9)

        zeroRow n b =
            foldl (\e a -> set n e 0 a) b (List.range 0 9)

        pushZeros b =
            foldl (\e a -> zeroRow e a) b (List.range 0 (-1 + List.length (completeRows board)))

        shiftRows fromRow toRow lstFullRows brd =
            if fromRow < 0 then
                pushZeros brd

            else
                case lstFullRows of
                    [] ->
                        shiftRows (fromRow - 1) (toRow - 1) lstFullRows (copyRow fromRow toRow brd)

                    a :: rest ->
                        if fromRow == a then
                            shiftRows (fromRow - 1) toRow rest brd

                        else
                            shiftRows (fromRow - 1) (toRow - 1) lstFullRows (copyRow fromRow toRow brd)
    in
    { model
        | well = shiftRows 19 19 (completeRows board) board
        , score = model.score + ((2 ^ List.length (completeRows board)) - 1) * (level model + 1)
    }


transformShape : Tetromino -> List ( Int, Int )
transformShape t =
    let
        rotate ccw p =
            if 0 < ccw then
                rotate (ccw - 1) (map (\pt -> ( 0 - second pt, first pt )) p)

            else
                p

        translate tr p =
            map (\pt -> ( first tr + first pt, second tr + second pt )) p
    in
    (getPiece t.id).shape
        |> rotate (modBy 4 t.rot)
        |> translate t.pos



--         #     ##   ##    #       #   ##
-- #x##   #x#   #x     x#   #x#   #x#   x#
-- 1 I    2 T   3 S   4 Z   5 J   6 L   7 O
-- light blue, purple, green, red, dark blue, orange, yellow


getPiece n =
    let
        emptypiece =
            { shape = [], color = "white" }

        d =
            fromList
                [ ( 0, emptypiece )
                , ( 1, { shape = [ ( 0, -1 ), ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ], color = "lightblue" } )
                , ( 2, { shape = [ ( 0, -1 ), ( 0, 0 ), ( -1, 0 ), ( 0, 1 ) ], color = "purple" } )
                , ( 3, { shape = [ ( 0, -1 ), ( 0, 0 ), ( -1, 0 ), ( -1, 1 ) ], color = "green" } )
                , ( 4, { shape = [ ( -1, -1 ), ( 0, 0 ), ( -1, 0 ), ( 0, 1 ) ], color = "red" } )
                , ( 5, { shape = [ ( 0, -1 ), ( -1, -1 ), ( 0, 0 ), ( 0, 1 ) ], color = "darkblue" } )
                , ( 6, { shape = [ ( 0, -1 ), ( 0, 0 ), ( 0, 1 ), ( -1, 1 ) ], color = "orange" } )
                , ( 7, { shape = [ ( 0, 0 ), ( -1, 0 ), ( 0, 1 ), ( -1, 1 ) ], color = "yellow" } )
                ]
    in
    Maybe.withDefault emptypiece (Dict.get n d)



-- VIEW


view model =
    div [ Html.Attributes.style "display" "flex" ]
        [ viewScore model
        , div [] [ viewGame model ]
        , viewInstructions
        ]


viewInstructions =
    div
        [ Html.Attributes.style "margin" "5em"
        , Html.Attributes.style "font-family" "monospace"
        ]
        [ div [] [ text "<A> rotate counter-clockwise" ]
        , div [] [ text "<S> rotate clockwise" ]
        , div [] [ text "<J> move left" ]
        , div [] [ text "<K> move right" ]
        , Html.br [] []
        , Html.button [ Html.Events.onClick NewGame ] [ text "New Game" ]
        ]


viewScore model =
    div
        [ Html.Attributes.style "font-size" "30pt"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "margin" "2em"
        ]
        [ div [] [ text "Score" ]
        , div [] [ text (fromInt model.score) ]
        , Html.br [] []
        , div [] [ text "Level" ]
        , div [] [ text (fromInt (level model + 1)) ]
        ]


viewGame model =
    let
        viewSq row col sq =
            let
                color =
                    if model.state == GameOver && sq /= 0 then
                        "lightgrey"

                    else
                        (getPiece sq).color
            in
            td
                [ Html.Attributes.style "border" "1px solid white"
                , Html.Attributes.style "width" "30px"
                , Html.Attributes.style "height" "30px"
                , Html.Attributes.style "background" color
                , Html.Attributes.style "border-radius" "8px"
                ]
                []

        viewRow row =
            tr [] (List.map (\col -> viewSq row col (get row col model.well)) (List.range 0 9))
    in
    table
        [ Html.Attributes.style "border-collapse" "collapse"
        , Html.Attributes.style "margin" "auto"
        , Html.Attributes.style "box-shadow" "5px 5px 3px grey"
        , Html.Attributes.style "border" "5px solid black"
        , Html.Attributes.style "background" "white"
        ]
        (List.map (\e -> viewRow e) (List.range 0 19))

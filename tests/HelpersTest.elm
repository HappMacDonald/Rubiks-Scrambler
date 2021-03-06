module HelpersTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
--import Fuzz

import Rubiks.Constants as C
import Rubiks.Helpers as H

import Array


rubiksScramblerTest : Test
rubiksScramblerTest =
  describe "RubicksScrambler"
  [ describe "defaultScrambles"
    <|let
        values =
          [ (0,-3)
          , (1,3)
          , (2,9)
          , (3,15)
          , (4,21)
          , (5,27)
          ]

        testTemplate (a, output) =
          test
          ( "getLayer "
          ++(toString a)
          ++" == "
          ++(toString output)
          )
          <|\_ ->
              Expect.equal ( H.defaultScrambles a ) output

      in
        List.map testTemplate values
  , describe "toRangedInteger"
    [ test "toRangedInteger \"5\" -10 10 == Ok 5"
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger "5" -10 10 )
          ( Ok 5 )
    , test "toRangedInteger \"5\" 10 -10 == Err \"min 10 must be no greater than max -10.\""
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger "5" 10 -10 )
          ( Err "min 10 must be no greater than max -10." )
    , test "toRangedInteger \"  5\\t\" -10 10 == Ok 5"
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger "  5\t" -10 10 )
          ( Ok 5 )
    , test "toRangedInteger \"5 \" 8 10 == Err \"'5 ' must be no less than 8.\""
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger "5 " 8 10 )
          ( Err "'5 ' must be no less than 8." )
    , test "toRangedInteger \" 5 \" -10 3 == Err \"' 5 ' must be no greater than 3.\""
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger " 5 " -10 3 )
          ( Err "' 5 ' must be no greater than 3." )
    , test "toRangedInteger \"\" -10 10 == Ok 0"
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger "" -10 10 )
          ( Ok 0 )
    , test "toRangedInteger \"\\t \\t\" -10 10 == Ok 0"
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger "\t \t" -10 10 )
          ( Ok 0 )
    , test "toRangedInteger \"\" 5 10 == Err \"'' must be no less than 5.\""
      <|\_ ->
          Expect.equal
          ( H.toRangedInteger "" 5 10 )
          ( Err "'' must be no less than 5." )
    ]
  , describe "renderMoves"
    [ test "full suite listed in module doc"
      <|\_ ->
          let
            moves =
            [ { axis = 1 -- second available layer of 2
              , layer = 4 -- "23"
              , twistDegrees = 1 -- 90 degrees
              }
            , { axis = 1 -- second available layer of 2
              , layer = 2 -- "2"
              , twistDegrees = 2 -- 180 degrees
              }
            , { axis = 0 -- first available layer of 2
              , layer = 9 -- "34"
              , twistDegrees = 3 -- 270 degrees
              }
            , { axis = -1 -- invalid
              , layer = 9
              , twistDegrees = 3
              }
            , { axis = 0
              , layer = -1 -- invalid
              , twistDegrees = 3
              }
            , { axis = 0
              , layer = 9
              , twistDegrees = 4 -- invalid
              }
            ]
          in
            Expect.equal
            ( H.renderMoves 4 0 moves )
            [ "R23(90)" -- 0 taken, second available is 2="D"
            , "U12(180)" -- 2 taken, second available is 1="U"
            , "F34(270)" -- 1 taken, first available is 0="F"
            , "!!34(270)"
            , "U!!(270)" -- "-1 taken" technically has undefined behavior..
            , "F34(!!)" -- 1 taken, first available = 0="F"
            ]
    ]
  , describe "renderOrientation"
    [ test "renderOrientation (0, 0) == Orientation 0 1 2"
      <|\_ ->
          Expect.equal (H.renderOrientation (0, 0) ) ( C.Orientation 0 1 2)
    , test "renderOrientation (5, 2) == Orientation 5 4 3"
      <|\_ ->
          Expect.equal (H.renderOrientation (5, 2) ) ( C.Orientation 5 4 3)
    ]
  , describe "defaultCubeLayout"
    [ test "2x2x2"
      <|\_ ->
          Expect.equal
            ( H.defaultCubeLayout 2 )
            <|Array.fromList
              [ Array.fromList
                [ Array.fromList [ 0, 0 ]
                , Array.fromList [ 0, 0 ]
                ]
              , Array.fromList
                [ Array.fromList [ 1, 1 ]
                , Array.fromList [ 1, 1 ]
                ]
              , Array.fromList
                [ Array.fromList [ 2, 2 ]
                , Array.fromList [ 2, 2 ]
                ]
              , Array.fromList
                [ Array.fromList [ 3, 3 ]
                , Array.fromList [ 3, 3 ]
                ]
              , Array.fromList
                [ Array.fromList [ 4, 4 ]
                , Array.fromList [ 4, 4 ]
                ]
              , Array.fromList
                [ Array.fromList [ 5, 5 ]
                , Array.fromList [ 5, 5 ]
                ]
              ]
    ]
    , describe "blankFaceLayout"
      [ test "4x4x4"
        <|\_ ->
            Expect.equal
            ( H.blankFaceLayout 4 )
            <|Array.fromList
              [ Array.fromList [ 6, 6, 6, 6 ]
              , Array.fromList [ 6, 6, 6, 6 ]
              , Array.fromList [ 6, 6, 6, 6 ]
              , Array.fromList [ 6, 6, 6, 6 ]
              ]
      ]
    , describe "solidFaceLayout"
      [ test "3x3 yellow"
        <|\_ ->
            Expect.equal
            ( H.solidFaceLayout 3 2 ) -- 2 == yellow
            <|Array.fromList
              [ Array.fromList [ 2, 2, 2 ]
              , Array.fromList [ 2, 2, 2 ]
              , Array.fromList [ 2, 2, 2 ]
              ]
      ]
    , describe "orientedCubeLayout"
      [ test "1x1x1 front=orange, up=blue, right=white (Orientation 5 1 4)"
        <|\_ ->
            Expect.equal
            ( C.Orientation 5 1 4
            |>H.orientedCubeLayout 1
            )
            <|Array.fromList
              [ Array.fromList [ Array.fromList [ 1 ] ]
              , Array.fromList [ Array.fromList [ 2 ] ]
              , Array.fromList [ Array.fromList [ 5 ] ]
              , Array.fromList [ Array.fromList [ 4 ] ]
              , Array.fromList [ Array.fromList [ 0 ] ]
              , Array.fromList [ Array.fromList [ 3 ] ]
              ]
      ]
  ]
module RubicksScramblerTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
--import Fuzz

import RubiksScrambler as R

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
              Expect.equal ( R.defaultScrambles a ) output

      in
        List.map testTemplate values
  , describe "toRangedInteger"
    [ test "toRangedInteger \"5\" -10 10 == Ok 5"
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger "5" -10 10 )
          ( Ok 5 )
    , test "toRangedInteger \"5\" 10 -10 == Err \"min 10 must be no greater than max -10.\""
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger "5" 10 -10 )
          ( Err "min 10 must be no greater than max -10." )
    , test "toRangedInteger \"  5\\t\" -10 10 == Ok 5"
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger "  5\t" -10 10 )
          ( Ok 5 )
    , test "toRangedInteger \"5 \" 8 10 == Err \"'5 ' must be no less than 8.\""
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger "5 " 8 10 )
          ( Err "'5 ' must be no less than 8." )
    , test "toRangedInteger \" 5 \" -10 3 == Err \"' 5 ' must be no greater than 3.\""
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger " 5 " -10 3 )
          ( Err "' 5 ' must be no greater than 3." )
    , test "toRangedInteger \"\" -10 10 == Ok 0"
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger "" -10 10 )
          ( Ok 0 )
    , test "toRangedInteger \"\\t \\t\" -10 10 == Ok 0"
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger "\t \t" -10 10 )
          ( Ok 0 )
    , test "toRangedInteger \"\" 5 10 == Err \"'' must be no less than 5.\""
      <|\_ ->
          Expect.equal
          ( R.toRangedInteger "" 5 10 )
          ( Err "'' must be no less than 5." )
    ]
  , describe "renderMoves"
    [ test "full suite listed in module doc"
      <|\_ ->
          let
            moves =
            [ { axis = 1 -- second available layer of 2
              , layer = 4 -- "23"
              , twistDegrees = 0 -- 90 degrees
              }
            , { axis = 1 -- second available layer of 2
              , layer = 2 -- "2"
              , twistDegrees = 1 -- 180 degrees
              }
            , { axis = 0 -- first available layer of 2
              , layer = 9 -- "34"
              , twistDegrees = 2 -- 270 degrees
              }
            , { axis = -1 -- invalid
              , layer = 9
              , twistDegrees = 2
              }
            , { axis = 0
              , layer = -1 -- invalid
              , twistDegrees = 2
              }
            , { axis = 0
              , layer = 9
              , twistDegrees = 3 -- invalid
              }
            ]
          in
            Expect.equal
            ( R.renderMoves 4 0 moves )
            [ "R23(90)" -- 0 taken, second available is 2="D"
            , "U12(180)" -- 2 taken, second available is 1="U"
            , "F34(270)" -- 1 taken, first available is 0="F"
            , "!!34(270)"
            , "U!!(270)" -- "-1 taken" technically has undefined behavior..
            , "F34(!!)" -- 1 taken, first available = 0="F"
            ]
    ]
  ]
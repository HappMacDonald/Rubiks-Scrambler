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
  ]
module RubiksLayersTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
--import Fuzz

import Rubiks.Layers as L

rubiksLayersTest : Test
rubiksLayersTest =
  describe "RubiksLayers"
  [ describe "numberOfLayers" -- Apparently this is OEIS sequence A161206
    <|let
        values =
          [ (0,0)
          , (1,1)
          , (2,3)
          , (3,7)
          , (4,13)
          , (5,21)
          , (6,31)
          , (7,43)
          , (8,57)
          ]

        testTemplate (input, output) =
          test
          ( "numberOfLayers "
          ++(toString input)
          ++" == "
          ++(toString output)
          )
          <|\_ ->
              Expect.equal ( L.numberOfLayers input ) output
      in
        List.map testTemplate values
  , describe "getLayer"
    <|let
        values =
          [ (3,0,Just "1")
          , (5,16,Just "1245")
          , (3,100,Nothing)
          , (4,3,Just "3")
          ]

        testTemplate (a, b, output) =
          test
          ( "getLayer "
          ++(toString a)
          ++" "
          ++(toString b)
          ++" == "
          ++(toString output)
          )
          <|\_ ->
              Expect.equal ( L.getLayer a b ) output
      in
        List.map testTemplate values
  ]
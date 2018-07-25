module CubeFaceLayoutTest exposing (cubeFaceLayoutTest)

import Expect exposing (Expectation)
import Test exposing (..)
--import Fuzz

import Array

import Rubiks.CubeFaceLayout as CFL


cubeFaceLayoutTest : Test
cubeFaceLayoutTest =
  describe "cubeFaceLayoutTest"
  [ describe "solidFaceLayout -- ?"
    [ test "3x3 yellow"
      <|\_ ->
          Expect.equal
          ( CFL.solidFaceLayout 3 2 ) -- 2 == yellow
          <|?
    ]
  ]
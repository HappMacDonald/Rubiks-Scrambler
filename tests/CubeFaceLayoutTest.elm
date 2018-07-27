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
  , describe "rowFromTop"
    [ test "no write"
      <|\_ ->
          let
            face0 =
              solidFaceLayout 3 4

            (row0, face1) =
              rowFromTop 0 ( Just [1,2,3] ) face0

          in
            Expect.equal
              (  )
              ( CubeRowLayout [4,4,4]
              , CubeFaceLayout
                { cubeSize = 3
                , data =
                  [ CubeRowLayout [1,2,3]
                  , CubeRowLayout [4,4,4]
                  , CubeRowLayout [4,4,4]
                  ]
                }
              )
    , test "read only"
      <|\_ ->
          let
            face0 =
              solidFaceLayout 3 4

            (prevRow, nextFace) =
              rowFromTop 0 ( Just [1,2,3] ) face0
            
            ()
          in
            Expect.equal
              ( rowFromTop 0 Nothing face0 )
              ( CubeRowLayout [4,4,4], face0 )


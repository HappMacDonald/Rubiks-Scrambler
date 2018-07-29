module CubeFaceLayoutTest exposing (cubeFaceLayoutTest)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
--import Fuzz

import Array

import Rubiks.CubeFaceLayout as CFL


testSetup : RowManipulator -> String -> Test
testSetup rowManipulator label faceTest =
  let
    face0 =
      solidFaceLayout 3 4

    row0 =
      cubeRowLayout [4,4,4]

    rowNew = 
      cubeRowLayout [1,2,3]

    (row1, face1) =
      rowManipulator 0 ( Just rowNew ) face0

    (row2, face2) =
      rowManipulator 0 Nothing face1

  in
    describe label
    [ test "row0 == row1"
      <|\_ ->
          Expect.equal row0 row1

    , test "face0 /= face1"
      <|\_ ->
          Expect.notEqual face0 face1

    , test "rowNew == row2"
      <|\_ ->
          Expect.equal rowNew row2

    , test "face1 == face2"
      <|\_ ->
          Expect.equal face1 face2
          
    , test "face1 == faceTest"
      <|\_ ->
          Expect.equal face1 faceTest

    ]



cubeFaceLayoutTest : Test
cubeFaceLayoutTest =
  describe "cubeFaceLayoutTest"
  [ testSetup rowFromTop "rowFromTop"
      cubeFaceLayout
      ( cubeRowLayout [1,2,3]
      , cubeRowLayout [4,4,4]
      , cubeRowLayout [4,4,4]
      )
  , testSetup rowFromBottom "rowFromBottom"
      cubeFaceLayout
      ( cubeRowLayout [4,4,4]
      , cubeRowLayout [4,4,4]
      , cubeRowLayout [1,2,3]
      )
  , testSetup colFromLeft "colFromLeft"
      cubeFaceLayout
      ( cubeRowLayout [1,4,4]
      , cubeRowLayout [2,4,4]
      , cubeRowLayout [3,4,4]
      )
  , testSetup colFromRight "colFromRight"
      cubeFaceLayout
      ( cubeRowLayout [4,4,1]
      , cubeRowLayout [4,4,2]
      , cubeRowLayout [4,4,3]
      )
  ]

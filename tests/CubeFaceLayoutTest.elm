module CubeFaceLayoutTest exposing (cubeFaceLayoutTest)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
--import Fuzz

--import Array

import Rubiks.CubeFaceLayout as CFL


testSetup : CFL.RowManipulator -> String -> CFL.CubeFaceLayout -> Test
testSetup rowManipulator label faceTest =
  let
    face0 =
      CFL.solidFaceLayout 3 <| Just 4

    row0 =
      CFL.cubeRowLayout [4,4,4]

    rowNew = 
      CFL.cubeRowLayout [1,2,3]

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
  [ testSetup CFL.rowFromTop "rowFromTop"
      ( CFL.cubeFaceLayout
        [ CFL.cubeRowLayout [1,2,3]
        , CFL.cubeRowLayout [4,4,4]
        , CFL.cubeRowLayout [4,4,4]
        ]
        |> Result.withDefault ( CFL.blankFaceLayout 0 )
      )
  , testSetup CFL.rowFromBottom "rowFromBottom"
      ( CFL.cubeFaceLayout
        [ CFL.cubeRowLayout [4,4,4]
        , CFL.cubeRowLayout [4,4,4]
        , CFL.cubeRowLayout [1,2,3]
        ]
        |> Result.withDefault ( CFL.blankFaceLayout 0 )
      )
  , testSetup CFL.colFromLeft "colFromLeft"
      ( CFL.cubeFaceLayout
        [ CFL.cubeRowLayout [1,4,4]
        , CFL.cubeRowLayout [2,4,4]
        , CFL.cubeRowLayout [3,4,4]
        ]
        |> Result.withDefault ( CFL.blankFaceLayout 0 )
      )
  , testSetup CFL.colFromRight "colFromRight"
      ( CFL.cubeFaceLayout
        [ CFL.cubeRowLayout [4,4,1]
        , CFL.cubeRowLayout [4,4,2]
        , CFL.cubeRowLayout [4,4,3]
        ]
        |> Result.withDefault ( CFL.blankFaceLayout 0 )
      )
  ]

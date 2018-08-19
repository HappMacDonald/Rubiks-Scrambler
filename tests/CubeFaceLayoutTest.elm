module CubeFaceLayoutTest exposing (cubeFaceLayoutTest)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
--import Fuzz

--import Array

import Rubiks.CubeFaceLayout as CFL
import Rubiks.Cell as Cell


cubeFaceLayoutTest : Test
cubeFaceLayoutTest =
  describe "cubeFaceLayoutTest"
  [ describe "cubeFaceLayout"
    [ test "cubeFaceLayout [[1,2],[3,4]] /= Nothing"
        (\_->
          Expect.notEqual
            ( CFL.cubeFaceLayout [[1,2],[3,4]] )
            Nothing
        )
    , test "cubeFaceLayout [] == Nothing"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [] )
            Nothing
        )
    , test "cubeFaceLayout [[1,2],[3]] == Nothing"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [[1,2],[3]] )
            Nothing
        )
    , test "cubeFaceLayout [[1,2],[-3,4]] == Nothing"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [[1,2],[-3,4]] )
            Nothing
        )
    ]
  
  , describe "cubeSize"
    [ test "cubeFaceLayout [[1,2],[3,4]] |> cubeSize == 2"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [[1,2],[3,4]] |> Maybe.map CFL.cubeSize )
            ( Just 2 )
        )
    ]

  , describe "solidFaceLayout"
    [ test "solidFaceLayout 2 4 /= Nothing"
        (\_->
          Expect.notEqual
            ( CFL.solidFaceLayout 2 4 )
            Nothing
        )
    , test "solidFaceLayout 2 4 == cubeFaceLayout [[4,4], [4,4]]"
        (\_->
          Expect.equal
            ( CFL.solidFaceLayout 2 4 )
            ( CFL.cubeFaceLayout [[4,4],[4,4]] )
        )
    , test "solidFaceLayout 2 8 == Nothing"
        (\_->
          Expect.equal
            ( CFL.solidFaceLayout 2 8 )
            Nothing
        )
    , test "solidFaceLayout 0 4 == Nothing"
        (\_->
          Expect.equal
            ( CFL.solidFaceLayout 0 4 )
            Nothing
        )
    ]

  , describe "blankFaceLayout"
    [ test "blankFaceLayout 2 /= Nothing"
        (\_->
          Expect.notEqual
            ( CFL.blankFaceLayout 2 )
            Nothing
        )
    , test "blankFaceLayout 0 == Nothing"
        (\_->
          Expect.equal
            ( CFL.blankFaceLayout 0 )
            Nothing
        )
    ]

  , describe "cellAt"
    [ test
        ( "solidFaceLayout 2 4 |> Maybe.map ( CFL.cellAt 0 0 ) /= Nothing -- "
        ++"EG, the call to solidFaceLayout didn't spit out a Nothing."
        )
        (\_->
          Expect.notEqual
            ( CFL.solidFaceLayout 2 4 |>Maybe.map ( CFL.cellAt 0 0 ) )
            Nothing
        )
    , test
        ( "solidFaceLayout 2 4 |> Maybe.map ( CFL.cellAt 0 0 )"
        ++" /= Just Nothing"
        ++"EG, the call to CFL.cellAt didn't spit out a Nothing, either."
        )
        (\_->
          Expect.notEqual
            ( CFL.solidFaceLayout 2 4 |>Maybe.map ( CFL.cellAt 0 0 ) )
            ( Just Nothing )
        )
    , test
        ( "solidFaceLayout 2 4 |> Maybe.map ( CFL.cellAt 0 0 )"
        ++" == Just ( Cell.colorCell 4 )"
        )
        (\_->
          Expect.equal
            ( CFL.solidFaceLayout 2 4 |>Maybe.map ( CFL.cellAt 0 0 ) )
            ( Just ( Cell.colorCell 4 ) )
        )
    , test
        ( "blankFaceLayout 2 |> Maybe.map ( CFL.cellAt 1 1 )"
        ++" == Just BlankCell "
        )
        (\_->
          Expect.equal
            ( CFL.blankFaceLayout 2 |> Maybe.map ( CFL.cellAt 1 1 ))
            ( Just ( Just Cell.BlankCell ) )
        )
    , test
        ( "solidFaceLayout 2 4 |> Maybe.map ( CFL.cellAt 2 1 )"
        ++" == Just Nothing"
        ++" -- EG, the call to solidFaceLayout succeeds"
        ++", but the call to cellAt fails on out of bounds."
        )
        (\_->
          Expect.equal
            ( CFL.blankFaceLayout 2 |> Maybe.map ( CFL.cellAt 2 1 ))
            ( Just Nothing )
        )
    ]

  , let
      a = 3
    in
      describe "rowFromTop"
      [ test "?"
          (\_ ->
            Expect.equal
              (a)
              3
          )
      ]
  ]

module CubeFaceLayoutTest exposing (cubeFaceLayoutTest)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
import Debug
--import Fuzz

--import Array

-- import MyBasics
import Rubiks.CubeFaceLayout as CFL
import Rubiks.CubeRowLayout as CRL
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

  , describe "rowFromTop"
    ( let
        (row0, face0) =
          ( CRL.cubeRowLayout [1,2,3]
          , CFL.solidFaceLayout 3 4
          )

        maybeRow1Face1 =
          Maybe.andThen (CFL.rowFromTop 0 row0) face0

        maybeRow2Face2 =
          Maybe.andThen
            (\(_, face1) ->
              CFL.rowFromTop 0 Nothing face1
            )
            maybeRow1Face1

      in
        Maybe.map4
          (\row0 face0 (row1, face1) (row2, face2) ->
            [ test "row1 == CRL.cubeRowLayout [4,4,4] -- data read from face0"
                (\_ ->
                  Expect.equal
                    ( Just row1 )
                    <|CRL.cubeRowLayout [4,4,4]
                )
            , test
                ( "face1 == cubeFaceLayout [[1,2,3], [4,4,4], [4,4,4]]"
                ++" -- changed results of face"
                )
                (\_ ->
                  Expect.equal
                    ( Just face1 )
                    <|CFL.cubeFaceLayout [[1,2,3], [4,4,4], [4,4,4]]
                )
            , test "row2 == row0 -- row we wrote in was read back out"
                (\_ ->
                  Expect.equal row2 row0
                )
            , test "face2 == face1 -- second operation did not write to face"
                (\_ ->
                  Expect.equal face2 face1
                )
            
            ]
          )
          row0
          face0
          maybeRow1Face1
          maybeRow2Face2
        |>Maybe.withDefault
          [ test "Serious input operator mishap! D:"
              (\_ -> Expect.fail "Serious input operator mishap! D:" )
          ]
    )
    
  , describe "rowFromBottom"
    ( let
        (row0, face0) =
          ( CRL.cubeRowLayout [1,2,3]
          , CFL.solidFaceLayout 3 4
          )

        maybeRow1Face1 =
          Maybe.andThen (CFL.rowFromBottom 0 row0) face0

        maybeRow2Face2 =
          Maybe.andThen
            (\(_, face1) ->
              CFL.rowFromBottom 0 Nothing face1
            )
            maybeRow1Face1

      in
        Maybe.map4
          (\row0 face0 (row1, face1) (row2, face2) ->
            [ test "row1 == CRL.cubeRowLayout [4,4,4] -- data read from face0"
                (\_ ->
                  Expect.equal
                    ( Just row1 )
                    <|CRL.cubeRowLayout [4,4,4]
                )
            , test
                ( "face1 == cubeFaceLayout [[4,4,4], [4,4,4], [1,2,3]]"
                ++" -- changed results of face"
                )
                (\_ ->
                  Expect.equal
                    ( Just face1 )
                    <|CFL.cubeFaceLayout [[4,4,4], [4,4,4], [1,2,3]]
                )
            , test "row2 == row0 -- row we wrote in was read back out"
                (\_ ->
                  Expect.equal row2 row0
                )
            , test "face2 == face1 -- second operation did not write to face"
                (\_ ->
                  Expect.equal face2 face1
                )
            
            ]
          )
          row0
          face0
          maybeRow1Face1
          maybeRow2Face2
        |>Maybe.withDefault
          [ test "Serious input operator mishap! D:"
              (\_ -> Expect.fail "Serious input operator mishap! D:" )
          ]
    )
    
  , describe "colFromLeft"
    ( let
        (row0, face0) =
          ( CRL.cubeRowLayout [1,2,3]
          , CFL.solidFaceLayout 3 4
          )

        maybeRow1Face1 =
          Maybe.andThen (CFL.colFromLeft 0 row0) face0

        maybeRow2Face2 =
          Maybe.andThen
            (\(_, face1) ->
              CFL.colFromLeft 0 Nothing face1
            )
            maybeRow1Face1

      in
        Maybe.map4
          (\row0 face0 (row1, face1) (row2, face2) ->
            [ test "row1 == CRL.cubeRowLayout [4,4,4] -- data read from face0"
                (\_ ->
                  Expect.equal
                    ( Just row1 )
                    <|CRL.cubeRowLayout [4,4,4]
                )
            , test
                ( "face1 == cubeFaceLayout [[1,4,4], [2,4,4], [3,4,4]]"
                ++" -- changed results of face"
                )
                (\_ ->
                  Expect.equal
                    ( Just face1 )
                    <|CFL.cubeFaceLayout [[1,4,4], [2,4,4], [3,4,4]]
                )
            , test "row2 == row0 -- row we wrote in was read back out"
                (\_ ->
                  Expect.equal row2 row0
                )
            , test "face2 == face1 -- second operation did not write to face"
                (\_ ->
                  Expect.equal face2 face1
                )
            
            ]
          )
          row0
          face0
          maybeRow1Face1
          maybeRow2Face2
        |>Maybe.withDefault
          [ test "Serious input operator mishap! D:"
              (\_ -> Expect.fail "Serious input operator mishap! D:" )
          ]
    )
    
  -- , describe "rotate90"
  --   [ test ""
  --       (\_->
  --         Expect.equal
  --           ( CFL.? )
  --           ( CFL.? )
  --       )
  --   ]

  ]

module CubeRowLayoutTest exposing (cubeRowLayoutTest)


import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
--import Fuzz
--import Array


import Rubiks.CubeRowLayout as CRL
import Rubiks.Cell as Cell exposing (Cell)


cubeRowLayoutTest : Test
cubeRowLayoutTest =
  describe "cubeRowLayoutModule"
  [ describe "cubeRowLayout"
    [ test "cubeRowLayout [1,2,3] != Nothing"
        <|\_ ->
            Expect.notEqual
              ( CRL.cubeRowLayout [1,2,3] )
              Nothing
    , test "cubeRowLayout [1,2,9] == Nothing"
        <|\_ ->
            Expect.equal
              ( CRL.cubeRowLayout [1,2,9] )
              Nothing
    , test "cubeRowLayout [] == ???"
        <|\_ ->
            Expect.equal
              ( CRL.cubeRowLayout [] )
              Nothing
    ]

  , describe "solidRowLayout"
    [ test "solidRowLayout 3 5 != Nothing"
        <|\_ ->
            Expect.notEqual
              ( CRL.solidRowLayout 3 5 )
              Nothing
    , test "solidRowLayout 3 5 == cubeRowLayout [5,5,5]"
        <|\_ ->
            Expect.equal
              ( CRL.solidRowLayout 3 5 )
              ( CRL.cubeRowLayout [5,5,5] )
    , test "solidRowLayout 3 -1 == Nothing"
        <|\_ ->
            Expect.equal
              ( CRL.solidRowLayout 3 -1 )
              Nothing
    ]

  , describe "blankRowLayout"
    [ test "blankRowLayout 3 != Nothing"
        <|\_ ->
            Expect.notEqual
              ( CRL.blankRowLayout 3 )
              Nothing
    , test "blankRowLayout 0 == Nothing"
        <|\_ ->
            Expect.equal
              ( CRL.blankRowLayout 0 )
              Nothing
    ]

  , describe "cellAt"
    [ test "CubeRowLayout A[1,2,3] |> cellAt 1 == Just ColorCell 2"
        <|\_ ->
            Expect.equal
              ( [1,2,3] |> CRL.cubeRowLayout |> Maybe.map (CRL.cellAt 1) )
              ( Just <| Cell.colorCell 2 )
    , test "CubeRowLayout A[1,2,3] |> cellAt 1 != Nothing"
        <|\_ ->
            Expect.notEqual
              ( [1,2,3] |> CRL.cubeRowLayout |> Maybe.map (CRL.cellAt 1) )
              Nothing
    , test "CubeRowLayout A[1,2,3] |> cellAt 10 == Nothing"
        <|\_ ->
            Expect.equal
              ( [1,2,3] |> CRL.cubeRowLayout |> Maybe.map (CRL.cellAt 10) )
              ( Just Nothing )
    , test "CubeRowLayout A[BlankCell] |> cellAt 0 == Just BlankCell"
        <|\_ ->
            Expect.equal
              ( CRL.blankRowLayout 1 |> Maybe.map (CRL.cellAt 0) )
              ( Just <| Just Cell.BlankCell )
    , test "CubeRowLayout A[1,1,1] |> cellAt 1 == Just ColorCell 1"
        <|\_ ->
            Expect.equal
              ( CRL.solidRowLayout 3 1 |> Maybe.map (CRL.cellAt 1) )
              ( Just <| Cell.colorCell 1 )
    ]

  , describe "length"
    [ test "CubeRowLayout A[1,1,1] |> length == 3"
        <|\_ ->
            Expect.equal
              ( CRL.solidRowLayout 3 1
              |>Maybe.map CRL.length
              |>Maybe.withDefault -999
              )
              3
    ]
  ]

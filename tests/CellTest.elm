module CellTest exposing (cellTest)


import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
--import Fuzz
--import Array


import Rubiks.Cell as Cell


cellTest : Test
cellTest =
  describe "cell"
  [ describe "colorCell"
    [
      test "colorCell 3 /= Nothing (Because I can't peer deeper into it :P)"
        <|\_ ->
            Expect.notEqual
              ( Cell.colorCell 3)
              Nothing
    , test "colorCell 6 == Nothing"
        <|\_ ->
            Expect.equal
              ( Cell.colorCell 6 )
              Nothing
    , test "colorCell -1 == Nothing"
        <|\_ ->
            Expect.equal
              ( Cell.colorCell -1 )
              Nothing
    ]
  , describe "cellColor"
    [ test "colorCell 3 |> Maybe.withDefault BlankCell |> cellColor == Just 3"
        <|\_ ->
            Expect.equal
              ( Cell.colorCell 3 |> Maybe.withDefault Cell.BlankCell |> Cell.cellColor )
              ( Just 3 )
    , test "BlankCell |> cellColor == Nothing"
        <|\_ ->
            Expect.equal
              ( Cell.BlankCell |> Cell.cellColor )
              Nothing
    ]
  ]

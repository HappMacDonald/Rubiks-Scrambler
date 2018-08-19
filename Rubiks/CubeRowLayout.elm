module Rubiks.CubeRowLayout exposing
  ( CubeRowLayout
  , cubeRowLayout
  , solidRowLayout
  , blankRowLayout
  , cellAt
  , length
  , flipRowLayout
  )


import Array exposing (Array)
import Maybe.Extra


import Rubiks.Cell as Cell exposing (Cell)


type CubeRowLayout
  = CubeRowLayout (Array Cell)


{-|Converts a list of Ints (representing colors) into a Just CubeRowLayout.
Returns Nothing if any of the Ints are out of bounds.

    cubeRowLayout [1,2,3] == those three cell colors in a row
-}

cubeRowLayout : List Int -> Maybe CubeRowLayout
cubeRowLayout ints =
  case Maybe.Extra.traverse Cell.colorCell ints of
    Nothing ->
      Nothing

    Just cells ->
      case cells of
        [] ->
          Nothing

        cells ->
          Array.fromList cells
          |>CubeRowLayout
          |>Just

{-|Constructs a row that is a solid color.

    solidRowLayout 3 5 == Just (3 orange cells in a row)
    solidRowLayout 3 10 == Nothing
    solidRowLayout 3 -2 == Nothing
    solidRowLayout 0 5 == Nothing
-}

solidRowLayout : Int -> Int -> Maybe CubeRowLayout
solidRowLayout cubeSize color =
  List.repeat cubeSize color
  |>cubeRowLayout


{-|Constructs a row full of BlankCell's

    blankRowLayout 5 == Just (a row of 5 blank cells)
    blankRowLayout 0 == blankRowLayout -7 == Nothing
-}

blankRowLayout : Int -> Maybe CubeRowLayout
blankRowLayout cubeSize =
  if cubeSize<1 then
    Nothing
  else
    List.repeat cubeSize Cell.BlankCell
    |>Array.fromList
    |>CubeRowLayout
    |>Just


{-|Returns the (Maybe Cell) corresponding with the position in the row indicated
Returns Nothing if position indicated is out of bounds

    CubeRowLayout [1,2,3] |> cellAt 1 == Just ColorCell 2
    CubeRowLayout [1,2,3] |> cellAt 10 == Nothing
-}

cellAt : Int -> CubeRowLayout -> Maybe Cell
cellAt position (CubeRowLayout row) =
  Array.get position row


{-|Tells how long the CubeRowLayout object is in cells.

    CubeRowLayout A[1,1,1] |> length == 3
-}

length : CubeRowLayout -> Int
length ( CubeRowLayout row ) =
  Array.length row


flipRowLayout : CubeRowLayout -> CubeRowLayout
flipRowLayout (CubeRowLayout row) =
  row
  |> Array.toList
  |> List.reverse
  |> Array.fromList
  |> CubeRowLayout 

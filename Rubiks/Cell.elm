module Rubiks.Cell exposing
  ( Cell(BlankCell)
  , colorCell
  , cellColor
  )


import Array exposing (Array)


type Cell
  = ColorCell Int
  | BlankCell


type CubeRowLayout
  = CubeRowLayout (Array (Maybe Int))


{-| Constructor for a cell with a given color
The input argument SHOULD be in the range 0..5, if it's not then
you'll get a Maybe.Nothing.

    colorCell 3 == Just <| ColorCell 3
    colorCell 7 == colorCell -5 == Nothing
-}

colorCell : Int -> Maybe Cell
colorCell color =
  if(color >= 0 && color < 6) then
    Just <| ColorCell color
  else
    Nothing


{-| Returns which color a given Cell is.
BlankCell returns Nothing.

    colorCell (cellColor 3) == Just 3
    colorCell BlankCell == Nothing
-}

cellColor : Cell -> Maybe Int
cellColor cell =
  case cell of
    ColorCell color ->
      Just color
    
    BlankCell ->
      Nothing



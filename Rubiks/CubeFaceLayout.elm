module Rubiks.CubeFaceLayout exposing
  ( CubeFaceLayout
  , CubeRowLayout
  , solidFaceLayout
  )


import Rubiks.Constants

import Array exposing (Array)


type CubeRow =
  CubeRow Array (Maybe Int)


type CubeRowLayout =
  CubeRowLayout
  { cubeSize : Int
  , data : Array CubeRow
  }


{-|Constructs a row that is a solid color.

    solidRowLayout 3 <| Just 5 == 3 orange cells in a row
-}

solidRowLayout : Int -> Maybe Int -> CubeRowLayout
solidRowLayout cubeSize color =
  Array.fromList <| List.repeat cubeSize color


{-|Constructs a face that is a solid color, the one provided.
"color" is expressed as a Maybe Int.
0-6 are valid cube colors as defined in Rubicks.Constants.
Nothing is the blank color.
Nothing renders to transparent in graphics, and is used as
a logical transparency in overpainting operations.

    solidFaceLayout 4 2 == a 4x4 CubeFaceLayout that is yellow everywhere.
-}
solidFaceLayout : Int -> Maybe Int -> CubeFaceLayout
solidFaceLayout cubeSize color =
  CubeFaceLayout
  { cubeSize = cubeSize
  , data =
      Array.fromList
      <|List.repeat cubeSize -- cubeSize full rows
      <|solidRowLayout cubeSize color -- cubeSize cells per row
  }


{-|Create an empty row of given size.

    blankRowLayout 3 == 3 empty (colorless) cells in a row.
-}

blankRowLayout : Int -> CubeRowLayout
blankRowLayout cubesize =
  solidRowLayout cubesize Nothing


{-|Create an empty face of the given size.

    blankFaceLayout 3 == an empty (colorless) 3x3 face.
-}

blankFaceLayout : Int -> CubeFaceLayout
blankFaceLayout cubeSize =
  solidFaceLayout cubeSize Nothing


{-|Opaque getter token. Used to address a row counting from the top.

    fromTop 2 <| solidFaceLayout 3 4 == CubeRowLayout [4,4,4]
-}

fromTop : Int -> CubeFaceLayout -> CubeRowLayout
fromTop row (CubeFaceLayout {data, cubeSize}) =
  data
  |>Array.get row
  |>Maybe.withDefault (blankRowLayout cubeSize)


{-|Opaque getter token. Used to address a row counting from the bottom.

    fromBottom 2 <| solidFaceLayout 3 4 == CubeRowLayout [4,4,4]
-}

fromBottom : Int -> CubeFaceLayout -> CubeRowLayout
fromBottom row (CubeFaceLayout {cubeSize}) as face =
  fromTop (cubeSize-row) face


{-|Opaque getter token. Used to address a row counting from the left.

    fromLeft 2 <| solidFaceLayout 3 4 == CubeRowLayout [4,4,4]
-}

fromLeft : Int -> CubeFaceLayout -> CubeRowLayout
fromLeft col (CubeFaceLayout {data, cubeSize}) =
  data
  |>Array.map
    ( \row ->
        Array.get row col
        |>Maybe.withDefault Nothing
    )


{-|Opaque getter token. Used to address a row counting from the right.

    fromRight 2 <| solidFaceLayout 3 4 == CubeRowLayout [4,4,4]
-}

fromLeft : Int -> CubeFaceLayout -> CubeRowLayout
fromLeft col (CubeFaceLayout {cubeSize}) as face =
  fromLeft (cubeSize-row) face



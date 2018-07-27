module Rubiks.CubeFaceLayout exposing
  ( CubeFaceLayout
  , CubeRowLayout
  , solidFaceLayout
  , rowFromTop
  , rowFromBottom
  , colFromLeft
  , colFromRight
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


{-|Getter/setter function. Used to address a row counting from the top.

    --setup starting condition
    face0 = solidFaceLayout 3 4

    -- write and read
    (row0, face1) = rowFromTop 0 ( Just [1,2,3] ) face0

    -- read only from previously written state
    (row1, _) = rowFromTop 0 Nothing face1

    -- results
    row0 == [1,2,3] -- that's the row data we wrote the first time
    face1 ==        -- changed results of face
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
    row1 == row0 -- row we wrote in was read back out
        
-}

rowFromTop : Int -> Maybe CubeRowLayout -> CubeFaceLayout ->
  (CubeRowLayout, CubeFaceLayout(modified face))
rowFromTop rowInt rowNew (CubeFaceLayout {data, cubeSize}) as face =
  let
    rowOutput =
      data
      |>Array.get row
      |>Maybe.withDefault (blankRowLayout cubeSize)

    faceOutput =
      case rowNew of
        Nothing ->
          face

        Just rowNew ->
          { face
          | data =
              data
              |>Array.set rowInt rowNew
          }

  in
    (rowOutput, faceOutput)


{-|Getter/setter function. Used to address a row counting from the bottom.

    --setup starting condition
    face0 = solidFaceLayout 3 4

    -- write and read
    (row0, face1) = rowFromBottom 0 ( Just [1,2,3] ) face0

    -- read only from previously written state
    (row1, _) = rowFromBottom 0 Nothing face1

    -- results
    row0 == [1,2,3] -- that's the row data we wrote the first time
    face1 ==        -- changed results of face
      ( CubeRowLayout [4,4,4]
      , CubeFaceLayout
        { cubeSize = 3
        , data =
          [ CubeRowLayout [4,4,4]
          , CubeRowLayout [4,4,4]
          , CubeRowLayout [1,2,3]
          ]
        }
      )
    row1 == row0 -- row we wrote in was read back out
-}

rowFromBottom : Int -> Maybe CubeRowLayout -> CubeFaceLayout ->
  (CubeRowLayout, CubeFaceLayout(modified face))
rowFromBottom rowInt rowNew (CubeFaceLayout {cubeSize}) as face =
  rowFromTop (cubeSize-row) rowNew face


{-|Getter/setter function. Used to address a col counting from the left.

    --setup starting condition
    face0 = solidFaceLayout 3 4

    -- write and read
    (col0, face1) = colFromLeft 0 ( Just [1,2,3] ) face0

    -- read only from previously written state
    (col1, _) = colFromLeft 0 Nothing face1

    -- results
    col0 == [1,2,3] -- that's the col data we wrote the first time
    face1 ==        -- changed results of face
      ( CubeRowLayout [4,4,4]
      , CubeFaceLayout
        { cubeSize = 3
        , data =
          [ CubeRowLayout [1,4,4]
          , CubeRowLayout [2,4,4]
          , CubeRowLayout [3,4,4]
          ]
        }
      )
    col1 == col0 -- col we wrote in was read back out
-}

colFromLeft : Int -> CubeFaceLayout -> CubeRowLayout
colFromLeft col (CubeFaceLayout {data, cubeSize}) =
  data
  |>Array.map
    ( \col ->
        Array.get col col
        |>Maybe.withDefault Nothing
    )


{-|Getter/setter function. Used to address a col counting from the right.

    --setup starting condition
    face0 = solidFaceLayout 3 4

    -- write and read
    (col0, face1) = colFromRight 0 ( Just [1,2,3] ) face0

    -- read only from previously written state
    (col1, _) = colFromRight 0 Nothing face1

    -- results
    col0 == [1,2,3] -- that's the col data we wrote the first time
    face1 ==        -- changed results of face
      ( CubeRowLayout [4,4,4]
      , CubeFaceLayout
        { cubeSize = 3
        , data =
          [ CubeRowLayout [4,4,1]
          , CubeRowLayout [4,4,2]
          , CubeRowLayout [4,4,3]
          ]
        }
      )
    col1 == col0 -- col we wrote in was read back out
-}

colFromRight : Int -> CubeFaceLayout -> CubeRowLayout
colFromRight col (CubeFaceLayout {cubeSize}) as face =
  colFromRight (cubeSize-col) face



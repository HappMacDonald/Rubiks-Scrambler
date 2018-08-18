module Rubiks.CubeFaceLayout exposing
  ( CubeFaceLayout
  , cubeFaceLayout
  , RowManipulator
  , cubeSize
  -- , solidFaceLayout
  -- , blankFaceLayout
  -- , rowFromTop
  -- , rowFromBottom
  -- , colFromLeft
  -- , colFromRight
  )


--import Rubiks.Constants
import Rubiks.CubeRowLayout as CRL


import Array exposing (Array)
import Maybe.Extra


type CubeFaceLayout =
  CubeFaceLayout (Array CRL.CubeRowLayout)


type alias RowManipulator =
  Int -> CRL.CubeRowLayout -> CubeFaceLayout ->
  (CRL.CubeRowLayout, CubeFaceLayout)


{-| Primary constructor. Transforms a list of lists of Ints into a cube face.

    cubeFaceLayout [[1,2],[3,4]] == Just CubeFaceLayout (opaque)

Empty list fails
    cubeFaceLayout [] == Nothing

Any rows with lengths not matching number of rows fails
    cubeFaceLayout [[1],[2]] == Nothing

Any invalid sticker colors fails
    cubeFaceLayout [[1,2],[-3,4]] == Nothing
-}

cubeFaceLayout : List (List Int) -> Maybe CubeFaceLayout
cubeFaceLayout rowsData =
  case rowsData of
    [] -> -- empty lists fail
      Nothing
    
    head :: tail -> -- all nonempty lists must have a head
      let
        length =
          List.length head -- how long is head?
      in
        if length < 1
          ||(List.length rowsData) /= length
        then
          Nothing
        else
          case List.all (\row -> List.length row == length) tail of
            False ->
              Nothing

            True ->
              ( CRL.cubeRowLayout head )
              ::( List.map CRL.cubeRowLayout tail )
              |>Array.fromList
              |>Maybe.Extra.combineArray
              |>Maybe.map CubeFaceLayout


{-|
-}

cubeSize : CubeFaceLayout -> Int
cubeSize ( CubeFaceLayout rows ) =
  Array.length rows

-- {-|Constructs a face that is a solid color, the one provided.
-- "color" is expressed as a Maybe Int.
-- 0-6 are valid cube colors as defined in Rubicks.Constants.
-- Nothing is the blank color.
-- Nothing renders to transparent in graphics, and is used as
-- a logical transparency in overpainting operations.

--     solidFaceLayout 4 2 == a 4x4 CubeFaceLayout that is yellow everywhere.
-- -}
-- solidFaceLayout : Int -> Maybe Int -> CubeFaceLayout
-- solidFaceLayout cubeSize color =
--   CubeFaceLayout
--   { cubeSize = cubeSize
--   , data =
--       Array.fromList
--       <|List.repeat cubeSize -- cubeSize full rows
--       <|solidRowLayout cubeSize color -- cubeSize cells per row
--   }


-- {-|Create an empty row of given size.

--     blankRowLayout 3 == 3 empty (colorless) cells in a row.
-- -}

-- blankRowLayout : Int -> CubeRowLayout
-- blankRowLayout cubesize =
--   solidRowLayout cubesize Nothing


-- {-|Create an empty face of the given size.

--     blankFaceLayout 3 == an empty (colorless) 3x3 face.
-- -}

-- blankFaceLayout : Int -> CubeFaceLayout
-- blankFaceLayout cubeSize =
--   solidFaceLayout cubeSize Nothing


-- cubeRowLayout : List (Maybe Int) -> CubeRowLayout
-- cubeRowLayout rowData =
--   rowData
--   -- |> List.map Just
--   |> Array.fromList
--   |> CubeRowLayout


-- flipRowLayout : CubeRowLayout -> CubeRowLayout
-- flipRowLayout (CubeRowLayout row) =
--   row
--   |> Array.toList
--   |> List.reverse
--   |> Array.fromList
--   |> CubeRowLayout 


-- {-|Getter/setter function. Used to address a row counting from the top.

--     --setup starting condition
--     face0 = solidFaceLayout 3 4

--     -- write and read
--     (row0, face1) = rowFromTop 0 ( Just [1,2,3] ) face0

--     -- read only from previously written state
--     (row1, _) = rowFromTop 0 Nothing face1

--     -- results
--     row0 == [1,2,3] -- that's the row data we wrote the first time
--     face1 ==        -- changed results of face
--       ( CubeRowLayout [4,4,4]
--       , CubeFaceLayout
--         { cubeSize = 3
--         , data =
--           [ CubeRowLayout [1,2,3]
--           , CubeRowLayout [4,4,4]
--           , CubeRowLayout [4,4,4]
--           ]
--         }
--       )
--     row1 == row0 -- row we wrote in was read back out
        
-- -}

-- rowFromTop : RowManipulator
-- rowFromTop rowInt rowNew ((CubeFaceLayout {data, cubeSize}) as face) =
--   let
--     rowOutput =
--       data
--       |>Array.get rowInt
--       |>Maybe.withDefault (blankRowLayout cubeSize)

--     faceOutput =
--       case rowNew of
--         Nothing ->
--           face

--         Just rowNew ->
--           CubeFaceLayout
--           { cubeSize = cubeSize
--           , data =
--               data
--               |>Array.set rowInt rowNew
--           }

--   in
--     (rowOutput, faceOutput)


-- {-|Getter/setter function. Used to address a row counting from the bottom.

--     --setup starting condition
--     face0 = solidFaceLayout 3 4

--     -- write and read
--     (row0, face1) = rowFromBottom 0 ( Just [1,2,3] ) face0

--     -- read only from previously written state
--     (row1, _) = rowFromBottom 0 Nothing face1

--     -- results
--     row0 == [1,2,3] -- that's the row data we wrote the first time
--     face1 ==        -- changed results of face
--       ( CubeRowLayout [4,4,4]
--       , CubeFaceLayout
--         { cubeSize = 3
--         , data =
--           [ CubeRowLayout [4,4,4]
--           , CubeRowLayout [4,4,4]
--           , CubeRowLayout [1,2,3]
--           ]
--         }
--       )
--     row1 == row0 -- row we wrote in was read back out
-- -}

-- rowFromBottom : RowManipulator
-- rowFromBottom rowInt rowNew ((CubeFaceLayout {cubeSize}) as face) =
--   rowFromTop (cubeSize-rowInt) rowNew face


-- {-|Getter/setter function. Used to address a col counting from the left.

--     --setup starting condition
--     face0 = solidFaceLayout 3 4

--     -- write and read
--     (col0, face1) = colFromLeft 0 ( Just [1,2,3] ) face0

--     -- read only from previously written state
--     (col1, _) = colFromLeft 0 Nothing face1

--     -- results
--     col0 == [1,2,3] -- that's the col data we wrote the first time
--     face1 ==        -- changed results of face
--       ( CubeRowLayout [4,4,4]
--       , CubeFaceLayout
--         { cubeSize = 3
--         , data =
--           [ CubeRowLayout [1,4,4]
--           , CubeRowLayout [2,4,4]
--           , CubeRowLayout [3,4,4]
--           ]
--         }
--       )
--     col1 == col0 -- col we wrote in was read back out
-- -}

-- colFromLeft : RowManipulator
-- colFromLeft colInt colNew ((CubeFaceLayout {data, cubeSize}) as face) =
--   let
--     colOutput =
--       data
--       |>Array.map
--       (\ (CubeRowLayout row) ->
--           row
--           |>Array.get colInt
--           |>Maybe.withDefault Nothing
--       )
--       |>CubeRowLayout

--     faceOutput =
--       case colNew of
--         Nothing ->
--           face

--         Just (CubeRowLayout colNew) ->
--           CubeFaceLayout
--           { cubeSize = cubeSize
--           , data =
--               data
--               |>Array.map
--               (\(CubeRowLayout row) ->
--                   row
--                   |>Array.set colInt
--                     ( Array.get colInt colNew
--                       |>Maybe.withDefault Nothing
--                     )
--                   |>CubeRowLayout
--               )
--           }

--   in
--     (colOutput, faceOutput)


-- {-|Getter/setter function. Used to address a col counting from the right.

--     --setup starting condition
--     face0 = solidFaceLayout 3 4

--     -- write and read
--     (col0, face1) = colFromRight 0 ( Just [1,2,3] ) face0

--     -- read only from previously written state
--     (col1, _) = colFromRight 0 Nothing face1

--     -- results
--     col0 == [1,2,3] -- that's the col data we wrote the first time
--     face1 ==        -- changed results of face
--       ( CubeRowLayout [4,4,4]
--       , CubeFaceLayout
--         { cubeSize = 3
--         , data =
--           [ CubeRowLayout [4,4,1]
--           , CubeRowLayout [4,4,2]
--           , CubeRowLayout [4,4,3]
--           ]
--         }
--       )
--     col1 == col0 -- col we wrote in was read back out
-- -}

-- colFromRight : RowManipulator
-- colFromRight colInt colNew ((CubeFaceLayout {cubeSize}) as face) =
--   colFromLeft (cubeSize-colInt) colNew face



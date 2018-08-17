module Rubiks.Helpers exposing (..)

{-| Just a mess of helper functions for Rubik's Scrambler
-}


import Array exposing (Array)
import Rubiks.Layers as Layers
import Rubiks.Constants as Constants
import Rubiks.CubeFaceLayout as CubeFaceLayout exposing (CubeFaceLayout)


{-| Just a quick, author-invented guestimate function of optimal number
of scramble moves given a cube size. Ultimately this is (cubesize-0.5)*6,
but I convoluted the formula slightly just to avoid floats.

    defaultScrambles 4 == 21
-}

defaultScrambles : Int -> Int
defaultScrambles cubeSize =
  ( cubeSize*2 - 1 ) * 3


{-| Converts string into an integer, iff it lies between min and max.
String is trimmed first, and blank string is a special case interpreted as 0.

    toRangedInteger "5" -10 10 == Ok 5
    toRangedInteger "5" 10 -10 == Err "min 10 must be no greater than max -10"
    toRangedInteger "  5\t" -10 10 == Ok 5
    toRangedInteger "5 " 8 10 == Err "'5 ' must be no less than 8"
    toRangedInteger " 5 " -10 3 == Err "' 5 ' must be no greater than 3"
    toRangedInteger "" -10 10 == Ok 0
    toRangedInteger "\t \t" -10 10 == Ok 0
    toRangedInteger "" 5 10 == Err "'' must be no less than 5"
-}

toRangedInteger : String -> Int -> Int -> Result String Int
toRangedInteger inputStr min max =
  let
    trimmed =
      String.trim inputStr

    input =
      if String.isEmpty trimmed then
        "0"
      else
        trimmed
        
  in
-- We've got to interpret empty as zero so that you can backspace through
-- an entire number and then type new digits and wind up with the expected
-- resulting number. You wind up typing after a zero that has unexpectedly
-- emerged from nowhere, but it then vanishes again after you type your
-- first new digit so everything still works out semantically. 👍
    if min > max then
      Err
        <|"min "
        ++( toString min )
        ++" must be no greater than max "
        ++( toString max )
        ++"."
    else 
      input |> String.toInt |> Result.andThen
        (\int ->
          if int>=min
            then if int<=max
              then Ok int
              else Err
                <|"'"
                ++inputStr
                ++"' must be no greater than "
                ++(toString max)
                ++"."
            else Err
              <|"'"
              ++inputStr
              ++"' must be no less than "
              ++(toString min)
              ++"."
        )


{-| Renders an internal list of moves into a list of strings to display.

    let
      moves =
      [ { axis = 1 -- second available layer of 2
        , layer = 4 -- "23"
        , twistDegrees = 1 -- 90 degrees
        }
      , { axis = 1 -- second available layer of 2
        , layer = 2 -- "2"
        , twistDegrees = 2 -- 180 degrees
        }
      , { axis = 0 -- first available layer of 2
        , layer = 9 -- "34"
        , twistDegrees = 3 -- 270 degrees
        }
      , { axis = -1 -- invalid
        , layer = 9
        , twistDegrees = 3
        }
      , { axis = 0
        , layer = -1 -- invalid
        , twistDegrees = 3
        }
      , { axis = 0
        , layer = 9
        , twistDegrees = 4 -- invalid
        }
      ]
    in
      renderMoves 4 0 moves ==
        [ "R23(90)" -- 0 taken, second available is 2="D"
        , "U12(180)" -- 2 taken, second available is 1="U"
        , "F34(270)" -- 1 taken, first available is 0="F"
        , "!!34(270)"
        , "U!!(270)" -- "-1 taken" technically has undefined behavior..
        , "F34(!!)" -- 1 taken, first available = 0="F"
        ]
-}

renderMoves : Int -> Int -> (List Constants.Move) -> List String
renderMoves cubeSize previousAxis uncookedMoves =
  case uncookedMoves of
    [] -> -- bottom out
      []
    
    uncookedMove :: remainingMoves ->
      let
        cookedMove =
          { uncookedMove
          | axis =
              if uncookedMove.axis >= previousAxis
                then uncookedMove.axis + 1
                else uncookedMove.axis

          }

      in
        ( ( Array.get cookedMove.axis Constants.axisNames
            |>Maybe.withDefault "!!"
          )
          ++( Layers.getLayer cubeSize cookedMove.layer
              |>Maybe.withDefault "!!"
            )
          ++"("
          ++( Array.get cookedMove.twistDegrees Constants.twistDegrees
              |>Maybe.withDefault "!!"
            )
          ++")"
        )
        :: renderMoves cubeSize cookedMove.axis remainingMoves


{-|This takes as input 2 integers representing:
A> which face to be Front (0..5)
B> which entry index along it's ring to be Up (0..3)

and returns the full Front/Up/Right orientation that represents.

    renderOrientation (0, 0) -- Red front, first ring element
      == Orientation 0 1 2 -- Front=Red, Up=Blue, Right=Yellow
    renderOrientation (5, 2) -- Orange front, third ring element
      == Orientation 5 4 3 -- Front=Orange, Up=White, Right=Green
-}

renderOrientation : Constants.RandomOrientation -> Constants.Orientation
renderOrientation ( front, upIndex ) =
  let
    frontRing =
      Constants.faceRings
      |>Array.get front

    up =
      frontRing
      |>Maybe.andThen ( Array.get upIndex )

    rightIndex =
      ( upIndex+1 ) -- next index in clockwise dir
      % 
      ( Array.length Constants.twistDegrees ) -- modulo length of array

    right =
      frontRing
      |>Maybe.andThen ( Array.get rightIndex )

  in
    Maybe.map2 ( Constants.Orientation front ) up right
    |>Maybe.withDefault Constants.defaultOrientation


solidCubeLayout : Int -> List (Maybe Int) -> Array CubeFaceLayout
solidCubeLayout cubeSize colors =
  Array.fromList colors
  |>Array.map
    ( CubeFaceLayout.solidFaceLayout cubeSize )


{-|This creates a fresh new cube layout that is colored per
ordinary face order (cells in face = faceIndex :P) at the specified cubeSize.

    defaultCubeLayout 2 == (A default CubeLayout for a 2x2x2)
-}

defaultCubeLayout : Int -> Array CubeFaceLayout
defaultCubeLayout cubeSize =
  List.range 0 5
  |> List.map Just
  |> solidCubeLayout cubeSize


{-|Creates a solid cube layout that matches the requested orientation.
-}

orientedCubeLayout : Int -> Constants.Orientation -> Array CubeFaceLayout
orientedCubeLayout cubeSize orientation =
  -- let
    -- opposingColor : Int -> Maybe Int
    -- opposingColor color =
    --   Constants.opposingColorInts |> Array.get color 
      -- Array.get color Constants.opposingColorInts
      -- Maybe.map
      --   Constants.opposingColorInts
      --   |>Array.get color
  -- in
    [ Just orientation.up
    , Constants.opposingColorInts |> Array.get orientation.right
    , Just orientation.front
    , Just orientation.right
    , Constants.opposingColorInts |> Array.get orientation.front
    , Constants.opposingColorInts |> Array.get orientation.up
    ]
    |>solidCubeLayout cubeSize



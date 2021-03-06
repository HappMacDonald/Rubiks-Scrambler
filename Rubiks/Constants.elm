module Rubiks.Constants exposing (..)

{-| Just constants used by the Rubik's Scramble app
-}


import Array exposing (Array)
import Char


type alias Model =
  { errorStr : String
  , scrambleTotalMoves : Int
  , scrambleResults : List String
  , cubeSize : Int
  , orientation : Orientation
  , cubeLayout : CubeLayout
  }


type Msg
  = UpdateScrambleMoves String
  | DoScrambles
  | DoneScrambles RandomPayload
  | SizeChange String


type alias Move =
  { twistDegrees : Int -- index into array of same name
  , axis : Int -- index into array of same name
  , layer : Int -- index into array of same name
  }


type alias Orientation =
  { front : Int
  , up : Int
  , right : Int
  }


type alias RandomOrientation =
  ( Int, Int )


type alias RandomPayload =
  ( RandomOrientation, Int, List Move )


type alias CubeLayout =
  Array CubeFaceLayout
  

type alias CubeFaceLayout =
  Array CubeRowLayout


type alias CubeRowLayout =
  Array Int


minimumAllowedMoves : Int
minimumAllowedMoves =
  0


maximumAllowedMoves : Int
maximumAllowedMoves =
  30


defaultCubeSize : Int
defaultCubeSize =
  3


blankLayerChar : Char
blankLayerChar =
  '-'


defaultOrientation : Orientation
defaultOrientation =
  Orientation 0 0 0 -- All sides red, basically an error condition.


twistDegrees : Array String
twistDegrees =
  Array.fromList ["  0", " 90", "180", "270"]


axisNames : Array String
axisNames =
  Array.fromList ["F", "U", "R"]


faceData : Array String
faceData =
  Array.fromList
  [ "Red"
  , "Blue"
  , "Yellow"
  , "Green"
  , "White"
  , "Orange"
  , "" -- Used for non-cell space in the scrambled graphic
  ]


opposingColors : Array Int
opposingColors =
  Array.fromList
  [ 5 --Orange - Yellow = 0=Red
  , 3 --Green - Yellow = 1=Blue
  , 4 --White + Yellow = 2=Yellow
  , 1 --Blue + Yellow = 3=Green
  , 2 --Yellow - Yellow = 4=White
  , 0 --Red + Yellow = 5=Orange
  ]


blankFace : Int
blankFace =
  6


{-| This strategy numbers each face, and defines an "axis" as
a rotational ring of faces starting from arbitrary points and
proceeding in clockwise order per F/U/R.

The first dimension lines up with axisNames, and the second
dimension represents the ring of rotations lining up with
indices of twistDegrees
-}

faceRings : Array ( Array Int )
faceRings =
  Array.fromList
  [ Array.fromList [ 1, 2, 3, 4 ] -- Red ring (blue,yellow,green,white)
  , Array.fromList [ 4, 5, 2, 0 ] -- Blue ring (white,orange,yellow,red)
  , Array.fromList [ 3, 0, 1, 5 ] -- Yellow ring (greem,red,blue,orange)
  , Array.fromList [ 5, 4, 0, 2 ] -- Green ring (orange,white,red,yellow)
  , Array.fromList [ 0, 3, 5, 1 ] -- White ring (red,green,orange,blue)
  , Array.fromList [ 2, 1, 4, 3 ] -- Orange ring (yellow,blue,white,green)
  ]


cubeShapes : List String
cubeShapes =
  List.map
  ( \cubeSize ->
      cubeSize
      |>toString
      |>List.repeat 3
      |>String.join "x"

  )
  <| List.range 1 10



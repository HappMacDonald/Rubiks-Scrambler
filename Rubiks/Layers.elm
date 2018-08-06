module Rubiks.Layers exposing (numberOfLayers, getLayer)

{-| This library handles sorting out valid combinations
of layers to use in a scramble move

# Temporary, transitional, hardcoded Array of Strings
@docs layers

-}


import Array exposing (Array)
import Arithmetic exposing (toBase)
import Char
import MyBasics exposing (incrementIf, decrementIf, curryRight)


-- This is a type never meant to be validly used,
-- thus the presence of it's constructor guarantees a compiler error.
type Todo = Todo


-- Public (exposed) functions:

{-| Tells you how many total, valid layers exist for a cube of a given size.

    numberOfLayers 3 == 7
    numberOfLayers 4 == 11
    numberOfLayers 5 == 20
-}

numberOfLayers : Int -> Int
numberOfLayers cubeSize =
  let
    allUncookedLayers = List.range 0 ( (2^cubeSize) - 1 )
{-
    _ =
      Debug.log "numberOfLayers"
      <|toString
          { allUncookedLayers = allUncookedLayers
          }

-}
  in
  MyBasics.count (isUncookedLayerValid cubeSize) allUncookedLayers


{-| Renders the exact string matching which layers turn
for the Nth valid layer combination for a cube of cubeSize.

    getLayer <cubeSize:Int> <desiredLayer:int>
      == <layerString:Maybe String>
    getLayer 3 0 == Just "1"
    getLayer 5 16 == Just "1245"
    getLayer 3 100 == Nothing
-}

getLayer : Int -> Int -> Maybe String
getLayer cubeSize desiredLayer =
  getLayerHelper 0 0 cubeSize desiredLayer


-- Private (helper) functions

getLayerHelper : Int -> Int -> Int -> Int -> Maybe String
getLayerHelper uncookedIndex layerValidIndex cubeSize desiredLayer =
  let
    isValid =
      isUncookedLayerValid cubeSize uncookedIndex

{-
    _ = Debug.log "getLayerHelper"
    <|toString
        { uncookedIndex = uncookedIndex
        , layerValidIndex = layerValidIndex
        , limit = ( 2 ^ cubeSize )
        , desiredLayer = desiredLayer
        }

-}
  in
  if uncookedIndex < 2 ^ cubeSize then
    if isValid && desiredLayer == layerValidIndex then
      Just <| renderUncookedLayer (Char.toCode '1') uncookedIndex
    else
      getLayerHelper
        ( uncookedIndex + 1 )
        ( incrementIf
            isValid
            layerValidIndex
        )
        cubeSize
        desiredLayer
  else
    -- search failed, we ran out of uncooked layers to check
    -- before we got to layerValidIndex. D:
    Nothing


{-| Recursive function to count how many times a list of binary digits
changes between zero and one. Expects caller to feed it the head of the list
separately from the tail.

    countCrosses <first> <runningTotal> <allTheRest> == <finalTotal>
    countCrosses 1 0 [1,1,1] == 0
    countCrosses 0 0 [1,0,1] == 3
-}

countCrosses : Int -> Int -> List Int -> Int
countCrosses lastBit oldTotal moreBits =
  case moreBits of
    [] ->
      oldTotal

    thisBit :: stillMoreBits ->
      let
        currentTotal =
          -- the incremental magic happens when the bits disagree!
          incrementIf ( lastBit /= thisBit ) oldTotal

      in
        countCrosses thisBit currentTotal stillMoreBits
        

{-| Given an uncookedIndex and a charcode to represent the first layer,
render the move into a string. Each successive layer gets represented
by the character at the code point following the preceding layer.

    renderUncookedLayer (String.toCharCode "1") 15 == "1234"
    renderUncookedLayer (String.toCharCode "A") 11 == "ACD"
-}

renderUncookedLayer : Int -> Int -> String
renderUncookedLayer digitCharCode uncookedIndex =
{-
  let
    _ = Debug.log "renderUncookedLayer"
    <|toString
        { digitCharCode = digitCharCode
        , uncookedIndex = uncookedIndex
        }

  in
-}
  if uncookedIndex<1 then -- bottom out
    ""
  else
    let
      currentChar =
        if MyBasics.isEven uncookedIndex then
          ""
        else
          digitCharCode
          |>Char.fromCode
          |>String.fromChar 
    in
      String.append
        currentChar
        ( renderUncookedLayer
          ( digitCharCode + 1 )
          ( uncookedIndex // 2 )
        )


isUncookedLayerValid : Int -> Int -> Bool
isUncookedLayerValid cubeSize uncookedLayer =
  if uncookedLayer == 0 then
    False
  else
    let
      bits =
        toBase 2 uncookedLayer
        |>MyBasics.padListLeft cubeSize [0]

      crosses =
        case bits of
          [] ->
            Nothing
          
          headBit :: tailBits ->
            Just <| countCrosses headBit 0 tailBits

{-
      _ = Debug.log "isUncookedLayerValid"
          <|toString
              { cubeSize = cubeSize
              , uncookedLayer = uncookedLayer
              , bits = bits
              , crosses = crosses
              }

-}
    in
      -- We only allow a maximum of two "crosses"
      -- between participation and non-participation of adjacent layers
      -- in any candidate layer combination in order for it to be called valid.
      case crosses of
        Nothing ->
          False

        Just crosses ->
          crosses<=2

-- Deprecated, transitional, hardcoded crap

{-| This is a temporary (transitional), hardcoded list of layers.
It can be accessed via Array.get like so:

    Array.get 2 layers == "12"
-}

{- 3x3x3 layers
layers : Array String
layers =
    Array.fromList
--  [   ""     --  0,X  NO-OP
    [   "1"    --  1,0  Front face
    ,   "2"    --  2,1  second to Front-most layer
    ,   "12"   --  3,2  Front half
    ,   "3"    --  4,3  second from back layer
    ,   "13"   --  5,4  ew!
    ,   "23"   --  6,5  Middle two layers
    ,   "123"  --  7,6  Everything EXCEPT back face
--  ,   "4"    --  8,X  Just the back face
--  ,   "14"   --  9,X  Front AND back faces
--  ,   "24"   -- 10,X  ew!
--  ,   "124"  -- 11,X  ew..
--  ,   "34"   -- 12,X  Back half
--  ,   "134"  -- 13,X  ew..
--  ,   "234"  -- 14,X  Everything EXCEPT front face
--  ,   "1234" -- 15,X  Turn entire cube in your hand
    ]
--}


{- 4x4x4 layers 
layers : Array String
layers =
    Array.fromList
--  [   ""     --  0,X  NO-OP
    [   "1"    --  1,0  Front face
    ,   "2"    --  2,1  second to Front-most layer
    ,   "12"   --  3,2  Front half
    ,   "3"    --  4,3  second from back layer
--  ,   "13"   --  5,X  ew!
    ,   "23"   --  6,4  Middle two layers
    ,   "123"  --  7,5  Everything EXCEPT back face
    ,   "4"    --  8,6  Just the back face
    ,   "14"   --  9,7  Front AND back faces
--  ,   "24"   -- 10,X  
    ,   "124"  -- 11,8  ew..
    ,   "34"   -- 12,9  Back half
    ,   "134"  -- 13,10 
    ,   "234"  -- 14,11 Everything EXCEPT front face
    ,   "1234" -- 15,12 Turn entire cube in your hand
    ]
--}


{- 5x5x5 layers -}
layers : Array String
layers =
    Array.fromList
--  [   ""      --  0,X  NO-OP
    [   "1"     --  1,0  Front face
    ,   "2"     --  2,1  second to Front-most layer
    ,   "12"    --  3,2  Front 2 layers
    ,   "3"     --  4,3  third layer
--  ,   "13"    --  5,X  First and last.. or else .. ew!
    ,   "23"    --  6,4  two adjacent layers past front layer
    ,   "123"   --  7,5  Front 3 layers
    ,   "4"     --  8,6  Fourth layer
--  ,   "14"    --  9,X  First and last.. or else .. ew!
--  ,   "24"    -- 10,X  ew!
--  ,   "124"   -- 11,X  ew..
    ,   "34"    -- 12,7  Third and Fourth layers
--  ,   "134"   -- 13,X  ew..
    ,   "234"   -- 14,8  Second through fourth layers
    ,   "1234"  -- 15,9  Front 4 layers
    ,   "5"     -- 16,10 Fifth layer
    ,   "15"    -- 17,11 First and last.. or else .. ew!
--  ,   "25"    -- 18,X  ew..?
    ,   "125"   -- 19,12 holding layer 2 and 3 still
--  ,   "35"    -- 20,X  ew.. 
--  ,   "135"   -- 21,X  ew~
--  ,   "235"   -- 22,X  ...
    ,   "1235"  -- 23,13 Yeah, I can handle this one.
    ,   "45"    -- 24,14 pair of layers together
    ,   "145"   -- 25,15 can use thumb to hold middle bit still for 5x5x5
--  ,   "245"   -- 26,X  Nope.
    ,   "1245"  -- 27,16 5x5x5 hold middle still
    ,   "345"   -- 28,17 3 layers
    ,   "1345"  -- 29,18 5x5x5 hold layer 2 still
    ,   "2345"  -- 30,19 4 layers
    ,   "12345" -- 31,20 5x5x5 rotate entire cube
    ]
--}

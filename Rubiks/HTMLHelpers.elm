module Rubiks.HTMLHelpers exposing (cubeShapeOptions)


{-|Helper module for RubiksScrambler that produces snippets of HTML
-}


import Html exposing (Html)
import Html.Attributes as Attr


cubeShapeOptions : Int -> Int -> List String -> List (Html msg)
cubeShapeOptions defaultCubeSize index cubeShapes =
  case cubeShapes of
    [] ->
      [] -- bottom out

    head :: rest ->
      Html.option
        [ index
          |> toString
          |> Attr.value
        , Attr.selected <| index == defaultCubeSize
        ]
        [ Html.text <| "Cube Shape: " ++ head ]
      :: cubeShapeOptions defaultCubeSize (index+1) rest
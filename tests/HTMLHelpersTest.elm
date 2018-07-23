module HTMLHelpersTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
--import Fuzz

import Rubiks.HTMLHelpers as H
import Rubiks.Constants as C

import Html exposing (Html)
import Html.Attributes as Attr

htmlHelpersTest : Test
htmlHelpersTest =
  describe "HTMLHelpers"
  [ describe "cubeShapeOptions"
    [ test "empty in, empty out"
      <|\_ ->
          Expect.equal ( H.cubeShapeOptions 4 0 [] ) []
    , test "simple example"
      <|\_ ->
          let
            input =
              [ "square"
              , "circle"
              , "triangle"
              ]

            output =
              [ Html.option
                [ Attr.value "0"
                , Attr.selected False
                ]
                [ Html.text "Cube Shape: square" ]
              , Html.option
                [ Attr.value "1"
                , Attr.selected False
                ]
                [ Html.text "Cube Shape: circle" ]
              , Html.option
                [ Attr.value "2"
                , Attr.selected True
                ]
                [ Html.text "Cube Shape: triangle" ]
              ]
          in
            Expect.equal
              ( H.cubeShapeOptions 2 0 input )
              output
    , describe "orientationDisplay"
      [ test "simple example"
        <|\_ ->
            let
              expectedOutput =
                Html.dl []
                  [ Html.dt
                    [ Attr.class "Red" ]
                    [ Html.text <| "Front:" ]
                  , Html.dd
                    [ Attr.class "Red" ]
                    [ Html.text <| "Red" ]
                  , Html.dt
                     [ Attr.class "White" ]
                     [ Html.text <| "Up:" ]
                  , Html.dd
                    [ Attr.class "White" ]
                    [ Html.text <| "White" ]
                  , Html.dt
                    [ Attr.class "Blue" ]
                    [ Html.text <| "Right:" ]
                  , Html.dd
                    [ Attr.class "Blue" ]
                    [ Html.text <| "Blue" ]
                  ]

            in
              Expect.equal
              ( H.orientationDisplay ( C.Orientation 0 4 1) )
              expectedOutput
      ]
    ]
  ]
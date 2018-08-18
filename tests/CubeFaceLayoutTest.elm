module CubeFaceLayoutTest exposing (cubeFaceLayoutTest)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)
--import Fuzz

--import Array

import Rubiks.CubeFaceLayout as CFL


cubeFaceLayoutTest : Test
cubeFaceLayoutTest =
  describe "cubeFaceLayoutTest"
  [ describe "cubeFaceLayout"
    [ test "cubeFaceLayout [[1,2],[3,4]] /= Nothing"
        (\_->
          Expect.notEqual
            ( CFL.cubeFaceLayout [[1,2],[3,4]] )
            Nothing
        )
    , test "cubeFaceLayout [] == Nothing"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [] )
            Nothing
        )
    , test "cubeFaceLayout [[1,2],[3]] == Nothing"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [[1,2],[3]] )
            Nothing
        )
    , test "cubeFaceLayout [[1,2],[-3,4]] == Nothing"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [[1,2],[-3,4]] )
            Nothing
        )
    ]
  
  , describe "cubeSize"
    [ test "cubeFaceLayout [[1,2],[3,4]] |> cubeSize == 2"
        (\_->
          Expect.equal
            ( CFL.cubeFaceLayout [[1,2],[3,4]] |> Maybe.map CFL.cubeSize )
            ( Just 2 )
        )
    ]
  ]

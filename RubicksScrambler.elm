import Html exposing (Html, div)
import Html.Attributes as Attr
import Html.Events as Events
import Array exposing (Array)
import Random

{-|

-- Todo
* Current rendered move list gets lots of "!!"'s, which indicate exceptions.

* Display rendered move list.

* Probably support 0 moves

* Make a button that you have to press to render new moves.

* Build a visual for what the scrambled cube will look like I suppose?

* randomly choose and then demand colors be assigned to F/U/R 
** Presume plus-yellow color scheme with Red->White->Blue clockwise around a corner.

* Support sizes other than 4x4x4
** This involves changing how I do "Layers". Maybe make it a record of arrays of strings or something *shrugs*

-}


-- CONSTANTS


twistDegrees : Array String
twistDegrees =
    Array.fromList ["90", "180", "270"]


axes : Array String
axes =
    Array.fromList ["F", "U", "R"]


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
--  ,   "24"   -- 10,X  ew!
--  ,   "124"  -- 11,X  ew..
    ,   "34"   -- 12,8  Back half
--  ,   "134"  -- 13,X  ew..
    ,   "234"  -- 14,9  Everything EXCEPT front face
    ,   "1234" -- 15,10 Turn entire cube in your hand
    ]


-- HELPER FUNCTIONS

{-| Returns result with an error (could not parse to integer, or integer not positive) or with parsed positive integer.
-}

toRangedInteger : String -> Int -> Int -> Result String Int
toRangedInteger inputStr min max =
    String.toInt (String.trim inputStr)
    |> Result.andThen
        (\int ->
            if int>=min
                then if int<=max
                    then Ok int
                    else Err <| "'" ++ inputStr ++ "' must be less than " ++ (toString max) ++ "."
                else Err <| "'" ++ inputStr ++ "' must be greater than " ++ (toString min) ++ "."
        )


-- MAIN

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions =
        (\_ -> Sub.none)

    }


-- COMMANDS

type alias Move =
    {   twistDegrees : Int -- index into array of same name
    ,   axis : Int -- index into array of same name
    ,   layer : Int -- index into array of same name
    }


randomlySelectAxis : Random.Generator Int
randomlySelectAxis =
    ( Random.int 0 <| (Array.length axes) - 1) -- randomly select initial, fake "previously used axis"


randomMove : Random.Generator Move
randomMove =
    Random.map3
        Move
            ( Random.int 0 <| (Array.length twistDegrees) - 1 )
            ( Random.int 0 <| (Array.length axes) - 2 ) -- skip previously-used axis
            ( Random.int 0 <| (Array.length layers) - 1 )


doScrambles : Model -> Cmd Msg
doScrambles model =
    Random.generate
    DoneScrambles
    (   Random.pair
            randomlySelectAxis
            ( Random.list model.scrambleTotalMoves randomMove )
    )


-- MODEL / INIT


type alias Model =
    {   errorStr : String
    ,   scrambleTotalMoves : Int
    ,   scrambleResults : List String
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            {   errorStr =
                    ""

            ,   scrambleTotalMoves =
                    14

            ,   scrambleResults = []

            }
    in
        model ! [ doScrambles model ]


-- UPDATE

renderMove : Move -> (Int, List String) -> (Int, List String)
renderMove uncookedMove (previousAxis, intermediateResult) =
    let
        cookedMove =
            {   uncookedMove
            |   axis =
                    if uncookedMove.axis >= previousAxis
                        then uncookedMove.axis + 1
                        else uncookedMove.axis

            }

    in
        (   cookedMove.axis -- This move's cooked axis is next move's "previously used axis".
        ,   intermediateResult -- Append the new string onto the result list of strings.
        ++  [   (   Maybe.withDefault "!!"
                    <| Array.get cookedMove.axis axes
                )
                ++  (   Maybe.withDefault "!!"
                        <| Array.get cookedMove.layer layers
                    )
                ++  "("
                ++  (   Maybe.withDefault "!!"
                        <| Array.get cookedMove.twistDegrees twistDegrees
                    )
                ++  ")"
            ]
        )



type Msg
    =   UpdateScrambleMoves String
    |   DoScrambles
    |   DoneScrambles ( Int, List Move )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateScrambleMoves inputStr ->
            case toRangedInteger inputStr 1 30 of
                Err error ->
                    { model
                    | errorStr =
                        error
                    
                    } ! []

                Ok scrambleTotalMoves ->
                    { model
                    | errorStr =
                        ""
                    
                    ,   scrambleTotalMoves =
                            scrambleTotalMoves
                    
                    } ! []
        
        DoScrambles ->
            model ! [ doScrambles model ]
        
        DoneScrambles ( previousAxis, moves ) ->
            let
                (_, scrambleResults) =
                    List.foldl renderMove (previousAxis, []) moves

            in
                { model
                |   scrambleResults =
                        scrambleResults

{- Quick debug in case choices seem strange
                ,   errorStr
                        =   (List.head moves |> toString)
                        ++ " : "
                        ++  toString previousAxis

-}

                } ! []


-- VIEW


view : Model -> Html Msg
view model =
    div []
    <|  [   Html.node "style" []
            [   Html.text """
                    html
                    {   font-family: sans-serif;
                    }
                    div.error
                    {   background-color: red;
                        color: white;
                        text-weight: bold;
                    }
                    """
            ]
        ,   div [ Attr.class "error" ] [ Html.text model.errorStr ]
        ,   Html.input
            [   Attr.type_ "number"
            ,   Attr.value <| toString model.scrambleTotalMoves
            ,   Attr.attribute "min" "1"
            ,   Attr.attribute "max" "30"
            ,   Events.onInput UpdateScrambleMoves
            ] []
        ,   Html.button
                [ Events.onClick DoScrambles ]
                [ Html.text "Show " {-++ (toString model.scrambleTotalMoves) ++ " New Scrambles!"-} ]
        ]
    ++  ( List.map (\move -> Html.p [] [ Html.text move ]) model.scrambleResults )


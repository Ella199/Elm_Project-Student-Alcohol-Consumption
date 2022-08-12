module VisualisationStickfigurePlot exposing (..)

import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (g, polyline, style, svg, text_)
import TypedSvg.Attributes as TSA exposing (class, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types as TST exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px) 
import Browser
import Html exposing (div, h1, p, button)
import Html.Events exposing (onClick)
import Http
import Csv
import Csv.Decode
import Statistics
import Html.Attributes as HA

w : Float
w =
    900

h : Float
h =
    450

padding : Float
padding =
    60

radius : Float
radius =
    5.0

tickCount : Int
tickCount =
    5

    main : Program () Model Msg
main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getCsv GotText
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

type Model
 = Error
 | Loading
 | Success
    { data : List StudentAcoholConsumption
    , len : Float
    }


type alias StudentAcoholConsumption =
    { sex : String
    , firstperiodGradeMath : Float
    , secondperiodGradeMath : Float
    , thirdperiodGradeMath : Float
    , firstperiodGradePort : Float
    , secondperiodGradePort : Float
    , thirdperiodGradePort : Float
    , dalc : Float
    , walc : Float
    , medu : Float
    , fedu : Float
    , absences : Float
    , freetime : Float
    }
    type Msg
    = GotText (Result Http.Error String)
    | ChangeLen (String)


type alias Point =
    { pointName : String, x : Float, y : Float, z : Float, a : Float, b : Float , c : Float , d : Float , e : Float , f : Float , g : Float , h : Float , i : Float } 

type alias XYData =
    { data : List Point
    }
getCsv : (Result Http.Error String -> Msg) -> Cmd Msg
getCsv x = 
    list
        |> List.map
            (\data ->
                Http.get
                    { url = "https://raw.githubusercontent.com/Ella199/Elm_Project-Student-Alcohol-Consumption/main/Data/CSV_Daten/" ++ data
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

list : List String 
list = 
    [ "mergedstudent_FINAL_NaN.csv" ]
    csvStringToData : String -> List StudentAcoholConsumption
csvStringToData csvR =
    Csv.parse csvR
        |> Csv.Decode.decodeCsv decodingStudentAcoholConsumption
        |> Result.toMaybe
        |>Maybe.withDefault []

decodingStudentAcoholConsumption : Csv.Decode.Decoder (StudentAcoholConsumption -> a) a
decodingStudentAcoholConsumption =
        Csv.Decode.map StudentAcoholConsumption
            (Csv.Decode.field "sex" Ok 
                
                |> Csv.Decode.andMap (Csv.Decode.field "firstperiodGradeMath"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "secondperiodGradeMath"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "thirdperiodGradeMath"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "firstperiodGradePort"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "secondperiodGradePort"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "thirdperiodGradePort"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "dalc"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "walc"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "Medu"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "Fedu"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "freetime"(String.toFloat >> Result.fromMaybe "error parsing string"))
                |> Csv.Decode.andMap (Csv.Decode.field "absences"(String.toFloat >> Result.fromMaybe "error parsing string"))
            )
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = studentAcoholConsumptionList [ fullText ], len=5 }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeLen v ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, len = Maybe.withDefault 0 <| String.toFloat v }, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
studentAcoholConsumptionList :List String -> List StudentAcoholConsumption
studentAcoholConsumptionList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat

filterReducedStudentAcoholConsumption : List StudentAcoholConsumption -> XYData 

filterReducedStudentAcoholConsumption my_stud =
    XYData <| List.filterMap stud2point my_stud

andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap = Maybe.map2 (|>)

stud2point : StudentAcoholConsumption -> Maybe Point
stud2point stud =
    Maybe.map pointLabel 
        (Just stud.sex) 
            |> andMap (Just stud.firstperiodGradeMath) 
            |> andMap (Just stud.secondperiodGradeMath)
            |> andMap (Just stud.thirdperiodGradeMath)
            |> andMap (Just stud.firstperiodGradePort)
            |> andMap (Just stud.secondperiodGradePort)
            |> andMap (Just stud.thirdperiodGradePort)
            |> andMap (Just stud.dalc)
            |> andMap (Just stud.walc)
            |> andMap (Just stud.fedu)
            |> andMap (Just stud.medu)
            |> andMap (Just stud.freetime)
            |> andMap (Just stud.absences)
pointLabel : String -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Point 
pointLabel sex firstperiodGradeMath secondperiodGradeMath thirdperiodGradeMath firstperiodGradePort secondperiodGradePort thirdperiodGradePort dalc walc fedu medu freetime absences= 
    Point (sex ++ " (" ++ String.fromFloat firstperiodGradeMath ++ ", " ++ String.fromFloat firstperiodGradeMath ++ ", "
        ++ String.fromFloat thirdperiodGradeMath ++ ", " ++ String.fromFloat secondperiodGradePort ++ ", " 
        ++ String.fromFloat secondperiodGradePort ++ ", " ++ String.fromFloat thirdperiodGradePort ++ ", "
        ++ String.fromFloat dalc ++ ", " ++ String.fromFloat walc ++ ", " ++ String.fromFloat fedu ++ ", "++ String.fromFloat medu ++ ", " 
        ++ String.fromFloat freetime ++ ", " ++ String.fromFloat absences ++")") 
        (secondperiodGradeMath) (firstperiodGradeMath) (thirdperiodGradeMath) (firstperiodGradePort) (secondperiodGradePort) (thirdperiodGradePort) (dalc) (walc) (medu) (fedu) (absences) (freetime)
xAxis : List Float -> Svg msg
xAxis values = 
    Axis.bottom [ Axis.tickCount tickCount] (xScale values)

yAxis : List Float -> Svg msg 
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] ( yScale values 
xScale : List Float -> ContinuousScale Float 
xScale values = 
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )
yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )
defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

adding : (Float, Float) -> Float-> (Float, Float) 
adding (min, max) x =
    if min <= 0 then
        ( 0, max + x)
    else 
        (min - x, max + x)
wideExtent : List Float -> ( Float, Float )
wideExtent values = 
    let
        result = 
            Maybe.withDefault (0, 0)
            (Statistics.extent values)
        
        max =          
            Maybe.withDefault (0)
            (List.maximum values)
            
        result1 = 
            adding result (toFloat(tickCount)*max/50)
        
        result2 = 
            adding result1 (0.0)       
    in
        result2
stickfigureplot : XYData -> Float -> Svg msg
stickfigureplot model len =
 -- funktionen und parameter deklarieren
    let
        
        xValues : List Float
        xValues =
            List.map .x model.data --x -- xmts

        yValues : List Float
        yValues =
            List.map .a model.data --y -- xmts

        uValues : List Float
        uValues =
            List.map .i model.data --u

        vValues : List Float
        vValues =
            List.map .e model.data --v

        pValues : List Float
        pValues =
            List.map .f model.data --p

        qValues : List Float
        qValues =
            List.map .h model.data --q

        zValues : List Float
        zValues =
            List.map .g model.data --z
        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues
        uDegree : List Float
        uDegree = 
            List.map (\x -> (270 - (x))) (inDegree uValues)

        vDegree : List Float
        vDegree = 
            List.map (\x -> (270 - (x))) (inDegree vValues)

        pDegree : List Float
        pDegree = 
            List.map (\x -> (270 - (x))) (inDegree pValues)

        qDegree : List Float
        qDegree = 
            List.map (\x -> (270 - (x))) (inDegree qValues)

        zDegree : List Float
        zDegree = 
            List.map (\x -> (270 - (x))) (inDegree zValues)

         half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) *8/9

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = (wideExtent xValues |> half) 
            , y = (wideExtent yValues |> Tuple.second)
            }
    in
    -- output als svg => scatter plot
    
    svg [ viewBox 0 0 w h, TSA.width <| TST.Percent 100, TSA.height <| TST.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .line polyline { stroke: lightGrey; fill: rgba(255, 255, 255,0.3); ; stroke-width:1; }
            .line text { display: none; }
            .line:hover polyline { stroke: black; stroke-width:1.5; }
            .line:hover text { display: inline; font-size: small }
          """ ]       
    -- plot x axis    
         , g[ transform [ Translate (60) (390)]]
            [
                xAxis xValues
                , text_
                [ x ( Scale.convert xScaleLocal (labelPositions.x))
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text "Math10" ] -- x -- xmts
                ]
     -- plot y axis             
         ,g[transform [Translate(60) (60)]]
         [
             yAxis yValues
             , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text "Port10" ] -- y -- xmts
             ]
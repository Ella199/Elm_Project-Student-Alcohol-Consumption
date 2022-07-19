module Scatterplot exposing (..)

import Axis
import Html exposing (Html)
import Http
import Scale exposing (ContinuousScale)
import Statistics 
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment (..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser
import Html exposing (li)
import Html.Events exposing (onClick)
import Html exposing (ul)

type Model
 = Error
 | Loading
 | Success
    { data : List StudentAcoholConsumption
    , xFunction : StudentAcoholConsumption -> Float
    , yFunction : StudentAcoholConsumption -> Float
    , xName : String
    , yName : String
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
    }
type Msg
    = GotText (Result Http.Error String)
    | ChangeX (StudentAcoholConsumption -> Float, String)
    | ChangeY (StudentAcoholConsumption -> Float, String)

type alias Point = 
    { pointName : String, x : Float, y : Float }
type alias XYData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }
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
            )
            -- hinzufügen update : Msg

        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = studentAcoholConsumptionList [ fullText ], xFunction = .thirdperiodGradeMath, yFunction = .dalc, xName = "workday alcohol consumption", yName = "third period Grade Math"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = x, yFunction = m.yFunction, xName = a, yName = m.yName }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = m.xFunction, yFunction = y, xName = m.xName, yName = a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

studentAcoholConsumptionList :List String -> List StudentAcoholConsumption
studentAcoholConsumptionList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat

filterReducedStudentAcoholConsumption : List StudentAcoholConsumption -> (StudentAcoholConsumption -> String) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption->Float) -> String -> String -> XYData 

filterReducedStudentAcoholConsumption studentAcoholConsumptionsliste a b c x y =
    XYData x y (List.map (\n -> pointName n a b c x y) studentAcoholConsumptionsliste)
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- nach vorne 
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

xAsis : List Float -> Svg msg
xAsis values = 
    Axis.bottom [ Axis.tickCount tickCount] (xScale values)

yAxis : List Float -> Svg msg 
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] ( yScale values )

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
pointName : StudentAcoholConsumption -> (StudentAcoholConsumption -> String) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> String -> String -> Point
pointName studentAcoholConsumption u v x y z =
    Point (u studentAcoholConsumption ++ ", " ++ y ++ ": " ++ String.fromFloat (v studentAcoholConsumption) ++ ", " ++ z ++ ": " ++ String.fromFloat (x studentAcoholConsumption)) (v studentAcoholConsumption) (x studentAcoholConsumption)

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY yxPoint =
    g
        [
            class["point"]
            ,fontSize <| Px 15.0
            ,fontFamily ["Times New Roman"]
            ,transform
                [
                    Translate
                    (Scale.convert scaleX yxPoint.x)
                    (Scale.convert scaleY yxPoint.y)
                ]
        ]

        [
            circle [cx 0, cy 0, r 5] []
            , text_ [x 10, y -20, textAnchor AnchorMiddle] [Html.text yxPoint.pointName]
        ]

scatterplot : XYData -> Svg msg
scatterplot model =
    let
        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPosition : { x : Float, y : Float }
        labelPosition =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(194, 27, 207, 0.8)); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(27, 97, 232, 0.73)); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAsis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPosition.x)
                , y 35
                 , fontFamily [ "Times New Roman" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30
                , fontFamily [ "Times New Roman" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]
view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Unfortunately scatterplot StudentAcoholConsumption can not be open."

        Loading ->
            Html.text "Loading StudentAcoholConsumption"

        Success l ->
            let
                studentAcoholConsumption =
                    filterReducedStudentAcoholConsumption l.data .sex l.xFunction l.yFunction l.xName l.yName

            in 
            Html.div []
                [
                    ul[][
                        li[][
                            Html.text <| "You have a lot of interesting combinations to look for X at here"
                            , Html.button [onClick (ChangeX (.firstperiodGradeMath, "first period Grade mathematics"))] [Html.text "first period Grade mathematics"]
                            , Html.button [onClick (ChangeX (.secondperiodGradeMath, "second period Grade mathematics"))] [Html.text "second period Grade mathematics"]
                            , Html.button [onClick (ChangeX (.thirdperiodGradeMath, "third period Grade mathematics"))] [Html.text "third period Grade mathematics"]
                            , Html.button [onClick (ChangeX (.firstperiodGradePort, "first period Grade portuguese"))] [Html.text "first period Grade portuguese"]
                            , Html.button [onClick (ChangeX (.firstperiodGradePort, "second period Grade portuguese"))] [Html.text "second period Grade portuguese"]
                            , Html.button [onClick (ChangeX (.firstperiodGradePort, "third period Grade portuguese"))] [Html.text "third period Grade portuguese"]
                            , Html.button [onClick (ChangeX (.dalc, "workday alcohol consumption"))] [Html.text "workday alcohol consumption"]
                            , Html.button [onClick (ChangeX (.walc, "weekend alcohol consumption"))] [Html.text "weekend alcohol consumption"]
                       ]
                    ]
                    , ul[][
                        li[][
                            Html.text <| "You have a lot of interesting combinations to look for Y at here"
                            , Html.button [onClick (ChangeY (.firstperiodGradeMath, "first period Grade mathematics"))] [Html.text "first period Grade mathematics"]
                            , Html.button [onClick (ChangeY (.secondperiodGradeMath, "second period Grade mathematics"))] [Html.text "second period Grade mathematics"]
                            , Html.button [onClick (ChangeY (.thirdperiodGradeMath, "third period Grade mathematics"))] [Html.text "third period Grade mathematics"]
                            , Html.button [onClick (ChangeY (.firstperiodGradePort, "first period Grade portuguese"))] [Html.text "first period Grade portuguese"]
                            , Html.button [onClick (ChangeY (.firstperiodGradePort, "second period Grade portuguese"))] [Html.text "second period Grade portuguese"]
                            , Html.button [onClick (ChangeY (.firstperiodGradePort, "third period Grade portuguese"))] [Html.text "third period Grade portuguese"]
                            , Html.button [onClick (ChangeY (.dalc, "workday alcohol consumption"))] [Html.text "workday alcohol consumption"]
                            , Html.button [onClick (ChangeY (.walc, "weekend alcohol consumption"))] [Html.text "weekend alcohol consumption"]
                       ]
                    ]
                    ,   scatterplot studentAcoholConsumption
                ]

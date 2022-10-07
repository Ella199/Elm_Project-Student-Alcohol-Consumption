module Scatterplot exposing (..)

import Axis
import Html exposing (Html)
import Http
import Scale exposing (ContinuousScale)
import Statistics 
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment (..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser
import Html exposing (li)
import Html.Events exposing (onClick)
import Html exposing (ul)
import Html.Attributes
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Attributes
import Json.Decode

type alias Data =
    { data : List StudentAcoholConsumption
    , xFunction : StudentAcoholConsumption -> Float
    , yFunction : StudentAcoholConsumption -> Float
    , xName : String
    , yName : String
    }

type Model
 = Error
 | Loading
 | Success Data


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

type Sex
    = M
    | F
    | UnknownSex

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
                    ( Success <| { data = studentAcoholConsumptionList [ fullText ], xFunction = .thirdperiodGradeMath, yFunction = .dalc, xName = "Alkoholkonsum (Wochentag)", yName = "Mathematik (12. Kl.)"}, Cmd.none )

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

sexLabel : String -> String
sexLabel sex = case sex of 
    "M" -> "männlich"
    "F" -> "weiblich"
    _ -> "unbekannt"

sexFlag :  String -> Sex
sexFlag sex = case sex of 
    "M" -> M
    "F" -> F
    _ -> UnknownSex

pointName : StudentAcoholConsumption -> (StudentAcoholConsumption -> String) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> String -> String -> Point
pointName studentAcoholConsumption u v x y z =
    Point (sexLabel (u studentAcoholConsumption) ++ ", " ++ y ++ ": " ++ String.fromFloat (v studentAcoholConsumption) ++ ", " ++ z ++ ": " ++ String.fromFloat (x studentAcoholConsumption)) (v studentAcoholConsumption) (x studentAcoholConsumption) (sexFlag (u studentAcoholConsumption))

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY yxPoint =
    g
        [
            class
                [ "point"
                , case yxPoint.sex of
                    M -> "sex-male"
                    F -> "sex-female"
                    UnknownSex -> "sex-unknown"
                ]
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
        [ style
            []
            [ TypedSvg.Core.text
                """
                .point circle {
                    stroke: #dddddd;
                    fill: #dddddd;
                    stroke-width: 2;
                    stroke-opacity: 0.3;
                    fill-opacity: 0.05;
                    transition: fill 0.2s ease, border 0.1s ease;
                }

                .point.sex-male circle {
                    stroke: #55bfff;
                    fill: #55bfff;
                }

                .point.sex-female circle {
                    stroke: #ff455f;
                    fill: #ff455f;
                }

                .point text {
                    font-family: "Inter Tight", sans-serif;
                    fill: #000;
                    text-shadow: 1px 1px 4px #fff, 1px -1px 4px #fff, -1px 1px 4px #fff, -1px -1px 4px #fff;
                    visibility: hidden;
                    opacity: 0;
                    transition: opacity 0.3s ease;
                }

                .point:hover circle {
                    stroke-opacity: 1;
                    fill-opacity: 1;
                }

                .point:hover text {
                    visibility: visible;
                    opacity: 1;
                }
                """
            ]
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


stylesheet : Html.Html Msg
stylesheet =
  let
    styles = 
        """
        #scatterplot-nav {
            display: flex;
            flex-wrap: wrap;
            gap: 1em 3em;
            margin: -1.5em -1em 1em -1em;
            padding: 1em; 
            padding-top: 1.5em; 
            background: #f8f8f8;
            border-bottom: 1px solid #dddddd;
        }

        #scatterplot-nav > span {
            flex: 0 0 100%;
        }

        #scatterplot-nav > form {
            display: flex;
            flex: 1;
            gap: 1em;
        }

        #scatterplot-nav > form > label {
            flex: 0 33%;
        }

        #scatterplot-nav > form > select {
            flex: 1;
        }
        """
  in
    Html.node "style" [] [ Html.text styles ]

change : ((StudentAcoholConsumption -> Float, String) -> Msg) -> String -> Msg
change msg value =
    case value of
        "Mathematik (10. Kl.)" -> msg (.firstperiodGradeMath, "Mathematik (10. Kl.)")
        "Mathematik (11. Kl.)" -> msg (.secondperiodGradeMath, "Mathematik (11. Kl.)")
        "Mathematik (12. Kl.)" -> msg (.thirdperiodGradeMath, "Mathematik (12. Kl.)")
        "Portugiesisch (10. Kl.)" -> msg (.firstperiodGradePort, "Portugiesisch (10. Kl.)")
        "Portugiesisch (11. Kl.)" -> msg (.secondperiodGradePort, "Portugiesisch (11. Kl.)")
        "Portugiesisch (12. Kl.)" -> msg (.thirdperiodGradePort, "Portugiesisch (12. Kl.)")
        "Alkoholkonsum (Wochentag)" -> msg (.dalc, "Alkoholkonsum (Wochentag)")
        "Alkoholkonsum (Wochenende)" -> msg (.walc, "Alkoholkonsum (Wochenende)")
        _ -> msg (.dalc, "Alkoholkonsum (Wochentag)")

nav : Data -> Html Msg
nav data = Html.nav
    [ Html.Attributes.id "scatterplot-nav" ]
    [ Html.span [] [ Html.text "Wechseln Sie die X- und Y-Achsen, um verschiedene Kombinationen zu erkunden." ]
    , Html.form
        []
        [ Html.label [] [ Html.text "X-Achse:" ]
        , Html.select
            [ Html.Events.onInput (change ChangeX) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Mathematik-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Mathematik (10. Kl.)"
                    , Html.Attributes.selected (data.xName == "Mathematik (10. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (11. Kl.)"
                    , Html.Attributes.selected (data.xName == "Mathematik (11. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (12. Kl.)"
                    , Html.Attributes.selected (data.xName == "Mathematik (12. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Portugiesisch-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Portugiesisch (10. Kl.)"
                    , Html.Attributes.selected (data.xName == "Portugiesisch (10. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (11. Kl.)"
                    , Html.Attributes.selected (data.xName == "Portugiesisch (11. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (12. Kl.)"
                    , Html.Attributes.selected (data.xName == "Portugiesisch (12. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Alkoholkonsum" ] 
                [   Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochentag)"
                    , Html.Attributes.selected (data.xName == "Alkoholkonsum (Wochentag)") ]
                    [ Html.text "Alkoholkonsum am Wochentag" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochenende)"
                    , Html.Attributes.selected (data.xName == "Alkoholkonsum (Wochenende)") ]
                    [ Html.text "Alkoholkonsum am Wochenende" ]
                ]
        ]
    ]
    , Html.form
        []
        [ Html.label [] [ Html.text "Y-Achse:" ]
        , Html.select
            [ Html.Events.onInput (change ChangeY) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Mathematik-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Mathematik (10. Kl.)"
                    , Html.Attributes.selected (data.yName == "Mathematik (10. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (11. Kl.)"
                    , Html.Attributes.selected (data.yName == "Mathematik (11. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (12. Kl.)"
                    , Html.Attributes.selected (data.yName == "Mathematik (12. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Portugiesisch-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Portugiesisch (10. Kl.)"
                    , Html.Attributes.selected (data.yName == "Portugiesisch (10. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (11. Kl.)"
                    , Html.Attributes.selected (data.yName == "Portugiesisch (11. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (12. Kl.)"
                    , Html.Attributes.selected (data.yName == "Portugiesisch (12. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Alkoholkonsum" ] 
                [   Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochentag)"
                    , Html.Attributes.selected (data.yName == "Alkoholkonsum (Wochentag)") ]
                    [ Html.text "Alkoholkonsum am Wochentag" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochenende)"
                    , Html.Attributes.selected (data.yName == "Alkoholkonsum (Wochenende)") ]
                    [ Html.text "Alkoholkonsum am Wochenende" ]
                ]
            ]
        ]
    ]



view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Unfortunately scatterplot StudentAcoholConsumption can not be open."

        Loading ->
            Html.span
                []
                [ Html.text "Lade  Scatterplot zum Alkoholkonsum von Schülern... "
                , FontAwesome.view (FontAwesome.styled [ FontAwesome.Attributes.spin ] FontAwesome.Solid.spinner)
                ]


        Success l ->
            let
                studentAcoholConsumption =
                    filterReducedStudentAcoholConsumption l.data .sex l.xFunction l.yFunction l.xName l.yName

            
                    ,   scatterplot studentAcoholConsumption
                ]

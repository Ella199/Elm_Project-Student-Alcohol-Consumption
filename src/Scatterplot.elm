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
import Util exposing (..)

type alias Data =
    { data : List StudentAcoholConsumption
    , xFunction : StudentAcoholConsumption -> Float
    , yFunction : StudentAcoholConsumption -> Float
    , xName : String 
    , yName : String
    , chosendata : Maybe StudentAcoholConsumption
    }

type Model
 = Error
 | Loading
 | Success Data


type alias StudentAcoholConsumption =
    { sex : Sex
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
    | PointChosen StudentAcoholConsumption



type alias Point = 
    { pointName : String, x : Float, y : Float, sex: Sex}
type alias XYData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    , chosendata : Maybe Point
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
            (Csv.Decode.field "sex" (\s -> Result.fromMaybe "sex not ok" (Just (sexFlag s)))
                
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
                    ( Success <| { data = studentAcoholConsumptionList [ fullText ], xFunction = .thirdperiodGradeMath, yFunction = .dalc, xName = "Mathematik 12. Kl.", yName = "Alkoholkonsum (Wochentag)", chosendata = Nothing}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = x, yFunction = m.yFunction, xName = a, yName = m.yName , chosendata = m.chosendata}, Cmd.none )
                    --( Success <| {m.data| xFunction=x, xName=a}, Cmd.none)

                _ ->
                    ( model, Cmd.none )
        ChangeY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = m.xFunction, yFunction = y, xName = m.xName, yName = a, chosendata = m.chosendata }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        PointChosen sac ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xFunction = m.xFunction, yFunction = m.yFunction, xName = m.xName, yName = m.yName, chosendata = Just sac }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



studentAcoholConsumptionList :List String -> List StudentAcoholConsumption
studentAcoholConsumptionList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat

{- filterReducedStudentAcoholConsumption : List StudentAcoholConsumption -> Maybe StudentAcoholConsumption -> (StudentAcoholConsumption -> String) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption->Float) -> String -> String -> XYData 

filterReducedStudentAcoholConsumption studentAcoholConsumptionsliste mchosen a b c x y =
    XYData x y (List.map (\n -> pointName n a b c x y) studentAcoholConsumptionsliste) (Maybe.map (\n -> pointName n a b c x y ) mchosen)
 -}
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

sexLabel : Sex -> String
sexLabel sex =
    case sex of
        M ->
            "männlich"

        F ->
            "weiblich"

        _ ->
            "unbekannt"


sexFlag : String -> Sex
sexFlag sex =
    case sex of
        "M" ->
            M

        "F" ->
            F

        _ ->
            UnknownSex



{- pointName : StudentAcoholConsumption -> (StudentAcoholConsumption -> String) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> String -> String -> Point
   pointName studentAcoholConsumption u v x y z =
       Point (sexLabel (u studentAcoholConsumption) ++ ", " ++ y ++ ": " ++ String.fromFloat (v studentAcoholConsumption) ++ ", " ++ z ++ ": " ++ String.fromFloat (x studentAcoholConsumption)) (v studentAcoholConsumption) (x studentAcoholConsumption) (sexFlag (u studentAcoholConsumption))
-}


point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY yxPoint =
    g
        [ class
            [ "point"
            , case yxPoint.sex of
                M ->
                    "sex-male"

                F ->
                    "sex-female"

                UnknownSex ->
                    "sex-unknown"
            ]
        , fontSize <| Px 15.0

        {- ,transform
           [
               Translate
               --(Scale.convert scaleX yxPoint.x)
               --(Scale.convert scaleY yxPoint.y)
           ]
        -}
        ]
        [ circle [ cx (Scale.convert scaleX yxPoint.x), cy (Scale.convert scaleY yxPoint.y), r 5 ] []
        , text_ [ x ((w / 4) + 0), y -20 ] [ Html.text yxPoint.pointName ]
        ]


drawpoint : ContinuousScale Float -> ContinuousScale Float -> (StudentAcoholConsumption -> Float) -> String -> (StudentAcoholConsumption -> Float) -> String -> StudentAcoholConsumption -> Svg Msg
drawpoint scaleX scaleY xfunc xname yfunc yname sac =
    g
        [ class
            [ "point"
            , case sac.sex of
                M ->
                    "sex-male"

                F ->
                    "sex-female"

                UnknownSex ->
                    "sex-unknown"
            ]
        , fontSize <| Px 15.0

        {- ,transform
           [
               Translate
               (Scale.convert scaleX (xfunc sac))
               (Scale.convert scaleY (yfunc sac))
           ]
        -}
        ]
        [ circle [ cx (Scale.convert scaleX (xfunc sac)), cy (Scale.convert scaleY (yfunc sac)), r 5 ] []
        , text_ [ x ((w / 4) + 0), y -20 ] [ Html.text (sexLabel sac.sex ++ ", " ++ xname ++ ": " ++ String.fromFloat (xfunc sac) ++ ", " ++ yname ++ ": " ++ String.fromFloat (yfunc sac)) ]
        ]


drawChosenpoint : ContinuousScale Float -> ContinuousScale Float -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> Maybe StudentAcoholConsumption -> String -> String -> List (Svg Msg)
drawChosenpoint scaleX scaleY xfunc yfunc msac xname yname =
    case msac of
        Nothing ->
            []

        Just sac ->
            [ g
                [ class
                    [ "cpoint"
                    , case sac.sex of
                        M ->
                            "sex-male"

                        F ->
                            "sex-female"

                        UnknownSex ->
                            "sex-unknown"
                    ]
                , fontSize <| Px 15.0

                {- ,transform
                   [
                       Translate
                       (Scale.convert scaleX (xfunc sac))
                       (Scale.convert scaleY (yfunc sac))
                   ]
                -}
                ]
                [ circle [ cx (Scale.convert scaleX (xfunc sac)), cy (Scale.convert scaleY (yfunc sac)), r 5 ] []
                , text_ [ x ((w / 4) + 0), y -20 ] [] --[ Html.text (sexLabel sac.sex ++ ", " ++ xname ++ ": " ++ String.fromFloat (xfunc sac) ++ ", " ++ yname ++ ": " ++ String.fromFloat (yfunc sac)) ]
                ]
            ]



scatterplot : Data -> Svg Msg
scatterplot model =
    let
        xValues : List Float
        xValues =
            List.map model.xFunction model.data

        yValues : List Float
        yValues =
            List.map model.yFunction model.data

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

                .Cpoint circle {
                    stroke: #000000;
                    fill: #000000;
                    stroke-width: 0;
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
                    fill: #000000;
                    stroke-width: 4;
                    text-shadow: 1px 1px 4px #fff, 1px -1px 4px #fff, -1px 1px 4px #fff, -1px -1px 4px #fff;
                    visibility: hidden;
                    opacity: 0;
                    transition: opacity 0.s ease;
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
                , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.xName ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30
                , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)
                ]
                [ TypedSvg.Core.text model.yName ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (drawChosenpoint xScaleLocal yScaleLocal model.xFunction model.yFunction model.chosendata model.xName model.yName
                ++ List.map (drawpoint xScaleLocal yScaleLocal model.xFunction model.xName model.yFunction model.yName) model.data
            )
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
            Html.text "Leider konnte der Scatterplot zum Alkoholkonsum von Schülern nicht geladen werden."

        Loading ->
            Html.span
                []
                [ Html.text "Lade  Scatterplot zum Alkoholkonsum von Schülern... "
                , FontAwesome.view (FontAwesome.styled [ FontAwesome.Attributes.spin ] FontAwesome.Solid.spinner)
                ]

        Success l ->
            --let
              --  studentAcoholConsumption : XYData
              --  studentAcoholConsumption =
              --      filterReducedStudentAcoholConsumption l.data .sex l.xFunction l.yFunction l.xName l.yName

            --in 
            Html.div
                []
                [ stylesheet
                , nav l
                , scatterplot l
                ]

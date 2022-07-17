module Test.ScatterplotTEST exposing (..)

import Axis
import Html exposing (Html,text, pre)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser

main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type Model
  = Failure
  | Loading
  | Success (List String)

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , liste
        |> List.map
        (\data ->
                Http.get
                    { url = "https://raw.githubusercontent.com/Ella199/Elm_Project-Student-Alcohol-Consumption/main/Data/CSV_Daten/" ++ data
                    , expect = Http.expectString GotText
                    }
            )
        |> Cmd.batch
    )  
liste : List String 
liste = 
    [ "mergedstudent_withoutline.csv" ]
csvString_to_data : String -> List (String, Maybe Float, Maybe Float)
csvString_to_data csvR = 
    Csv.parse csvR
        |> Csv.Decode.decodeCsv decodeStudentAcoholConsumption
        |> Result.toMaybe
        |> Maybe.withDefault []

decodeStudentAcoholConsumption : Csv.Decode.Decoder (( String, Maybe Float, Maybe Float ) -> a) a
decodeStudentAcoholConsumption = 
    Csv.Decode.map (\a b c-> ( a, Just b, Just c ))
        (Csv.Decode.field "sex" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "thirdperiodGradePort"
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                    |> Csv.Decode.andMap
                        (Csv.Decode.field "dalc"
                            (String.toFloat >> Result.fromMaybe "error parsing string")
                                
                        )
                )
        )
change : List (String, Maybe Float, Maybe Float) -> List (String, String, String)
change wholeText = 
    List.map (\( a, b, c ) -> ( a, b |> Maybe.map String.fromFloat |> Maybe.withDefault "No value available", c |> Maybe.map String.fromFloat |> Maybe.withDefault "No value available")) wholeText

change2 : List (String, Maybe Float, Maybe Float) -> List (String, Float, Float)
change2 wholeText = 
    List.map (\( a, b, c ) -> (a, b |> Maybe.withDefault 0.0, c |> Maybe.withDefault 0.0)) wholeText
    
-- ADD UPDATE 
renderList : List (String, String, String) -> Html msg
renderList lst =
    Html.ul []
        (List.map (\( a, b, c ) -> Html.li [] [ Html.text <| a ++ ", " ++ b ++ ", " ++ c ]) lst)

type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentList =
            case model of
                Success l ->
                    l

                Failure ->
                    []

                Loading ->
                    []
    in
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| currentList ++ [ fullText ], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

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


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


scatterplot : XyData -> Svg msg
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

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                , y 35
                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text "thirdperiodGradePort" ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text "dalc" ]
            
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]
        

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    g
        [
            class["point"]
            ,fontSize <| Px 15.0
            ,fontFamily ["serif"]
            ,transform
                [
                    Translate
                    (Scale.convert scaleX xyPoint.x)
                    (Scale.convert scaleY xyPoint.y)
                ]
        ]

        [
            circle [cx 0, cy 0, r 5] []
            , text_ [x 10, y -20, textAnchor AnchorMiddle] [Html.text xyPoint.pointName]
        ]


type alias Point =
    { pointName : String, x : Float, y : Float }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )
 
add : (Float, Float) -> Float-> (Float, Float) 
add (min, max) shift =
    if min <= 0 then
        ( 0, max + shift)
    else 
        (min - shift, max + shift)
    
 
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
            add result (toFloat(tickCount)*max/50)
        
        result2 = 
            add result1 (0.0)
        
          
    in
     result2

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

filterAndReduceStudentAcoholConsumption : List (String, Float, Float) -> XyData
filterAndReduceStudentAcoholConsumption theStudentAcoholConsumptionListe =
    XyData "thirdperiodGradePort" "dalc" (List.map pointName theStudentAcoholConsumptionListe)

pointName : (String, Float, Float) -> Point
pointName (x, y, z) =
    Point x y z


-- VIEW
view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "I cant open StudentAcoholConsumption"
        Loading ->
            Html.text "Loading StudentAcoholConsumption"
        Success l ->
            let 
                studentAcoholConsumption =
                                        filterAndReduceStudentAcoholConsumption <| studentAcoholConsumptionListe l
             in
            Html.div []
                [
                    scatterplot studentAcoholConsumption
                ]

studentAcoholConsumptionListe : List String -> List(String, Float, Float) 
studentAcoholConsumptionListe liste1 =
 List.map (\fulltext ->  change2 <| csvString_to_data fulltext ) liste1
    |> List.concat






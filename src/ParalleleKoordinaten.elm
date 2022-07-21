module ParalleleKoordinaten exposing (..)
import Axis
import Browser
import Color
import Csv.Decode
import TypedSvg exposing (path)
import Html exposing (Html, a, li, ul)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape 
import Statistics 
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox, class)
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Tuple exposing (second)
import Scatterplot exposing (Msg(..))
import Csv exposing (Csv)

type Model
  = Error
  | Loading
  | Success
    { data : List StudentAcoholConsumption
    , firstFUNCTION : StudentAcoholConsumption -> Float
    , secondFUNCTION : StudentAcoholConsumption -> Float
    , thirdFUNCTION : StudentAcoholConsumption -> Float
    , fourthFUNCTION : StudentAcoholConsumption -> Float
    , firstNAME : String
    , secondNAME : String
    , thirdNAME : String
    , fourthNAME : String
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

type alias MultiDimDATA = 
    { dimDescription : List String
    , data : List (ListMultiDimPoint)
    }

type alias MultiDimPOINT =
    { pointName : String, value : List Float }

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


type MSG 
    = GotText (Result Http.Error String)   
    | ChangONE (StudentAcoholConsumption -> Float,String)     
    | ChangeTWO (StudentAcoholConsumption -> Float,String) 
    | ChangeTHREE (StudentAcoholConsumption -> Float,String)
    | ChangeFOUR (StudentAcoholConsumption -> Float,String)
    
csvStringToData : String -> List StudentAcoholConsumption
csvStringToData csvR = 
    Csv.parse csvR
        |> Csv.Decode.decodeCsv decodingStudentAcoholConsumption
        |> Result.toMaybe
        |> Maybe.withDefault []

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = StudentAcoholConsumptionList [ fullText ], firstFUNCTION = .firstperiodGradeMath, secondFUNCTION = .secondperiodGradeMath, thirdFUNCTION = .thirdperiodGradeMath, fourthFUNCTION = firstperiodGradePort, firstNAME = "first period Grade mathematics", secondNAME = "second period Grade mathematics", thirdNAME = "third period Grade mathematics", fourthNAME = "first period Grade portuguese"}, Cmd.none )

                Err  _ ->
                    ( model, Cmd.none )

        ChangeONE (x, a) ->
            case model of 
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = x, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = m.fourthFUNCTION, firstNAME = x, secondNAME = m.secondNAME, thirdNAME = m.thirdNAME, fourthNAME = m.fourthNAME}, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        ChangeTWO (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = m.firstFUNCTION, secondFUNCTION = y, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = m.fourthFUNCTION , firstNAME = m.firstNAME, secondNAME = a, thirdNAME = m.thirdNAME, fourthNAME = m.fourthNAME}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeTHREE (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = m.firstFUNCTION, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = z, fourthFUNCTION = m.fourthFUNCTION, firstNAME = m.firstNAME, secondNAME = m.secondNAME, thirdNAME = a, fourthNAME = m.fourthNAME}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeFOUR (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = m.firstFUNCTION, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = m.thirdFUNCTION, fourthFFUNCTION = c , firstNAME = m.firstNAME, secondNAME = m.secondNAME, thirdNAME = m.thirdNAME, fourthNAME = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )     

studentAcoholConsumptionList :List String -> List StudentAcoholConsumption    
studentAcoholConsumptionList List1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat            

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

padding : Float
padding =
    60

radius : Float
radius =
    5.0

tickCount : Int
tickCount =
    8

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extent =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount)
    in
    ( Tuple.first closeExtent - extent |> max 0
    , Tuple.second closeExtent + extent
    )

parallelCoordinatesPlot : Float -> Float -> MultiDimData -> Svg msg
parallelCoordinatesPlot w ar model =
    let
        h : Float
        h =
            w / ar

        listTransformieren : List (List Float)
        listTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listWideExtent : List ( Float, Float )
        listWideExtent =
            listTransformieren |> List.map wideExtent

        listScale =
            List.map (Scale.linear ( h, 0 )) listWideExtent

        listAxis =
            List.map (Axis.left [ Axis.tickCount tickCount ]) listScale

        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style []
            [
                TypedSvg.Core.text """
                .parallelPoint { stroke: rgba(1, 0, 0,0.2);}
                .parallelPoint:hover {stroke: rgb(60, 179, 113); stroke-width: 2;} 
                .parallelPoint text { display: none; }
                .parallelPoint:hover text { display: inline; stroke: rgb(0, 0, 0); stroke-width: 0.1; font-size: small; font-family: calibri}  
                """
            ]
        , g [ TypedSvg.Attributes.class [ "parallelAxis" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xScale (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "Times New Roman" ]
                            , fontSize (Px 12)
                            , x <| Scale.convert xScale (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    drawPoint p name description =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listScale
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        g [class ["parallelPoint"]][
                            Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            , class ["parallelPoint"]
                            ]
                            , text_
                                [ x 300
                                , y -20
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p description)))]
                                
                        ]
                        
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (\a -> drawPoint a.value a.pointName model.dimDescription) dataset)
                        )
               )
view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Unfortunately scatterplot StudentAcoholConsumption can not be open."

        Loading ->
            Html.text "Loading StudentAcoholConsumption"
        Success l ->
                    let
                        multiDimDaten : List StudentAcoholConsumption -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> String) -> String -> String -> String -> String-> MultiDimData
                        multiDimDaten listStudentAcoholConsumption a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPoint (e x)
                                )
                                listStudentAcoholConsumption
                            ]

                        plotData = 
                            multiDimDaten l.data l.firstFUNCTION l.secondFUNCTION l.thirdFUNCTION l.fourthFUNCTION .name l.firstNAME l.secondName l.thirdNAME l.fourthNAME

                            in
                    Html.div []
                        [
                            ul[][
                                li[][
                                    Html.text <| "Please select a data type from the first axis to get interesting outups about the topic. "
                                    , Html.button [onClick (ChangeONE (.firstperiodGradeMath, "first period Grade mathematics"))][Html.text "first period Grade mathematics"]
                                    , Html.button [onClick (ChangeONE (.secondperiodGradeMath, "second period Grade mathematics"))][Html.text "second period Grade mathematics"]
                                    , Html.button [onClick (ChangeONE (.thirdperiodGradeMath, "third period Grade mathematics"))][Html.text "third period Grade mathematics"]
                                    , Html.button [onClick (ChangeONE (.firstperiodGradePort, "first period Grade portuguese"))][Html.text "first period Grade portuguese"]
                                    , Html.button [onClick (ChangeONE (.secondperiodGradePort, "second period Grade portuguese"))][Html.text "second period Grade portuguese"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Please select a data type from the first axis to get interesting outups about the topic. "
                                    , Html.button [onClick (ChangeTWO (.first period Grade mathematics, "first period Grade mathematics"))][Html.text "first period Grade mathematics"]
                                    , Html.button [onClick (ChangeTWO (.secondperiodGradeMath, "second period Grade mathematics"))][Html.text "second period Grade mathematics"]
                                    , Html.button [onClick (ChangeTWO (.thirdperiodGradeMath, "third period Grade mathematics"))][Html.text "third period Grade mathematics"]
                                    , Html.button [onClick (ChangeTWO (.firstperiodGradePort, "first period Grade portuguese"))][Html.text "first period Grade portuguese"]
                                    , Html.button [onClick (ChangeTWO (.secondperiodGradePort, "second period Grade portuguese"))][Html.text "second period Grade portuguese"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Please select a data type from the first axis to get interesting outups about the topic. "
                                    , Html.button [onClick (ChangeTHREE (.first period Grade mathematics, "first period Grade mathematics"))][Html.text "first period Grade mathematics"]
                                    , Html.button [onClick (ChangeTHREE (.secondperiodGradeMath, "second period Grade mathematics"))][Html.text "second period Grade mathematics"]
                                    , Html.button [onClick (ChangeTHREE (.thirdperiodGradeMath, "third period Grade mathematics"))][Html.text "third period Grade mathematics"]
                                    , Html.button [onClick (ChangeTHREE (.firstperiodGradePort, "first period Grade portuguese"))][Html.text "first period Grade portuguese"]
                                    , Html.button [onClick (ChangeTHREE (.secondperiodGradePort, "second period Grade portuguese"))][Html.text "second period Grade portuguese"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Please select a data type from the first axis to get interesting outups about the topic. "
                                    , Html.button [onClick (ChangeFOUR (.first period Grade mathematics, "first period Grade mathematics"))][Html.text "first period Grade mathematics"]
                                    , Html.button [onClick (ChangeFOUR (.secondperiodGradeMath, "second period Grade mathematics"))][Html.text "second period Grade mathematics"]
                                    , Html.button [onClick (ChangeFOUR (.thirdperiodGradeMath, "third period Grade mathematics"))][Html.text "third period Grade mathematics"]
                                    , Html.button [onClick (ChangeFOUR (.firstperiodGradePort, "first period Grade portuguese"))][Html.text "first period Grade portugueser"]
                                    , Html.button [onClick (ChangeFOUR (.secondperiodGradePort, "second period Grade portuguese"))][Html.text "second period Grade portuguese"]
                                ]
                             ]
                                , parallelCoordinatesPlot 600 2 plotData
                        ]

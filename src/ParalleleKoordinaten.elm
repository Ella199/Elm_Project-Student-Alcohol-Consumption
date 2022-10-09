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
import Csv exposing (Csv)
import Html.Attributes
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Attributes
import Json.Decode

type alias Data =
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

type Sex
    = M
    | F
    | UnknownSex

type alias MultiDimPoint =
    { pointName : String, sex: Sex, value : List Float }


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


type Msg 
    = GotText (Result Http.Error String)   
    | ChangeONE (StudentAcoholConsumption -> Float,String)     
    | ChangeTWO (StudentAcoholConsumption -> Float,String) 
    | ChangeTHREE (StudentAcoholConsumption -> Float,String)
    | ChangeFOUR (StudentAcoholConsumption -> Float,String)
    | MoveAxisUp Int
    
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
                    ( Success <| { data = studentAcoholConsumptionList [ fullText ], firstFUNCTION = .firstperiodGradeMath, secondFUNCTION = .secondperiodGradeMath, thirdFUNCTION = .thirdperiodGradeMath, fourthFUNCTION = .dalc, firstNAME = "Mathematik (10. Kl.)", secondNAME = "Mathematik (11. Kl.)", thirdNAME = "Mathematik (12. Kl.)", fourthNAME = "Alkoholkonsum (Wochentag)"}, Cmd.none )

                Err  _ ->
                    ( model, Cmd.none )

        ChangeONE (x, a) ->
            case model of 
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = x, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = m.fourthFUNCTION, firstNAME = a, secondNAME = m.secondNAME, thirdNAME = m.thirdNAME, fourthNAME = m.fourthNAME}, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        ChangeTWO (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFUNCTION = m.firstFUNCTION, secondFUNCTION = y, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = m.fourthFUNCTION , firstNAME = m.firstNAME, secondNAME = a, thirdNAME = m.thirdNAME, fourthNAME = m.fourthNAME}, Cmd.none )

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
                    ( Success <| { data = m.data, firstFUNCTION = m.firstFUNCTION, secondFUNCTION = m.secondFUNCTION, thirdFUNCTION = m.thirdFUNCTION, fourthFUNCTION = c , firstNAME = m.firstNAME, secondNAME = m.secondNAME, thirdNAME = m.thirdNAME, fourthNAME = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )   
         MoveAxisUp num ->
            case model of
                Success m ->
                    case num of
                        2 ->
                            ( Success <| { m | firstFUNCTION = m.secondFUNCTION, secondFUNCTION = m.firstFUNCTION, firstNAME = m.secondNAME, secondNAME = m.firstNAME}, Cmd.none )
                        3 ->
                            ( Success <| { m | secondFUNCTION = m.thirdFUNCTION, thirdFUNCTION = m.secondFUNCTION, secondNAME = m.thirdNAME, thirdNAME = m.secondNAME}, Cmd.none )
                        4 ->
                            ( Success <| { m | thirdFUNCTION = m.fourthFUNCTION, fourthFUNCTION = m.thirdFUNCTION, thirdNAME = m.fourthNAME, fourthNAME = m.thirdNAME}, Cmd.none )
                        _ ->
                            ( model, Cmd.none )     
                _ ->
                    ( model, Cmd.none ) 

studentAcoholConsumptionList :List String -> List StudentAcoholConsumption    
studentAcoholConsumptionList list1 =
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
        [ viewBox 0 0 (w + 3 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style
            []
            [ TypedSvg.Core.text
                """
                .parallelPoint path {
                    stroke: #dddddd;
                    stroke-width: 2;
                    stroke-opacity: 0.1;
                    transition: stroke 0.1s ease;
                }

                .parallelPoint.sex-male path {
                    stroke: #55bfff;
                }

                .parallelPoint.sex-female path {
                    stroke: #ff455f;
                }

                .parallelPoint:hover path {
                    stroke-opacity: 1;
                }

                .parallelPoint text {
                    font-family: "Inter Tight", sans-serif;
                    font-size: small;
                    fill: #000;
                    stroke: none;
                    visibility: hidden;
                    opacity: 0;
                    transition: opacity 0.15s ease;
                }

                .parallelPoint:hover text {
                    visibility: visible;
                    opacity: 1;
                }  
                """
            ]
        , g []
            [ g [ transform [ Translate (padding*2)  padding ] ] <|
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
            , g [ transform [ Translate (padding*2) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "Times New Roman" ]
                            , fontSize (Px 10)
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
                    drawPoint p name sex description =
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
                        g [class 
                            [ "parallelPoint"
                            , case sex of
                                M -> "sex-male"
                                F -> "sex-female"
                                UnknownSex -> "sex-unknown"
                            ]
                          ][
                            Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            ]
                            , text_
                                [ x 300
                                , y -50
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                , fontSize <| Px 2
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p description)))]
                                
                        ]
                        
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding*2) padding ] ]
                                (List.map (\a -> drawPoint a.value a.pointName a.sex model.dimDescription) dataset)
                        )
               )
stylesheet : Html.Html Msg
stylesheet =
  let
    styles = 
        """
        #parallel-nav {
            display: flex;
            flex-direction: column;
            gap: 1em;
            margin: -1.5em -1em 1em -1em;
            padding: 1em; 
            padding-top: 1.5em; 
            background: #f8f8f8;
            border-bottom: 1px solid #dddddd;
        }

        #parallel-nav > span {
            flex: 0 0 100%;
        }

        #parallel-nav > form {
            display: flex;
            gap: 3em;
        }

        #parallel-nav > form > fieldset {
            display: flex;
            gap: 0.5em;
            border: none;
            padding: 0;
        }

        #parallel-nav > form > fieldset > button {
            border-radius: 50%;
            background: #55bfff3b;
            border: 2px solid #55bfff;
            transition: background 0.2s ease;
        }

        #parallel-nav > form > fieldset > button[disabled] {
            opacity: 0.2;
        }

        #parallel-nav > form > fieldset > button:hover {
            background: #55bfff;
        }

        #parallel-nav > form > label {
            flex: 0 8%;
        }

        #parallel-nav > form > select {
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
    [ Html.Attributes.id "parallel-nav" ]
    [ Html.span [] [ Html.text "Wechseln Sie die 4 Achsen, um verschiedene Zusammenhänge in den Parallelen Koordinaten zu erkunden." ]
    , Html.form
        []
        [ Html.label [] [ Html.text "1. Achse:" ]
        , Html.fieldset
            []
            [ Html.button
                [ Html.Attributes.disabled True
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ onClick (MoveAxisUp 2)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            [ Html.Events.onInput (change ChangeONE) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Mathematik-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Mathematik (10. Kl.)"
                    , Html.Attributes.selected (data.firstNAME == "Mathematik (10. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (11. Kl.)"
                    , Html.Attributes.selected (data.firstNAME == "Mathematik (11. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (12. Kl.)"
                    , Html.Attributes.selected (data.firstNAME == "Mathematik (12. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Portugiesisch-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Portugiesisch (10. Kl.)"
                    , Html.Attributes.selected (data.firstNAME == "Portugiesisch (10. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (11. Kl.)"
                    , Html.Attributes.selected (data.firstNAME == "Portugiesisch (11. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (12. Kl.)"
                    , Html.Attributes.selected (data.firstNAME == "Portugiesisch (12. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Alkoholkonsum" ] 
                [   Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochentag)"
                    , Html.Attributes.selected (data.firstNAME == "Alkoholkonsum (Wochentag)") ]
                    [ Html.text "Alkoholkonsum am Wochentag" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochenende)"
                    , Html.Attributes.selected (data.firstNAME == "Alkoholkonsum (Wochenende)") ]
                    [ Html.text "Alkoholkonsum am Wochenende" ]
                ]
        ]
    ]
    , Html.form
        []
        [ Html.label [] [ Html.text "2. Achse:" ]
        , Html.fieldset
            []
            [ Html.button
                [ onClick (MoveAxisUp 2)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ onClick (MoveAxisUp 3)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            [ Html.Events.onInput (change ChangeTWO) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Mathematik-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Mathematik (10. Kl.)"
                    , Html.Attributes.selected (data.secondNAME == "Mathematik (10. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (11. Kl.)"
                    , Html.Attributes.selected (data.secondNAME == "Mathematik (11. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (12. Kl.)"
                    , Html.Attributes.selected (data.secondNAME == "Mathematik (12. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Portugiesisch-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Portugiesisch (10. Kl.)"
                    , Html.Attributes.selected (data.secondNAME == "Portugiesisch (10. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (11. Kl.)"
                    , Html.Attributes.selected (data.secondNAME == "Portugiesisch (11. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (12. Kl.)"
                    , Html.Attributes.selected (data.secondNAME == "Portugiesisch (12. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Alkoholkonsum" ] 
                [   Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochentag)"
                    , Html.Attributes.selected (data.secondNAME == "Alkoholkonsum (Wochentag)") ]
                    [ Html.text "Alkoholkonsum am Wochentag" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochenende)"
                    , Html.Attributes.selected (data.secondNAME == "Alkoholkonsum (Wochenende)") ]
                    [ Html.text "Alkoholkonsum am Wochenende" ]
                ]
            ]
        ]
    , Html.form
        []
        [ Html.label [] [ Html.text "3. Achse:" ]
        , Html.fieldset
            []
            [ Html.button
                [ onClick (MoveAxisUp 3)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ onClick (MoveAxisUp 4)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            [ Html.Events.onInput (change ChangeTHREE) ]
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Mathematik-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Mathematik (10. Kl.)"
                    , Html.Attributes.selected (data.thirdNAME == "Mathematik (10. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (11. Kl.)"
                    , Html.Attributes.selected (data.thirdNAME == "Mathematik (11. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (12. Kl.)"
                    , Html.Attributes.selected (data.thirdNAME == "Mathematik (12. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Portugiesisch-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Portugiesisch (10. Kl.)"
                    , Html.Attributes.selected (data.thirdNAME == "Portugiesisch (10. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (11. Kl.)"
                    , Html.Attributes.selected (data.thirdNAME == "Portugiesisch (11. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (12. Kl.)"
                    , Html.Attributes.selected (data.thirdNAME == "Portugiesisch (12. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Alkoholkonsum" ] 
                [   Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochentag)"
                    , Html.Attributes.selected (data.thirdNAME == "Alkoholkonsum (Wochentag)") ]
                    [ Html.text "Alkoholkonsum am Wochentag" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochenende)"
                    , Html.Attributes.selected (data.thirdNAME == "Alkoholkonsum (Wochenende)") ]
                    [ Html.text "Alkoholkonsum am Wochenende" ]
                ]
            ]
        ]

    , Html.form
        []
        [ Html.label [] [ Html.text "4. Achse:" ]
        , Html.fieldset
            [ Html.Events.onInput (change ChangeFOUR) ]
            [ Html.button
                [ onClick (MoveAxisUp 4)
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowUp ]
            , Html.button
                [ Html.Attributes.disabled True
                , Html.Attributes.type_ "button"
                ]
                [ FontAwesome.view FontAwesome.Solid.arrowDown ]
            ]
        , Html.select
            []
            [ Html.optgroup 
                [ Html.Attributes.attribute "label" "Mathematik-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Mathematik (10. Kl.)"
                    , Html.Attributes.selected (data.fourthNAME == "Mathematik (10. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (11. Kl.)"
                    , Html.Attributes.selected (data.fourthNAME == "Mathematik (11. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Mathematik (12. Kl.)"
                    , Html.Attributes.selected (data.fourthNAME == "Mathematik (12. Kl.)") ]
                    [ Html.text "Mathematik-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Portugiesisch-Note" ] 
                [ Html.option
                    [ Html.Attributes.value "Portugiesisch (10. Kl.)"
                    , Html.Attributes.selected (data.fourthNAME == "Portugiesisch (10. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 10. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (11. Kl.)"
                    , Html.Attributes.selected (data.fourthNAME == "Portugiesisch (11. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 11. Klasse" ]
                , Html.option
                    [ Html.Attributes.value "Portugiesisch (12. Kl.)"
                    , Html.Attributes.selected (data.fourthNAME == "Portugiesisch (12. Kl.)") ]
                    [ Html.text "Portugiesisch-Note in der 12. Klasse" ]
                ]
            , Html.optgroup 
                [ Html.Attributes.attribute "label" "Alkoholkonsum" ] 
                [   Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochentag)"
                    , Html.Attributes.selected (data.fourthNAME == "Alkoholkonsum (Wochentag)") ]
                    [ Html.text "Alkoholkonsum am Wochentag" ]
                , Html.option
                    [ Html.Attributes.value "Alkoholkonsum (Wochenende)"
                    , Html.Attributes.selected (data.fourthNAME == "Alkoholkonsum (Wochenende)") ]
                    [ Html.text "Alkoholkonsum am Wochenende" ]
                ]
            ]
        ]
    ]
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


view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Unfortunately scatterplot StudentAcoholConsumption can not be open."

        Loading ->
            Html.text "Loading StudentAcoholConsumption"

        Success l ->
                    let
                        multiDimDaten : List StudentAcoholConsumption -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> String) -> String -> String -> String -> String -> MultiDimData
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
                            multiDimDaten l.data l.firstFUNCTION l.secondFUNCTION l.thirdFUNCTION l.fourthFUNCTION .sex l.firstNAME l.secondNAME l.thirdNAME l.fourthNAME       
                    in
                    Html.div []
                        [
                            ul[][
                                li[][
                                    Html.text <| "Select a data type from the first axis."
                                    , Html.button [onClick (ChangeONE (.firstperiodGradeMath, "Math10"))][Html.text "Math10"]
                                    , Html.button [onClick (ChangeONE (.secondperiodGradeMath, "Math11"))][Html.text "Math11"]
                                    , Html.button [onClick (ChangeONE (.thirdperiodGradeMath, "Math12"))][Html.text "Math12"]
                                    , Html.button [onClick (ChangeONE (.firstperiodGradePort, "Port10"))][Html.text "Port10"]
                                    , Html.button [onClick (ChangeONE (.secondperiodGradePort, "Port11"))][Html.text "Port11"]
                                    , Html.button [onClick (ChangeONE (.thirdperiodGradePort, "Port12"))][Html.text "Port12"]
                                    , Html.button [onClick (ChangeONE (.dalc, "workdayAlcohol"))][Html.text "workdayAlcohol"]
                                    , Html.button [onClick (ChangeONE (.walc, "weekendAlcohol"))][Html.text "weekendAlcohol"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Select a data type from the second axis."
                                    , Html.button [onClick (ChangeTWO (.firstperiodGradeMath, "Math10"))][Html.text "Math10"]
                                    , Html.button [onClick (ChangeTWO (.secondperiodGradeMath, "Math11"))][Html.text "Math11"]
                                    , Html.button [onClick (ChangeTWO (.thirdperiodGradeMath, "Math12"))][Html.text "Math12"]
                                    , Html.button [onClick (ChangeTWO (.firstperiodGradePort, "Port10"))][Html.text "Port10"]
                                    , Html.button [onClick (ChangeTWO (.secondperiodGradePort, "Port11"))][Html.text "Port11"]
                                    , Html.button [onClick (ChangeONE (.thirdperiodGradePort, "Port12"))][Html.text "Port12"]
                                    , Html.button [onClick (ChangeONE (.dalc, "workdayAlcohol"))][Html.text "workdayAlcohol"]
                                    , Html.button [onClick (ChangeONE (.walc, "weekendAlcohol"))][Html.text "weekendAlcohol"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Select a data type from the third axis."
                                    , Html.button [onClick (ChangeTHREE (.firstperiodGradeMath, "Math10"))][Html.text "Math10"]
                                    , Html.button [onClick (ChangeTHREE (.secondperiodGradeMath, "Math11"))][Html.text "Math11"]
                                    , Html.button [onClick (ChangeTHREE (.thirdperiodGradeMath, "Math12"))][Html.text "Math12"]
                                    , Html.button [onClick (ChangeTHREE (.firstperiodGradePort, "Port10"))][Html.text "Port10"]
                                    , Html.button [onClick (ChangeTHREE (.secondperiodGradePort, "Port11"))][Html.text "Port11"]
                                    , Html.button [onClick (ChangeONE (.thirdperiodGradePort, "Port12"))][Html.text "Port12"]
                                    , Html.button [onClick (ChangeONE (.dalc, "workdayAlcohol"))][Html.text "workdayAlcohol"]
                                    , Html.button [onClick (ChangeONE (.walc, "weekendAlcohol"))][Html.text "weekendAlcohol"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Select a data type from the fourth axis."
                                    , Html.button [onClick (ChangeFOUR (.firstperiodGradeMath, "Math10"))][Html.text "Math10"]
                                    , Html.button [onClick (ChangeFOUR (.secondperiodGradeMath, "Math11"))][Html.text "Math11"]
                                    , Html.button [onClick (ChangeFOUR (.thirdperiodGradeMath, "Math12"))][Html.text "Math12"]
                                    , Html.button [onClick (ChangeFOUR (.firstperiodGradePort, "Port10"))][Html.text "Port10"]
                                    , Html.button [onClick (ChangeFOUR (.secondperiodGradePort, "Port11"))][Html.text "Port11"]
                                    , Html.button [onClick (ChangeONE (.thirdperiodGradePort, "Port12"))][Html.text "Port12"]
                                    , Html.button [onClick (ChangeONE (.dalc, "workdayAlcohol"))][Html.text "workdayAlcohol"]
                                    , Html.button [onClick (ChangeONE (.walc, "weekendAlcohol"))][Html.text "weekendAlcohol"]
                                ]
                             ]
                                , parallelCoordinatesPlot 600 2 plotData
                        ]

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
import Html.Attributes
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Attributes
import Json.Decode
import Util exposing (..)

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


stylesheet : Html.Html Msg
stylesheet =
  let
    styles = 
        """
        #stickfigure-nav {
            display: flex;
            flex-wrap: wrap;
            align-items: baseline;
            gap: 1em 3em;
            margin: -1.5em -1em 1em -1em;
            padding: 1em; 
            padding-top: 1.5em; 
            background: #f8f8f8;
            border-bottom: 1px solid #dddddd;
        }

        #stickfigure-nav > span {
            flex: 0 0 100%;
        }

        #stickfigure-nav > form {
            display: flex;
            flex: 1;
            gap: 1em;
        }

        #stickfigure-nav > form > label {
            flex: 0 33%;
        }

        #stickfigure-nav > form > select {
            flex: 1;
        }

        #stickfigure-nav > form > .size-selector {
            position: relative;
            flex: 1;
        }

        #stickfigure-nav > form > .size-selector > input {
            width: 100%;
        }

        #stickfigure-nav > form > .size-selector > span {
            position: absolute;
            top: -1.7em;
            left: 50%;
            transform: translateX(-50%);
            background: #fff;
            border-radius: 0.3em;
            padding: 0.3em;
            visibility: hidden;
            opacity: 0;
            transition: opacity 0.2s ease;
            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.4);
        }

        #stickfigure-nav > form > .size-selector:hover > span {
            visibility: visible;
            opacity: 1;
        }
        """
  in
    Html.node "style" [] [ Html.text styles ]


nav : Data -> Html Msg
nav data = Html.nav
    [ Html.Attributes.id "stickfigure-nav" ]
    [ Html.span [] [ Html.text "Wechseln Sie die Klassenstufe, um die Mathematik- und Portugiesisch-Noten mit den weiteren Daten in der Stickfigure-Darstellung zu erkunden. Stellen Sie die Größe der Stickfigures mit dem Regler so ein, dass eine Textur sichtbar wird." ]
    , Html.form
        []
        [ Html.label [] [ Html.text "Klassenstufe:" ]
        , Html.select
            [ Html.Events.onInput ChangeGrade ]
            [ Html.option
                [ Html.Attributes.value "10"
                , Html.Attributes.selected (data.gr == "10") ]
                [ Html.text "10. Klasse" ]
            , Html.option
                [ Html.Attributes.value "11"
                , Html.Attributes.selected (data.gr == "11") ]
                [ Html.text "11. Klasse" ]
            , Html.option
                [ Html.Attributes.value "12"
                , Html.Attributes.selected (data.gr == "12") ]
                [ Html.text "12. Klasse" ]
            ]
        ]
    , Html.form
        []
        [ Html.label [] [ Html.text "Stickfigure-Größe:" ]
        , Html.div 
            [ Html.Attributes.class "size-selector" ]
            [ Html.input 
                [ HA.type_ "range"
                , HA.min "2"
                , HA.max "15"
                , HA.value <| String.fromFloat data.len
                , Html.Events.onInput ChangeLen
                ]
                []
            , Html.span
                [ Html.Attributes.style "left" (String.fromFloat((data.len - 2) / (15.5 - 1.5) * 100 + 4.5) ++ "%") ]
                [ text <| String.fromFloat data.len ]
            ]
        ]
    ]

view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Leider konnte der Stickfigure-Plot zum Alkoholkonsum von Schülern nicht geladen werden."

        Loading ->
            Html.span
                []
                [ Html.text "Lade  Stickfigure-Plot zum Alkoholkonsum von Schülern... "
                , FontAwesome.view (FontAwesome.styled [ FontAwesome.Attributes.spin ] FontAwesome.Solid.spinner)
                ]

        Success l ->
            let
                filteredStud =
                     l.data 

                numberStudies =
                    List.length l.data

            in
                div []
                    [ stylesheet
                    , nav l    
                    , p
                        []
                        [ text "Anzahl der dargestellten Schüler und Schülerinnen: "
                        , text <| String.fromInt numberStudies
                        ]
                    , stickfigureplot filteredStud l.chosendata l.len l.gr
                ]
gr : String
gr = "12"



type alias Data =
    { data : List StudentAcoholConsumption
    , len : Float
    , gr : String
    , chosendata : Maybe StudentAcoholConsumption
    }

type Model
 = Error
 | Loading
 | Success Data


inDegree : List Float -> List Float
inDegree listvalue =
    List.map (\x -> (180 * (x - (Maybe.withDefault 0 (List.minimum listvalue)))/(((Maybe.withDefault 10000 (List.maximum listvalue))) - ((Maybe.withDefault 0 (List.minimum listvalue)))))) listvalue 

inDegree2 : Float -> (Float,Float)-> Float
inDegree2 x (min,max) =
    (180 * (x - ((min)))/((((max))) - (((min)))))


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
    , medu : Float
    , fedu : Float
    , absences : Float
    , freetime : Float
    }
type Msg
    = GotText (Result Http.Error String)
    | ChangeLen (String)
    | ChangeGrade (String)
    | PointChosen (Maybe StudentAcoholConsumption)


type alias Point =
    { pointName : String, x : Float, y : Float, z : Float, a : Float, b : Float , c : Float , d : Float , e : Float , f : Float , g : Float , h : Float , i : Float, sex: Sex } 

type alias XYData =
    { data : List Point
    , chosendata : Maybe Point
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
            (Csv.Decode.field "sex" (\s -> Result.fromMaybe "sex not ok" (Just (sexFlag s))) 
                
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
                    ( Success <| { data = studentAcoholConsumptionList [ fullText ], len=8, gr="10", chosendata = Nothing}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeLen v ->
            case model of
                Success m ->
                    (Success <| {data = m.data, len = Maybe.withDefault 0 <| String.toFloat v, gr=m.gr, chosendata = m.chosendata}, Cmd.none)
                _ ->
                    ( model, Cmd.none )
        ChangeGrade g ->
            case model of
                Success m ->
                    (Success <| {data = m.data, len = m.len, gr=g, chosendata = m.chosendata}, Cmd.none)
                _ ->
                    ( model, Cmd.none )

        PointChosen sac ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, chosendata = sac ,len = m.len, gr=m.gr}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
                   
studentAcoholConsumptionList :List String -> List StudentAcoholConsumption
studentAcoholConsumptionList list1 =
    List.map(\t -> csvStringToData t) list1
        |> List.concat

{- filterReducedStudentAcoholConsumption : List StudentAcoholConsumption -> XYData 

filterReducedStudentAcoholConsumption my_stud =
    XYData <| List.filterMap stud2point my_stud
 -}
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


sexLabel : Sex -> String
sexLabel sex = case sex of 
    M -> "männlich"
    F -> "weiblich"
    _ -> "unbekannt"

sexFlag :  String -> Sex
sexFlag sex = case sex of 
    "M" -> M
    "F" -> F
    _ -> UnknownSex

pointLabel : Sex -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Point 
pointLabel sex firstperiodGradeMath secondperiodGradeMath thirdperiodGradeMath firstperiodGradePort secondperiodGradePort thirdperiodGradePort dalc walc fedu medu freetime absences= 
    Point ((sexLabel sex) ++ ", " ++ "Alkohol Wochentag: " ++  String.fromFloat dalc  ++ ", " ++ "Bildung Vater: " ++ String.fromFloat fedu ++ ", " ++ "Bildung Mutter: " ++ String.fromFloat medu ++ ", " 
        ++ "Freizeit: " ++ String.fromFloat freetime ++ "h, " ++ "Fehltage: " ++ String.fromFloat absences ++ "d") 
        (secondperiodGradeMath) (firstperiodGradeMath) (thirdperiodGradeMath) (firstperiodGradePort) (secondperiodGradePort) (thirdperiodGradePort) (dalc) (walc) (medu) (fedu) (absences) (freetime) ( sex)
xAxis : List Float -> Svg msg
xAxis values = 
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
stickfigureplot : List StudentAcoholConsumption -> Maybe StudentAcoholConsumption->  Float -> String -> Svg Msg
stickfigureplot liststud mchosen len grade =

 -- funktionen und parameter deklarieren
    let

        xfunc =
            case grade of
                "10" -> .firstperiodGradeMath 
                "11" -> .secondperiodGradeMath
                _ -> .thirdperiodGradeMath 
        yfunc =
            case grade of
                "10" -> .firstperiodGradePort 
                "11" ->  .secondperiodGradePort 
                _ -> .thirdperiodGradePort  
        
        xValues : List Float
        xValues =
            List.map xfunc liststud

        yValues : List Float
        yValues =
            List.map yfunc liststud

        uValues : List Float
        uValues =
            List.map .freetime liststud --u

        vValues : List Float
        vValues =
            List.map .walc liststud --v

        pValues : List Float
        pValues =
            List.map .medu liststud --p

        qValues : List Float
        qValues =
            List.map .absences liststud --q

        zValues : List Float
        zValues =
            List.map .fedu liststud --z

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
        [ style
            []
            [ TypedSvg.Core.text
                """
                .line polyline {
                    stroke: #dddddd;
                    stroke-width: 1;
                    transition: stroke 0.15s ease;
                }

                .Cline polyline {
                    stroke: #000000; 
                    stroke-width: 4; ----farbe ändern für angeklicte sticks
                }

                .line.sex-male polyline {
                    stroke: #55bfff;
                }

                .line.sex-female polyline {
                    stroke: #ff455f;
                }

                .line text {
                    font-size: small;
                    font-family: "Inter Tight", sans-serif;
                    visibility: hidden;
                    opacity: 0;
                    transition: opacity 0.2s ease;
                }

                .line:hover polyline {
                    stroke: #333333;
                    stroke-width: 2;
                }

                .line:hover text {
                    visibility: visible;
                    opacity: 1;
                }
                """
            ]       
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
                [ text (if grade == "10" then "Mathematik (10. Kl.)"
                else if grade == "11" then "Mathematik (11. Kl.)"
                else "Mathematik (12. Kl.)")] -- x -- xmts
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
                [ text (if grade == "10" then "Portugiesisch (10. Kl.)"
                else if grade == "11" then "Portugiesisch (11. Kl.)"
                else "Portugiesisch (12. Kl.)") ] -- y -- xmts
             ]
    -- plot points and description     
         ,g [ transform [ Translate padding padding ] ]
            (List.map (stickfigure xScaleLocal yScaleLocal len xfunc yfunc 
                (Maybe.withDefault 0 (List.minimum uValues), Maybe.withDefault 1000 (List.maximum uValues)) 
                (Maybe.withDefault 0 (List.minimum vValues), Maybe.withDefault 1000 (List.maximum vValues)) 
                (Maybe.withDefault 0 (List.minimum pValues), Maybe.withDefault 1000 (List.maximum pValues)) 
                (Maybe.withDefault 0 (List.minimum qValues), Maybe.withDefault 1000 (List.maximum qValues)) 
                (Maybe.withDefault 0 (List.minimum zValues), Maybe.withDefault 1000 (List.maximum zValues)))
            liststud
                {- xValues 
                |> andMapl yValues
                |> andMapl uDegree 
                |> andMapl vDegree 
                |> andMapl pDegree 
                |> andMapl qDegree 
                |> andMapl zDegree 
                |> andMapl model.data -}
                
            )
            -- map data with the defined variables
        ,g [ transform [ Translate padding padding ] ]
            (drawChosenStickfigure xScaleLocal yScaleLocal len xfunc yfunc 
                (Maybe.withDefault 0 (List.minimum uValues), Maybe.withDefault 1000 (List.maximum uValues)) 
                (Maybe.withDefault 0 (List.minimum vValues), Maybe.withDefault 1000 (List.maximum vValues)) 
                (Maybe.withDefault 0 (List.minimum pValues), Maybe.withDefault 1000 (List.maximum pValues)) 
                (Maybe.withDefault 0 (List.minimum qValues), Maybe.withDefault 1000 (List.maximum qValues)) 
                (Maybe.withDefault 0 (List.minimum zValues), Maybe.withDefault 1000 (List.maximum zValues))
            mchosen)
        
        ]

andMapl : List a -> List (a -> b) -> List b
andMapl = List.map2 (|>)

stickfigure : ContinuousScale Float -> ContinuousScale Float -> Float -> (StudentAcoholConsumption->Float)->(StudentAcoholConsumption->Float)-> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> StudentAcoholConsumption -> Svg Msg
stickfigure scaleX scaleY lange xfunc yfunc   (umin,umax) (vmin,vmax) (pmin,pmax) (qmin,qmax) (zmin,zmax) sacC =
    let
        degf : Float -> (Float,Float) -> Float
        degf = (\x (min,max) -> (270 - (inDegree2 x (min,max))))

        uDegree = degf sacC.freetime (umin,umax)
        
        vDegree = degf sacC.walc (vmin,vmax)

        pDegree =  degf sacC.medu (pmin,pmax)

        qDegree = degf sacC.absences (qmin,qmax)

        zDegree = degf sacC.fedu (zmin,zmax)

    in
        g [ class 
            [ "line"
            , case sacC.sex of
                M -> "sex-male"
                F -> "sex-female"
                UnknownSex -> "sex-unknown"
            ] ]
          [
            g  
                [ transform [ Translate (padding) padding ]
                ]
                [ text_ [ x  350, y -100, textAnchor AnchorMiddle ]
                    
                    [ Html.text ((sexLabel sacC.sex) ++ ", " ++ "Alkohol Wochentag: " ++ String.fromFloat sacC.dalc  ++ ", " ++ "Bildung Vater: " ++ String.fromFloat sacC.fedu ++ ", " ++ "Bildung Mutter: " ++ String.fromFloat sacC.medu ++ ", " ++ "Freizeit: " ++ String.fromFloat sacC.freetime ++ "h, " ++ "Fehltage: " ++ String.fromFloat sacC.absences ++ "d" ) ]
                ]
            , g
                [   transform
                    [ Translate
                        (Scale.convert scaleX (xfunc sacC)) 
                        (Scale.convert scaleY (yfunc sacC))  
                    ]
                    ,Html.Events.onClick (PointChosen (Just sacC))
                ]
                
                [ polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree), -lange/2*sin(degrees uDegree)) ]
                        ]
                        []
                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( (-lange/2)*cos(degrees uDegree) + lange*cos(degrees vDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees vDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree) - lange*cos(degrees pDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees pDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) + lange*cos(degrees qDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees qDegree) ) ]
                        ]
                        []
                    
                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) - lange*cos(degrees zDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees zDegree) ) ]
                        ]
                        []
                ]
          ]


drawChosenStickfigure : ContinuousScale Float -> ContinuousScale Float -> Float -> (StudentAcoholConsumption->Float)->(StudentAcoholConsumption->Float)-> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Maybe StudentAcoholConsumption -> List (Svg Msg)
drawChosenStickfigure scaleX scaleY lange xfunc yfunc   (umin,umax) (vmin,vmax) (pmin,pmax) (qmin,qmax) (zmin,zmax) msacC =
    
        case msacC of 
        Nothing -> []
        Just sacC ->
            let
                degf : Float -> (Float,Float) -> Float
                degf = (\x (min,max) -> (270 - (inDegree2 x (min,max))))

                uDegree = degf sacC.freetime (umin,umax)
                
                vDegree = degf sacC.walc (vmin,vmax)

                pDegree =  degf sacC.medu (pmin,pmax)

                qDegree = degf sacC.absences (qmin,qmax)

                zDegree = degf sacC.fedu (zmin,zmax)

            in        
                [g [ class 
                    [ "Cline"
                    , case sacC.sex of
                        M -> "sex-male"
                        F -> "sex-female"
                        UnknownSex -> "sex-unknown"
                    ] ]
                    [
                    g
                        [   transform
                            [ Translate
                                (Scale.convert scaleX (xfunc sacC)) 
                                (Scale.convert scaleY (yfunc sacC)) 
                                 
                            ]
                            ,Html.Events.onClick (PointChosen (Nothing))
                        ]
                        
                        [ polyline
                                [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree), -lange/2*sin(degrees uDegree)) ]
                                ]
                                []
                        , polyline
                                [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( (-lange/2)*cos(degrees uDegree) + lange*cos(degrees vDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees vDegree) ) ]
                                ]
                                []

                        , polyline
                                [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree) - lange*cos(degrees pDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees pDegree) ) ]
                                ]
                                []

                        , polyline
                                [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) + lange*cos(degrees qDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees qDegree) ) ]
                                ]
                                []
                            
                        , polyline
                                [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) - lange*cos(degrees zDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees zDegree) ) ]
                                ]
                                []
                        ]
                    ]
                ]

{- drawChosenpoint : ContinuousScale Float -> ContinuousScale Float -> (StudentAcoholConsumption -> Float) -> (StudentAcoholConsumption -> Float) -> Maybe StudentAcoholConsumption -> List (Svg Msg)
drawChosenpoint scaleX scaleY xfunc yfunc msac =
    case msac of
        Nothing -> []
        Just sac -> 
            [g
                [
                    class
                        [ "cpoint"
                        , case sac.sex of
                            M -> "sex-male"
                            F -> "sex-female"
                            UnknownSex -> "sex-unknown"
                        ]
                    ,fontSize <| Px 15.0
                    ,transform
                        [
                            Translate
                            (Scale.convert scaleX (xfunc sac))
                            (Scale.convert scaleY (yfunc sac))
                        ]
                ]

                [ polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree), -lange/2*sin(degrees uDegree)) ]
                        ]
                        []
                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( (-lange/2)*cos(degrees uDegree) + lange*cos(degrees vDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees vDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree) - lange*cos(degrees pDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees pDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) + lange*cos(degrees qDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees qDegree) ) ]
                        ]
                        []
                    
                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) - lange*cos(degrees zDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees zDegree) ) ]
                        ]
                        []
                ]
            ] -}

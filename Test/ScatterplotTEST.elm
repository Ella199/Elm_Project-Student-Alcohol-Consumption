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
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch
list : List String 
list = 
    [ "mergedstudent_withoutline.csv" ]
csvString_to_data : String -> List (String, Maybe Float, Maybe Float)
csvString_to_data csvR = 
    Csv.parse csvRaw
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
change Text = 
    List.map (\( a, b, c ) -> ( a, b |> Maybe.map String.fromFloat |> Maybe.withDefault
    "No value available", c |> Maybe.map String.fromFloat |> Maybe.withDefault "No value available")) Text

change : List (String, Maybe Float, Maybe Float) -> List (String, Float, Float)
change TextTwo = 
    List.map (\( a, b, c ) -> (a, b |> Maybe.withDefault 0.0, c |> Maybe.withDefault 0.0)) Text
    

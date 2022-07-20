module ParalleleKoordinaten exposing (..)
import Axis
import Browser
import Color
import Csv.Decode
import TypedSvg exposing (path)
import Html.exposing (Html, a, li, ul)
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

        

    




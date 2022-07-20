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



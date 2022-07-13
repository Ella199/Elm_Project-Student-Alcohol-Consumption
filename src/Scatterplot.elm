module Scatterplot exposing (..)

import Axis
import Html exposing (Html)
import Http
import Scale exposing (Continousscale)
import Statistics 
import TypedSvg exposing (circle, g, style, svg, text)
import TypedSVG.Attributes exposing (class, frontFamily, frontSize, textAnchor, transform, viewBox)
import TypedSVG.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment (..), FrontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser


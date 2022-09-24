module Main exposing (..)
import Browser
import Html
import Html.Events exposing (onClick)
import VisualisationStickfigurePlot
import ParalleleKoordinaten
import Scatterplot



type alias Model =
    { 
      ScatterplotModel : Scatterplot.Model
    , ParalleleKoordinatenModel : ParalleleKoordinaten.Model
    , VisualisationStickfigurePlotModel : VisualisationStickfigurePlot.Model
    , active : Active
    }

type Active
    = Text
    | Scatterplot
    | ParalleleKoordinaten
    | VisualisationStickfigure


type Msg
    = TextMsg
    | ScatterplotMsg Scatterplot.Msg
    | ParalleleKoordinatenMsg ParalleleKoordinaten.Msg
    | VisualisationStickfigureMsg VisualisationStickfigure.Msg
    | SwitchView Active


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    Model Scatterplot.init ParalleleKoordinaten.init VisualisationStickfigure.init 

--

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ onClick (SwitchView Scatterplot) ] [ Html.text "Scatterplot" ]
        , Html.button [ onClick (SwitchView ParalleleKoordinaten) ] [ Html.text "ParalleleKoordinaten" ]
        , case model.active of
            Scatterplot ->
                Html.map ScatterplotMsg (Scatterplot.view model.scatterplotModel)

            ParalleleKoordinaten ->
                Html.map ParalleleKoordinatenMsg (ParalleleKoordinaten.view model.paralleleKoordinatenModel)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextMsg ->
            { model | textModel = Text.String}
        
        ScatterplotMsg ScatterplotMsg ->
            { model | scatterplotModel = Scatterplot.update ScatterplotMsg model.scatterplotModel }

        ParalleleKoordinatenMsg ParalleleKoordinatenMsg ->
            { model | paralleleKoordinatenModel = ParalleleKoordinaten.update ParalleleKoordinatenMsg model.paralleleKoordinaten }

        SwitchView newActitve ->
            { model | active = newActitve }
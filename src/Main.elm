module Main exposing (..)
import Browser
import Html
import Html.Events exposing (onClick)
import VisualisationStickfigurePlot
import ParalleleKoordinaten
import Scatterplot
import Scatterplot exposing (scatterplot)


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
    = ScatterplotMsg Scatterplot.Msg
    | ScatterplotMsg Scatterplot.Msg
    | ParalleleKoordinatenMsg ParalleleKoordinaten.Msg
    | VisualisationStickfigureMsg VisualisationStickfigure.Msg
    | SwitchView Active


main : Program () Model Msg
main =
     Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        (scatterplot, scatterplotCmd) = Scatterplot.init ()
        (paralleleKoordinaten, paralleleKoordinatenCmd) = ParalleleKoordinaten.init ()
        (visualisationStickfigurePlot, visualisationStickfigurePlotCmd) = VisualisationStickfigurePlot.init ()
    in
    ( Model scatterplot paralleleKoordinaten visualisationStickfigurePlot Text
    , Cmd.batch [ Cmd.map ScatterplotMsg scatterplotCmd, Cmd.map ParalleleKoordinatenMsg paralleleKoordinatenCmd, Cmd.map VisualisationStickfigureMsg visualisationStickfigurePlotCmd ])

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

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
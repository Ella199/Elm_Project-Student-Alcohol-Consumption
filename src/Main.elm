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

style : Html.Html Msg
style =
  let
    styles = 
        """
        @import url("https://fonts.googleapis.com/css2?family=Inter+Tight&family=Inter:wght@400;600&display=swap");
        body {
            font-family: "Inter", sans-serif;
            width: 100vw;
            height: 100vh;
            background: #f0f0f0;
        }

        * {
            font-family: inherit;
        }

        #wrapper {
            display: flex;
            flex-direction: column;
            width: 100%;
            max-width: 1000px;
            height: 100vh;
            margin: 0 auto;
            background: #ffffff;
            box-shadow: 0 0 16px 0 rgba(0,0,0,0.4);
        }

        #nav {
            display: flex;
            z-index: 10;
            flex: 0;
            background: #55bfff;
            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.4);
        }

        #nav-logo {
            line-height: 1em;
            padding: 0.5em;
            font-weight: 600;
            font-size: 1.5em;
        }

        #nav-title {
            flex: 1;
            line-height: 1em;
            padding: 1em;
            font-weight: 600;
            font-size: 1em;
            font-variant: small-caps;
        }

        #nav > button, #nav > a {
            display: flex;
            align-items: center;
            box-sizing: border-box;
            height: 100%;
            padding: 0 1em;
            background: transparent;
            border: none;
            border-top: 2px solid transparent;
            border-bottom: 2px solid transparent;
            font-size: 0.9em;
            transition: background 0.2s ease, border 0.2s ease;
        }

        #nav > button.active, #nav > a.active {
            background: #ffffff44;
            border-bottom-color: #000000;
        }

        #nav > button:hover, #nav > a:hover {
            background: #ffffffaa;
            border-bottom-color: #666666;
        }

        #nav > a:visited {
            color: unset;
        }

        #main {
            flex: 1;
            padding: 1em;
            padding-top: 1.5em;
            overflow: scroll;
        }

        #start-screen-nav {
            display: flex;
        }

        #start-screen-nav > button {
            height: 2.5em;
            border-radius: 1.25em;
            margin-right: 1em;
            padding: 0 1em;
            background: #55bfff3b;
            border: 2px solid #55bfff;
            font-size: 0.9em;
            transition: background 0.2s ease;
        }

        #start-screen-nav > button:hover {
            background: #55bfff;
        }

        #footer {
            display: flex;
            z-index: 10;
            flex: 0;
            font-size: 0.75em;
            padding: 1em;
            background: #f4f4f4;
            box-shadow: 0 -4px 8px 0 rgba(0,0,0,0.2);
            color: #444444;
        }

        #footer a, #footer a:visited {
            color: inherit;
        }

        #footer > p {
            flex: 1;
            margin: 0;
        }

        #footer > nav > a {
            margin-left: 1em;
        }
        """
  in
    Html.node "style" [] [ Html.text styles ]

nav : Model -> Html.Html Msg
nav model =
    Html.nav 
        [ Html.Attributes.id "nav" ] 
        [ Html.span 
            [ Html.Attributes.id "nav-logo"
            , onClick (SwitchView Text)
            ]
            [ FontAwesome.view FontAwesome.Solid.beer ]
        , Html.span 
            [ Html.Attributes.id "nav-title"
            , onClick (SwitchView Text)
            ] 
            [ Html.text " Alkoholkonsum von Schülern" ]
        , Html.button
            [ onClick (SwitchView Text)
            , Html.Attributes.class (if model.active == Text then "active" else "")
            ]
            [ FontAwesome.view FontAwesome.Solid.home ]
        , Html.button
            [ onClick (SwitchView Scatterplot)
            , Html.Attributes.class (if model.active == Scatterplot then "active" else "")
            ]
            [ Html.text "Scatterplot" ]
        , Html.button 
            [ onClick (SwitchView ParalleleKoordinaten)
            , Html.Attributes.class (if model.active == ParalleleKoordinaten then "active" else "")
            ]
            [ Html.text "Parallele Koordinaten" ]
        , Html.button 
            [ onClick (SwitchView VisualisationStickfigure)
            , Html.Attributes.class (if model.active == VisualisationStickfigure then "active" else "")
            ]
            [ Html.text "Stickfigures" ]
        , Html.a 
            [ Html.Attributes.href "https://github.com/Ella199/Elm_Project-Student-Alcohol-Consumption"
            , Html.Attributes.target "_blank"
            ]
            [ FontAwesome.view FontAwesome.Brands.github ] 
        ]

body : Model -> Html.Html Msg
body model =
    Html.main_ 
        [ Html.Attributes.id "main" ]
        [ case model.active of
            Text ->
                Html.div
                    [] 
                    [ Html.p
                        [] 
                        [ Html.text "Herzlich wilkommen!"
                        , Html.br [] []
                        , Html.text "Kleine Einführung in die Möglichkeiten der Visualisierung..."
                        , Html.br [] []
                        , Html.text "Was haben wir für Daten verwendet?"
                        ]
                    , Html.p [] [ Html.text "Wählen Sie nun eine unserer Visualisierungen:" ]       
                    , Html.nav
                        [ Html.Attributes.id "start-screen-nav" ]
                        [ Html.button
                            [ onClick (SwitchView Scatterplot) ]
                            [ Html.text "Scatterplot ",
                            FontAwesome.view FontAwesome.Solid.arrowRight
                            ]

                        , Html.button 
                            [ onClick (SwitchView ParalleleKoordinaten) ]
                            [ Html.text "Parallele Koordinaten ",
                            FontAwesome.view FontAwesome.Solid.arrowRight
                            ]
                        , Html.button 
                            [ onClick (SwitchView VisualisationStickfigure) ]
                            [ Html.text "Stickfigures ",
                            FontAwesome.view FontAwesome.Solid.arrowRight
                            ]
                        ]
            
                    ]

            Scatterplot ->
                Html.map ScatterplotMsg (Scatterplot.view model.scatterplotModel)


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ onClick (SwitchView Scatterplot) ] [ Html.text "Scatterplot" ]
        , Html.button [ onClick (SwitchView ParalleleKoordinaten) ] [ Html.text "ParalleleKoordinaten" ]
        , Html.button [ onClick (SwitchView VisualisationStickfigure) ] [ Html.text "VisualisationStickfigure" ]
        , case model.active of
            Text ->
                Html.p [] [ Html.text "Elm Visualisation" ]

            Scatterplot ->
                Html.map ScatterplotMsg (Scatterplot.view model.scatterplotModel)

            ParalleleKoordinaten ->
                Html.map ParalleleKoordinatenMsg (ParalleleKoordinaten.view model.paralleleKoordinatenModel)

            VisualisationStickfigure ->
                Html.map VisualisationStickfigureMsg (VisualisationStickfigurePlot.view model.visualisationStickfigurePlotModel)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        
        
        ScatterplotMsg scatterplotMsg ->
            let
                (scatterplot, scatterplotCmd) = Scatterplot.update scatterplotMsg model.scatterplotModel
            in
            ( { model | scatterplotModel = scatterplot }, Cmd.map ScatterplotMsg scatterplotCmd )

        ParalleleKoordinatenMsg paralleleKoordinatenMsg ->
            let
                (paralleleKoordinaten, paralleleKoordinatenCmd) = ParalleleKoordinaten.update paralleleKoordinatenMsg model.paralleleKoordinatenModel
            in
            ( { model | paralleleKoordinatenModel = paralleleKoordinaten }, Cmd.map ParalleleKoordinatenMsg paralleleKoordinatenCmd )

        VisualisationStickfigureMsg visualisationStickfigurePlotMsg ->
            let
                (visualisationStickfigurePlot, visualisationStickfigurePlotCmd) = VisualisationStickfigurePlot.update visualisationStickfigurePlotMsg model.visualisationStickfigurePlotModel
            in
            ( { model | visualisationStickfigurePlotModel = visualisationStickfigurePlot }, Cmd.map VisualisationStickfigureMsg visualisationStickfigurePlotCmd )

        SwitchView newActitve ->
            ( { model | active = newActitve }, Cmd.none )
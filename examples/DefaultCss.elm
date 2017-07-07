port module DefaultCss exposing (..)

import Alert.Stylesheets exposing (alertCss, CssIds(..))
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Css exposing (..)


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "alert.css"
          , Css.File.compile [ Alert.Stylesheets.alertCss, css ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure


css : Stylesheet
css =
    stylesheet
        [ id AlertsContainer
            [ fontFamilies
                [ "Open Sans", "Helvetica Neue", "Arial", "Sans Serif" ]
            , position fixed
            , width (px 300)
            , top (px -2)
            , right (px 8)
            ]
        ]

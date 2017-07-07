module Alert.Stylesheets exposing (alertNamespace, alertCss, CssIds(..), CssClasses(..))

{-| This is some basic CSS for the alerts.


# Namespace

@docs alertNamespace


# CSS

@docs alertCss, CssIds, CssClasses

-}

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html.CssHelpers exposing (withNamespace, Namespace)


{-| Namespace for alerts
-}
alertNamespace : Namespace String class id msg
alertNamespace =
    withNamespace "alert"


{-| Ids used in the alerts
-}
type CssIds
    = AlertsContainer


{-| Classes used in the alerts
-}
type CssClasses
    = InfoAlert
    | SuccessAlert
    | WarningAlert
    | ErrorAlert
    | LoadingAlert


{-| Basic Styles
-}
alertCss : Stylesheet
alertCss =
    (stylesheet << namespace alertNamespace.name)
        [ id AlertsContainer
            [ children
                [ div
                    [ width (pct 100)
                    , marginTop (px 10)
                    , padding4 (px 15) (px 35) (px 15) (px 15)
                    , boxSizing borderBox
                    , position relative
                    , borderRadius (px 2)
                    , overflow hidden
                    , withClass InfoAlert
                        [ backgroundColor (rgb 217 237 247)
                        , border3 (px 1) solid (rgb 202 234 244)
                        , color (rgb 49 114 150)
                        ]
                    , withClass SuccessAlert
                        [ backgroundColor (rgb 221 238 214)
                        , border3 (px 1) solid (rgb 206 224 192)
                        , color (rgb 60 118 61)
                        ]
                    , withClass WarningAlert
                        [ backgroundColor (rgb 252 248 227)
                        , border3 (px 1) solid (rgb 251 241 215)
                        , color (rgb 138 109 59)
                        ]
                    , withClass ErrorAlert
                        [ backgroundColor (rgb 242 222 222)
                        , border3 (px 1) solid (rgb 235 204 204)
                        , color (rgb 132 53 52)
                        ]
                    , withClass LoadingAlert
                        [ backgroundColor (rgb 217 237 247)
                        , border3 (px 1) solid (rgb 202 234 244)
                        , color (rgb 49 114 150)
                        , children
                            [ li
                                [ listStyle none
                                , position absolute
                                , right zero
                                , top (px -5)
                                ]
                            ]
                        ]
                    , children
                        [ div
                            [ position absolute
                            , top (px 22)
                            , right (px 27)
                            ]
                        , svg
                            [ position absolute
                            , top (px 15)
                            , right (px 15)
                            , cursor pointer
                            ]
                        ]
                    ]
                ]
            ]
        ]

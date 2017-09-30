module Alert exposing (Alert, AlertType(..), Model, initModel, view, subscriptions, Msg(..), update)

{-| This is a basic alert message library. It shows and hides alerts based on time or user input.


# Model

@docs Alert, AlertType, Model, initModel


# View

@docs view


# Subscriptions

@docs subscriptions


# State

@docs Msg, update

-}

import Alert.Stylesheets exposing (alertNamespace, CssIds(..), CssClasses(..))
import Animation exposing (px)
import Animation.Messenger
import Color
import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Events exposing (onClick)
import Spinner
import Svg exposing (svg, line)
import Svg.Attributes exposing (xmlSpace, width, height, viewBox, fill, stroke, strokeWidth, strokeLinecap, strokeLinejoin, x1, x2, y1, y2)
import Time exposing (every, millisecond)


-- MODEL --


{-| This is the alert model. `untilRemove` specifies how long until the message will be automatically removed (a value of `-1` for will display it until it is removed manually). If `icon` is `False` then the alert icon will be hidden (this is only useful if you want to prevent the user from closing the alert).
-}
type alias Alert =
    { type_ : AlertType
    , message : String
    , untilRemove : Int
    , icon : Bool
    }


{-| The type of alert, determines which class will be attached to the alert. `Loading` will display a spinner instead of a close icon.
-}
type AlertType
    = Info
    | Success
    | Warning
    | Error
    | Loading


{-| The model of all alerts, includes a dictionary of alerts, the next key to use and state for keeping track of animations.
-}
type alias Model =
    { alerts : Dict Int Alert
    , nextKey : Int
    , useAnimations : Bool
    , height : Dict Int (Animation.Messenger.State Msg)
    , opacity : Dict Int (Animation.Messenger.State Msg)
    , padding : Dict Int (Animation.Messenger.State Msg)
    , remove : Dict Int Int
    , spinner : Dict Int Spinner.Model
    }


{-| Initialize the model
-}
initModel : Bool -> Model
initModel useAnimations =
    { alerts = Dict.empty
    , nextKey = 0
    , useAnimations = useAnimations
    , height = Dict.empty
    , opacity = Dict.empty
    , padding = Dict.empty
    , remove = Dict.empty
    , spinner = Dict.empty
    }


type alias Styles =
    { shown : List Animation.Property
    , hidden : List Animation.Property
    , flat : List Animation.Property
    , full : List Animation.Property
    , bare : List Animation.Property
    , padded : List Animation.Property
    }


styles : Styles
styles =
    { shown =
        [ Animation.opacity 1.0 ]
    , hidden =
        [ Animation.opacity 0.0 ]
    , flat =
        [ Animation.height <| px 0.0 ]
    , full =
        [ Animation.height <| px 50.0 ]
    , bare =
        [ Animation.paddingTop <| px 0.0
        , Animation.paddingBottom <| px 0.0
        ]
    , padded =
        [ Animation.paddingTop <| px 15.0
        , Animation.paddingBottom <| px 15.0
        ]
    }



-- VIEW --


{ id, class, classList } =
    alertNamespace


{-| The view takes the model and displays a container with all the alerts
-}
view : Model -> Html Msg
view model =
    Dict.toList model.alerts
        |> List.reverse
        |> List.map
            (alertView
                model.useAnimations
                model.opacity
                model.height
                model.padding
                model.spinner
            )
        |> div [ id AlertsContainer ]


alertView :
    Bool
    -> Dict Int (Animation.Messenger.State Msg)
    -> Dict Int (Animation.Messenger.State Msg)
    -> Dict Int (Animation.Messenger.State Msg)
    -> Dict Int Spinner.Model
    -> ( Int, Alert )
    -> Html Msg
alertView useAnimations opacity height padding spinner ( key, { message, type_, icon } ) =
    let
        ( msgClass, iconHtml ) =
            case type_ of
                Info ->
                    ( InfoAlert, closeIcon key "rgb(49, 114, 150)" )

                Success ->
                    ( SuccessAlert, closeIcon key "rgb(60, 118, 61)" )

                Warning ->
                    ( WarningAlert, closeIcon key "rgb(138, 109, 59)" )

                Error ->
                    ( ErrorAlert, closeIcon key "rgb(132, 53, 52)" )

                Loading ->
                    ( LoadingAlert
                    , Spinner.view
                        { lines = 8
                        , length = 4
                        , width = 3
                        , radius = 5
                        , scale = 1
                        , corners = 1
                        , opacity = 0.25
                        , rotate = 0
                        , direction = Spinner.Clockwise
                        , speed = 1
                        , trail = 60
                        , translateX = 50
                        , translateY = 50
                        , shadow = False
                        , hwaccel = False
                        , color = always <| Color.rgba 34 34 34 0.8
                        }
                        (Maybe.withDefault Spinner.init <| Dict.get key spinner)
                    )

        alertOpacity =
            if useAnimations == True then
                Dict.get key opacity
                    |> Maybe.map Animation.render
                    |> Maybe.withDefault []
            else
                []

        alertHeight =
            if useAnimations == True then
                Dict.get key height
                    |> Maybe.map Animation.render
                    |> Maybe.withDefault []
            else
                []

        alertPadding =
            if useAnimations == True then
                Dict.get key padding
                    |> Maybe.map Animation.render
                    |> Maybe.withDefault []
            else
                []
    in
        div
            (List.concat
                [ alertOpacity
                , alertHeight
                , alertPadding
                , [ class [ msgClass ] ]
                ]
            )
            [ span [] [ text message ]
            , if icon == True then
                iconHtml
              else
                span [] []
            ]


closeIcon : Int -> String -> Html Msg
closeIcon key strokeColor =
    svg
        [ xmlSpace "http://www.w3.org/2000/svg"
        , width "20"
        , height "20"
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke strokeColor
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , onClick <| RemoveAlert key
        ]
        [ line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]



-- SUBSCRIPTIONS --


{-| The subscriptions takes the model and returns subscriptions for alerts
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        time =
            if Dict.size model.alerts > 0 then
                [ every millisecond Tick ]
            else
                []

        opacity =
            Dict.toList model.opacity
                |> List.map
                    (\( key, op ) ->
                        Animation.subscription (OpacityAnim key) [ op ]
                    )

        height =
            Dict.toList model.height
                |> List.map
                    (\( key, ht ) ->
                        Animation.subscription (HeightAnim key) [ ht ]
                    )

        padding =
            Dict.toList model.padding
                |> List.map
                    (\( key, pd ) ->
                        Animation.subscription (PaddingAnim key) [ pd ]
                    )

        spinner =
            Dict.toList model.spinner
                |> List.map
                    (\( key, sp ) ->
                        Sub.map (SpinnerMsg key) Spinner.subscription
                    )
    in
        Sub.batch <| List.concat [ time, opacity, height, padding, spinner ]



-- UPDATE --


{-| `ClearAll` will clear all alerts other than `Loading` alerts and `ForceClearAll` will clear all alerts including `Loading` alerts. To add an alert use `AddAlert`, to remove an alert use `RemoveAlert`. Do not use any of the other message types.
-}
type Msg
    = AddAlert Alert
    | RemoveAlert Int
    | ClearAll
    | ForceClearAll
    | Tick Float
    | OpacityAnim Int Animation.Msg
    | HeightAnim Int Animation.Msg
    | PaddingAnim Int Animation.Msg
    | SpinnerMsg Int Spinner.Msg
    | Hide Int


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAlert alert ->
            let
                opacity =
                    if model.useAnimations then
                        Dict.insert
                            model.nextKey
                            (Animation.interrupt
                                [ Animation.toWith
                                    (Animation.speed { perSecond = 2.0 })
                                    styles.shown
                                ]
                                (Animation.style styles.hidden)
                            )
                            model.opacity
                    else
                        model.opacity

                height =
                    if model.useAnimations then
                        Dict.insert
                            model.nextKey
                            (Animation.interrupt
                                [ Animation.toWith
                                    (Animation.speed { perSecond = 100.0 })
                                    styles.full
                                ]
                                (Animation.style styles.flat)
                            )
                            model.height
                    else
                        model.height

                padding =
                    if model.useAnimations then
                        Dict.insert
                            model.nextKey
                            (Animation.interrupt
                                [ Animation.toWith
                                    (Animation.speed { perSecond = 30.0 })
                                    styles.padded
                                ]
                                (Animation.style styles.bare)
                            )
                            model.padding
                    else
                        model.padding
            in
                ( { model
                    | alerts = Dict.insert model.nextKey alert model.alerts
                    , nextKey = model.nextKey + 1
                    , opacity = opacity
                    , height = height
                    , padding = padding
                    , spinner =
                        Dict.insert model.nextKey Spinner.init model.spinner
                  }
                , Cmd.none
                )

        RemoveAlert key ->
            let
                alerts =
                    Dict.update key
                        (\maybeAlert ->
                            case maybeAlert of
                                Nothing ->
                                    Nothing

                                Just alert ->
                                    if model.useAnimations then
                                        Just { alert | untilRemove = 1 }
                                    else
                                        Nothing
                        )
                        model.alerts
            in
                ( { model | alerts = alerts }
                , Cmd.none
                )

        ClearAll ->
            let
                alerts =
                    Dict.toList model.alerts
                        |> List.filter
                            (\( key, alert ) -> alert.type_ == Loading)
                        |> Dict.fromList
            in
                ( { model | alerts = alerts }, Cmd.none )

        ForceClearAll ->
            ( initModel model.useAnimations, Cmd.none )

        Tick time ->
            let
                tick =
                    Dict.toList model.alerts
                        |> List.map
                            (\( key, alert ) ->
                                if alert.untilRemove >= 0 then
                                    ( key
                                    , { alert | untilRemove = alert.untilRemove - 1 }
                                    )
                                else
                                    ( key, alert )
                            )

                removing =
                    List.filter (\( key, alert ) -> alert.untilRemove == 0) tick

                opacity =
                    List.foldl
                        (\( key, _ ) acc ->
                            Dict.insert key
                                (Animation.interrupt
                                    [ Animation.toWith
                                        (Animation.speed { perSecond = 2.0 })
                                        styles.hidden
                                    , Animation.Messenger.send <| Hide key
                                    ]
                                    (Animation.style styles.shown)
                                )
                                acc
                        )
                        model.opacity
                        removing

                height =
                    List.foldl
                        (\( key, _ ) acc ->
                            Dict.insert key
                                (Animation.interrupt
                                    [ Animation.toWith
                                        (Animation.speed { perSecond = 100.0 })
                                        styles.flat
                                    , Animation.Messenger.send <| Hide key
                                    ]
                                    (Animation.style styles.full)
                                )
                                acc
                        )
                        model.height
                        removing

                padding =
                    List.foldl
                        (\( key, _ ) acc ->
                            Dict.insert key
                                (Animation.interrupt
                                    [ Animation.toWith
                                        (Animation.speed { perSecond = 30.0 })
                                        styles.bare
                                    , Animation.Messenger.send <| Hide key
                                    ]
                                    (Animation.style styles.padded)
                                )
                                acc
                        )
                        model.padding
                        removing
            in
                ( { model
                    | alerts = Dict.fromList tick
                    , opacity = opacity
                    , height = height
                    , padding = padding
                  }
                , Cmd.none
                )

        OpacityAnim key animMsg ->
            let
                opacity =
                    Dict.get key model.opacity
            in
                case opacity of
                    Nothing ->
                        ( model, Cmd.none )

                    Just op ->
                        let
                            ( anim, cmd ) =
                                Animation.Messenger.update animMsg op
                        in
                            ( { model
                                | opacity = Dict.insert key anim model.opacity
                              }
                            , cmd
                            )

        HeightAnim key animMsg ->
            let
                height =
                    Dict.get key model.height
            in
                case height of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ht ->
                        let
                            ( anim, cmd ) =
                                Animation.Messenger.update animMsg ht
                        in
                            ( { model
                                | height = Dict.insert key anim model.height
                              }
                            , cmd
                            )

        PaddingAnim key animMsg ->
            let
                padding =
                    Dict.get key model.padding
            in
                case padding of
                    Nothing ->
                        ( model, Cmd.none )

                    Just pd ->
                        let
                            ( anim, cmd ) =
                                Animation.Messenger.update animMsg pd
                        in
                            ( { model
                                | padding = Dict.insert key anim model.padding
                              }
                            , cmd
                            )

        SpinnerMsg key msg ->
            let
                spinner =
                    Dict.get key model.spinner
            in
                case spinner of
                    Nothing ->
                        ( model, Cmd.none )

                    Just sp ->
                        ( { model
                            | spinner =
                                Dict.insert key
                                    (Spinner.update msg sp)
                                    model.spinner
                          }
                        , Cmd.none
                        )

        Hide key ->
            case Dict.get key model.remove of
                Nothing ->
                    ( { model | remove = Dict.insert key 1 model.remove }
                    , Cmd.none
                    )

                Just rm ->
                    if rm == 2 then
                        ( { model
                            | alerts = Dict.remove key model.alerts
                            , opacity = Dict.remove key model.opacity
                            , height = Dict.remove key model.height
                            , padding = Dict.remove key model.padding
                            , remove = Dict.remove key model.remove
                            , spinner = Dict.remove key model.spinner
                          }
                        , Cmd.none
                        )
                    else
                        ( { model
                            | remove = Dict.insert key (rm + 1) model.remove
                          }
                        , Cmd.none
                        )

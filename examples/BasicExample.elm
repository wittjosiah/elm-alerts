port module BasicExample exposing (..)

import Alert
import Html exposing (Html, program, div, button, text, span, input)
import Html.Attributes exposing (type_, value, checked)
import Html.Events exposing (onClick, onInput)


-- MODEL


type alias Model =
    { untilRemove : Int
    , useAnimations : Bool
    , forever : Bool
    , icon : Bool
    , message : String
    , alerts : Alert.Model
    }


initModel : Model
initModel =
    { untilRemove = 500
    , useAnimations = True
    , forever = False
    , icon = True
    , message = "Test Message"
    , alerts = Alert.initModel True
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick <|
                AlertMsg <|
                    Alert.AddAlert
                        { type_ = Alert.Error
                        , message = model.message
                        , untilRemove = model.untilRemove
                        , icon = model.icon
                        }
            ]
            [ text "Error" ]
        , button
            [ onClick <|
                AlertMsg <|
                    Alert.AddAlert
                        { type_ = Alert.Success
                        , message = model.message
                        , untilRemove = model.untilRemove
                        , icon = model.icon
                        }
            ]
            [ text "Success" ]
        , button
            [ onClick <|
                AlertMsg <|
                    Alert.AddAlert
                        { type_ = Alert.Info
                        , message = model.message
                        , untilRemove = model.untilRemove
                        , icon = model.icon
                        }
            ]
            [ text "Info" ]
        , button
            [ onClick <|
                AlertMsg <|
                    Alert.AddAlert
                        { type_ = Alert.Warning
                        , message = model.message
                        , untilRemove = model.untilRemove
                        , icon = model.icon
                        }
            ]
            [ text "Warning" ]
        , button
            [ onClick <|
                AlertMsg <|
                    Alert.AddAlert
                        { type_ = Alert.Loading
                        , message = model.message
                        , untilRemove = model.untilRemove
                        , icon = model.icon
                        }
            ]
            [ text "Loading" ]
        , div []
            [ span [] [ text "ms until removed" ]
            , input
                [ type_ "number"
                , value <| toString model.untilRemove
                , onInput UntilRemove
                ]
                []
            ]
        , div []
            [ span [] [ text "Don't use animations" ]
            , input
                [ type_ "checkbox"
                , onClick UseAnimations
                ]
                []
            ]
        , div []
            [ span [] [ text "Don't disappear" ]
            , input
                [ type_ "checkbox"
                , onClick Forever
                ]
                []
            ]
        , div []
            [ span [] [ text "Hide Alert Icon" ]
            , input
                [ type_ "checkbox"
                , onClick Icon
                ]
                []
            ]
        , div []
            [ span [] [ text "Alert Message" ]
            , input
                [ type_ "text"
                , value model.message
                , onInput Message
                ]
                []
            ]
        , Html.map AlertMsg <| Alert.view model.alerts
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map AlertMsg <| Alert.subscriptions model.alerts



-- UPDATE


type Msg
    = AlertMsg Alert.Msg
    | UntilRemove String
    | UseAnimations
    | Forever
    | Icon
    | Message String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AlertMsg alertMsg ->
            let
                ( alerts, cmd ) =
                    Alert.update alertMsg model.alerts
            in
                ( { model | alerts = alerts }, Cmd.map AlertMsg cmd )

        UntilRemove untilRemove ->
            case ( model.forever, String.toInt untilRemove ) of
                ( True, _ ) ->
                    ( { model | untilRemove = -1 }, Cmd.none )

                ( _, Ok until ) ->
                    ( { model | untilRemove = until }, Cmd.none )

                ( _, Err _ ) ->
                    ( model, Cmd.none )

        UseAnimations ->
            ( { model
                | alerts = Alert.initModel <| not model.useAnimations
                , useAnimations = not model.useAnimations
              }
            , Cmd.none
            )

        Forever ->
            ( { model
                | forever = not model.forever
                , untilRemove = -1
              }
            , Cmd.none
            )

        Icon ->
            ( { model | icon = not model.icon }, Cmd.none )

        Message message ->
            ( { model | message = message }, Cmd.none )



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

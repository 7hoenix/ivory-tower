module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


type alias Model =
    { numerator : Int
    , denominator : Int
    , error : Maybe String
    }


tryToAdd : String -> String -> Maybe Int
tryToAdd value1 value2 =
    case ( String.toInt value1, String.toInt value2 ) of
        ( Just v1, Just v2 ) ->
            Just (v1 + v2)

        ( Just v1, Nothing ) ->
            Just v1

        ( Nothing, Just v2 ) ->
            Just v2

        ( Nothing, Nothing ) ->
            Nothing


tryToDivide : String -> String -> Result String Int
tryToDivide value1 value2 =
    case ( String.toInt value1, String.toInt value2 ) of
        ( Just v1, Just v2 ) ->
            if v2 == 0 then
                Err "Can't divide by zero or whatever"

            else
                Ok (v1 // v2)

        ( _, _ ) ->
            Err "Must have two numbers to do division"


init : ( Model, Cmd Msg )
init =
    ( { numerator = 4, denominator = 4, error = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateDenominator String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateDenominator updatedDenominator ->
            let
                numeratorAsString =
                    String.fromInt model.numerator

                resultOfFn =
                    tryToDivide numeratorAsString updatedDenominator
            in
            case resultOfFn of
                Err err ->
                    ( { model | error = Just err }, Cmd.none )

                Ok _ ->
                    ( { model
                        | error = Nothing
                        , denominator = Maybe.withDefault model.denominator <| String.toInt updatedDenominator
                      }
                    , Cmd.none
                    )



---- VIEW ----


view : Model -> Html Msg
view { denominator, numerator, error } =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text <| String.fromInt (numerator // denominator) ]
        , div []
            [ text "Update Denonmiator"
            , input [ type_ "text", onInput UpdateDenominator ] []
            ]
        , case error of
            Nothing ->
                text ""

            Just e ->
                text e
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

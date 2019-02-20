module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


type alias Model =
    { numerator : Int
    , denominator : Int
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
    ( { numerator = 4, denominator = 4 }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateNumerator String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateNumerator updatedNumerator ->
            let
                denominatorAsString =
                    String.fromInt model.denominator

                resultOfFn =
                    tryToDivide updatedNumerator denominatorAsString
            in
            case resultOfFn of
                Err err ->
                    ( model, Cmd.none )

                Ok updated ->
                    ( { model | numerator = Maybe.withDefault model.numerator <| String.toInt updatedNumerator }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { denominator, numerator } =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text <| String.fromInt (numerator // denominator) ]
        , input [ type_ "text", name "numerator", onInput UpdateNumerator ] []
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

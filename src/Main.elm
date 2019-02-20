module Main exposing (Model, Msg(..), Person, findFirstAdult, main, over18, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List


main =
    Browser.sandbox { init = 0, update = update, view = view }


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


type alias Person =
    { name : String
    , age : Int
    }


findFirstAdult : List Person -> Maybe Person
findFirstAdult people =
    List.head <| List.filter (.age >> over18) people


over18 : Int -> Bool
over18 age =
    age >= 18


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]

module Slide exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font exposing (Font)
import Html exposing (Html)
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List.Zipper as Zipper exposing (Zipper)
import Markdown


type alias SlideShow a =
    Zipper (Slide a)


type alias Slide a =
    Element a


createSlideShow : List (Slide msg) -> SlideShow msg
createSlideShow =
    Zipper.fromList >> Maybe.withDefault (Zipper.singleton (text "There are no slides in this presentation :("))


previousSlide s =
    Zipper.previous s |> Maybe.withDefault s


nextSlide s =
    Zipper.next s |> Maybe.withDefault s


code attr language content =
    md attr ("```" ++ language ++ content ++ "```")


bullets_ attributes =
    column attributes << List.map (\s -> text <| "- " ++ s)


bullets attributes =
    column attributes << List.map (\e -> row [] [ text "- ", e ])


md attributes =
    el attributes << Element.html << Markdown.toHtml []


type alias Model =
    { slides : SlideShow Msg }


mapSlides f model =
    { model | slides = f model.slides }



-- UPDATE


type Msg
    = NoOp
    | KeyboardEvent KeyboardEvent


presentation : List (Element.Attribute Msg) -> List (Slide Msg) -> Platform.Program () Model Msg
presentation attr slides =
    let
        init : () -> ( Model, Cmd Msg )
        init _ =
            ( Model <| createSlideShow slides
            , Cmd.none
            )

        view : Model -> Html Msg
        view model =
            Element.layout [] <|
                el attr <|
                    Zipper.current model.slides

        update : Msg -> Model -> ( Model, Cmd Msg )
        update msg model =
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                KeyboardEvent { key } ->
                    let
                        _ =
                            Debug.log "Key Pressed" key
                    in
                    case key of
                        Just "ArrowRight" ->
                            ( mapSlides nextSlide model, Cmd.none )

                        Just "ArrowLeft" ->
                            ( mapSlides previousSlide model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        subscriptions : Model -> Sub Msg
        subscriptions model =
            Browser.Events.onKeyDown <| Decode.map KeyboardEvent decodeKeyboardEvent
    in
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

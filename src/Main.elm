module Main exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Slide exposing (..)



-- MAIN


composeExample : Element msg
composeExample =
    code [] "elm" """
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \\x -> f (g x)
compose f g x = f (g x)
(.) = compose
"""


box : Int -> Element msg
box c =
    el
        [ width (px 50)
        , height (px 50)
        , Background.color (rgb255 c c c)
        ]
        none


main : Program () Model Msg
main =
    presentation
        [ Font.family
            [ Font.typeface "Helvetica"
            , Font.monospace
            ]
        , centerX
        , centerY
        , scrollbars
        ]
        [ composeExample
        , box 223
        , bullets_ "· "
            []
            [ "test"
            , "test2"
            ]
        ]

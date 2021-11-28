module Main exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font exposing (monospace)
import Slide exposing (..)



-- MAIN


composeExample =
    code [] "elm" """
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \\x -> f (g x)
compose f g x = f (g x)
(.) = compose
"""


options =
    [ Font.family
        [ Font.typeface "Helvetica"
        , Font.monospace
        ]
    , centerX
    , centerY
    , scrollbars
    ]


main =
    presentation options
        [ composeExample
        , box 223
        , bullets_ "· "
            []
            [ "test"
            , "test2"
            ]
        ]


box c =
    el
        [ width (px 50)
        , height (px 50)
        , Background.color (rgb255 c c c)
        ]
        none

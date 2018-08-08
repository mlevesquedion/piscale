module Fonts exposing (..)

import Css exposing (..)


headerFont : Style
headerFont =
    Css.batch
        [ fontFamilies [ "Amatic SC", "Arial" ]
        , fontWeight bold
        ]


normalFont : Style
normalFont =
    Css.batch
        [ fontFamilies [ "Josefin Sans", "Times" ]
        , fontWeight normal
        ]

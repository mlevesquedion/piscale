module Pizza exposing (..)

import Html.Styled exposing (Html, text, table, thead, tr, tbody, td, input)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Css exposing (textAlign, center, margin, px)
import Fonts exposing (normalFont)


type alias Model =
    { diameter : String
    , price : String
    }


init : Model
init =
    { diameter = ""
    , price = ""
    }


type Msg
    = DiameterChanged String
    | PriceChanged String


area : Float -> Float
area diameter =
    pi * (diameter / 2) ^ 2


pizzaValue : Model -> Maybe Float
pizzaValue { diameter, price } =
    let
        diameter_ =
            String.toFloat diameter

        price_ =
            String.toFloat price
    in
        case ( diameter_, price_ ) of
            ( Ok d_, Ok p_ ) ->
                Just (area d_ / p_)

            _ ->
                Nothing


pizzaRatio : Model -> Model -> Maybe Float
pizzaRatio p1 p2 =
    let
        v1 =
            pizzaValue p1

        v2 =
            pizzaValue p2
    in
        case ( v1, v2 ) of
            ( Nothing, _ ) ->
                Nothing

            ( _, Nothing ) ->
                Nothing

            ( Just v1_, Just v2_ ) ->
                Just (v1_ / v2_ |> clamp 0.33 3)


update : Msg -> Model -> Model
update msg model =
    case msg of
        DiameterChanged newDiameter ->
            { model | diameter = newDiameter }

        PriceChanged newPrice ->
            { model | price = newPrice }


view : Model -> Html Msg
view model =
    table []
        [ thead []
            [ td [ css [ textAlign center, normalFont ] ] [ text "Diameter" ]
            , td [ css [ textAlign center, normalFont ] ] [ text "Price" ]
            ]
        , tbody []
            [ tr [ css [ margin (px 20) ] ]
                [ td []
                    [ input
                        [ (value model.diameter)
                        , (onInput DiameterChanged)
                        , css [ normalFont ]
                        ]
                        []
                    ]
                , td []
                    [ input
                        [ (value model.price)
                        , (onInput PriceChanged)
                        , css [ normalFont ]
                        ]
                        []
                    ]
                ]
            ]
        ]

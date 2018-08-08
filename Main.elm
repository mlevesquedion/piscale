module Main exposing (..)

import Html exposing (beginnerProgram)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Css exposing (..)
import Css.Colors as Colors
import Css.Transitions exposing (easeInOut, transition)
import Fonts exposing (headerFont, normalFont)
import Pizza


type alias Model =
    { left : Pizza.Model
    , right : Pizza.Model
    , deal : Maybe Deal
    }


type Either
    = L
    | R


type Deal
    = Left Float
    | Right Float


type Msg
    = PizzaMsg Either Pizza.Msg


init : Model
init =
    Model Pizza.init Pizza.init Nothing


pizzaImg : Float -> Html Msg
pizzaImg ratio =
    let
        factor =
            1.2
    in
        img
            [ src "pizza.png"
            , css
                [ width (px (ratio * factor))
                , height (px (ratio * factor))
                , transition
                    [ Css.Transitions.width3 200 0 easeInOut
                    , Css.Transitions.height3 200 0 easeInOut
                    ]
                ]
            ]
            []


viewDeal : Maybe Deal -> Html Msg
viewDeal deal =
    case deal of
        Nothing ->
            div
                [ css [ displayFlex, justifyContent center ] ]
                [ h2 [ css [ headerFont, fontSize (px 32) ] ] [ text "Fill in the fields below to find the best pizza deal!" ] ]

        Just deal_ ->
            case deal_ of
                Left ratio ->
                    div [ css [ displayFlex, justifyContent spaceAround, alignItems center ] ]
                        [ pizzaImg ratio
                        , pizzaImg 100.0
                        ]

                Right ratio ->
                    div [ css [ displayFlex, justifyContent spaceAround, alignItems center ] ]
                        [ pizzaImg 100.0
                        , pizzaImg ratio
                        ]


coloredBar : Color -> Html msg
coloredBar color_ =
    div [ css [ backgroundColor color_, color color_, width (pct 100) ] ]
        [ text "_" ]


italianRed : Color
italianRed =
    hex "A50B31"


italianGreen : Color
italianGreen =
    hex "026334"


backgroundColor_ : Color
backgroundColor_ =
    hex "FFFFFF"


githubLink : String
githubLink =
    "https://github.com/mlevesquedion"


viewFooter : Html Msg
viewFooter =
    div [ css [ position absolute, bottom (px 0), width (pct 100), displayFlex, justifyContent center, alignItems center ] ]
        [ p [ css [ normalFont, display inlineBlock, marginRight (px 5) ] ] [ text "Made by ", a [ href githubLink, css [ color Colors.black, textDecoration none ] ] [ text "@mlevesquedion" ], text ", for the love of pizza" ]
        , a [ href (githubLink ++ "/piscale") ]
            [ img [ src "gh_logo.png", css [ display inlineBlock, marginLeft (px 5), width (px 20), height (px 20) ] ] []
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ css [ backgroundColor backgroundColor_ ]
        ]
        [ h1
            [ css
                [ displayFlex
                , justifyContent center
                , headerFont
                , fontSize (px 48)
                , textShadow4 (px 0) (px 0) (px 5) (hex "F4AF27")
                ]
            ]
            [ text "PiScale" ]
        , coloredBar italianRed
        , coloredBar italianGreen
        , viewDeal model.deal
        , div
            [ css [ displayFlex, justifyContent spaceAround ] ]
            [ div []
                [ (Html.Styled.map (PizzaMsg L)) <| Pizza.view model.left
                ]
            , div []
                [ (Html.Styled.map (PizzaMsg R)) <| Pizza.view model.right
                ]
            ]
        , viewFooter
        ]


calculateDeal : Pizza.Model -> Pizza.Model -> Maybe Deal
calculateDeal left right =
    let
        ratio =
            Pizza.pizzaRatio left right
    in
        case ratio of
            Nothing ->
                Nothing

            Just ratio_ ->
                if ratio_ > 1 then
                    Just (Left (ratio_ * 100))
                else
                    Just (Right ((1 / ratio_) * 100))


update : Msg -> Model -> Model
update msg model =
    case msg of
        PizzaMsg L msg ->
            let
                newLeft =
                    Pizza.update msg model.left
            in
                { model | left = newLeft, deal = calculateDeal newLeft model.right }

        PizzaMsg R msg ->
            let
                newRight =
                    Pizza.update msg model.right
            in
                { model | right = newRight, deal = calculateDeal model.left newRight }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view >> toUnstyled
        , update = update
        }

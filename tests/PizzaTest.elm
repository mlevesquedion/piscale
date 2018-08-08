module PizzaTest exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Test exposing (..)
import Pizza


suite : Test
suite =
    describe "Pizza"
        [ describe "pizzaValue"
            [ test "Calculates a pizza's value" <|
                \_ ->
                    let
                        pizza =
                            Pizza.Model "8" "12"

                        value =
                            case Pizza.pizzaValue pizza of
                                Just value_ ->
                                    value_

                                Nothing ->
                                    0
                    in
                        Expect.within (Absolute 0.0001) value 4.18879
            , test "Given an invalid pizza, returns Nothing" <|
                \_ ->
                    let
                        pizza =
                            Pizza.Model "" ""

                        value =
                            Pizza.pizzaValue pizza
                    in
                        Expect.equal value Nothing
            ]
        , describe
            "pizzaRatio"
            [ test "Calculates the ratio of two pizzas' values" <|
                \_ ->
                    let
                        p1 =
                            Pizza.Model "12" "14"

                        p2 =
                            Pizza.Model "8" "12"

                        ratio =
                            case
                                Pizza.pizzaRatio p1 p2
                            of
                                Just ratio_ ->
                                    ratio_

                                Nothing ->
                                    0
                    in
                        Expect.within (Absolute 0.0001) ratio 1.92857
            , test "Caps the ratio at 3" <|
                \_ ->
                    let
                        p1 =
                            Pizza.Model "12" "2"

                        p2 =
                            Pizza.Model "8" "12"

                        ratio =
                            case
                                Pizza.pizzaRatio p1 p2
                            of
                                Just ratio_ ->
                                    ratio_

                                Nothing ->
                                    0
                    in
                        Expect.equal ratio 3
            , test "Caps the ratio at 0.33" <|
                \_ ->
                    let
                        p1 =
                            Pizza.Model "12" "14"

                        p2 =
                            Pizza.Model "8" "2"

                        ratio =
                            case
                                Pizza.pizzaRatio p1 p2
                            of
                                Just ratio_ ->
                                    ratio_

                                Nothing ->
                                    0
                    in
                        Expect.equal ratio 0.33
            , test "Given an invalid pizza, returns Nothing" <|
                \_ ->
                    let
                        p1 =
                            Pizza.Model "" ""

                        p2 =
                            Pizza.Model "12" "14"

                        ratio =
                            Pizza.pizzaRatio p1 p2
                    in
                        Expect.equal ratio Nothing
            ]
        ]

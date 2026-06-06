module DeterministicPrograms where

-- ============================================================
-- Task 1: first solution
-- ============================================================

threeNumbersFirst :: String
threeNumbersFirst =
    concat
        [
            "(begin ",

            "(define search ",
                "(lambda (x y z) ",

                    "(if (> x 11) ",
                        "(quote fail) ",

                        "(if (> y 11) ",
                            "(search (+ x 1) 0 0 ) ",

                            "(if (> z 11) ",
                                "(search x (+ y 1) 0) ",

                                "(if ",
                                    "(and ",
                                        "(distinct? (list x y z)) ",
                                        "(= (+ x y z) 15) ",
                                        "(< x y) ",
                                        "(< y z)) ",

                                    "(list x y z) ",

                                    "(search x y (+ z 1)))))))) ",

            "(search 0 0 0))"
        ]

-- ============================================================
-- Task 1: all solutions
-- ============================================================

threeNumbersAll :: String
threeNumbersAll =
    concat
        [
            "(begin ",

            "(define search ",
                "(lambda (x y z results) ",

                    "(if (> x 11) ",
                        "results ",

                        "(if (> y 11) ",
                            "(search (+ x 1) 0 0 results) ",

                            "(if (> z 11) ",
                                "(search x (+ y 1) 0 results) ",

                                "(if ",
                                    "(and ",
                                        "(distinct? (list x y z)) ",
                                        "(= (+ x y z) 15) ",
                                        "(< x y) ",
                                        "(< y z)) ",

                                    "(search x y (+ z 1) ",
                                        "(cons (list x y z) results)) ",

                                    "(search x y (+ z 1) results))))))) ",

            "(search 0 0 0 (list)))"
        ]

-- ============================================================
-- Task 2: SICP Multiple Dwelling, first solution
-- ============================================================

multipleDwellingFirst :: String
multipleDwellingFirst =
    concat
        [
            "(begin ",

            "(define search ",
                "(lambda (baker cooper fletcher miller smith) ",

                    "(if (> baker 5) ",
                        "(quote fail) ",

                        "(if (> cooper 5) ",
                            "(search (+ baker 1) 1 1 1 1) ",

                            "(if (> fletcher 5) ",
                                "(search baker (+ cooper 1) 1 1 1) ",

                                "(if (> miller 5) ",
                                    "(search baker cooper (+ fletcher 1) 1 1) ",

                                    "(if (> smith 5) ",
                                        "(search baker cooper fletcher (+ miller 1) 1) ",

                                        "(if ",
                                            "(and ",
                                                "(distinct? (list baker cooper fletcher miller smith)) ",
                                                "(not (= baker 5)) ",
                                                "(not (= cooper 1)) ",
                                                "(not (= fletcher 5)) ",
                                                "(not (= fletcher 1)) ",
                                                "(> miller cooper) ",
                                                "(not (= (abs (- smith fletcher)) 1)) ",
                                                "(not (= (abs (- fletcher cooper)) 1))) ",

                                            "(list ",
                                                "(list (quote baker) baker) ",
                                                "(list (quote cooper) cooper) ",
                                                "(list (quote fletcher) fletcher) ",
                                                "(list (quote miller) miller) ",
                                                "(list (quote smith) smith)) ",

                                            "(search baker cooper fletcher miller (+ smith 1)))))))))) ",

            "(search 1 1 1 1 1))"
        ]

-- ============================================================
-- Task 2: SICP Multiple Dwelling, all solutions
-- ============================================================

multipleDwellingAll :: String
multipleDwellingAll =
    concat
        [
            "(begin ",

            "(define search ",
                "(lambda (baker cooper fletcher miller smith results) ",

                    "(if (> baker 5) ",
                        "results ",

                        "(if (> cooper 5) ",
                            "(search (+ baker 1) 1 1 1 1 results) ",

                            "(if (> fletcher 5) ",
                                "(search baker (+ cooper 1) 1 1 1 results) ",

                                "(if (> miller 5) ",
                                    "(search baker cooper (+ fletcher 1) 1 1 results) ",

                                    "(if (> smith 5) ",
                                        "(search baker cooper fletcher (+ miller 1) 1 results) ",

                                        "(if ",
                                            "(and ",
                                                "(distinct? (list baker cooper fletcher miller smith)) ",
                                                "(not (= baker 5)) ",
                                                "(not (= cooper 1)) ",
                                                "(not (= fletcher 5)) ",
                                                "(not (= fletcher 1)) ",
                                                "(> miller cooper) ",
                                                "(not (= (abs (- smith fletcher)) 1)) ",
                                                "(not (= (abs (- fletcher cooper)) 1))) ",

                                            "(search baker cooper fletcher miller (+ smith 1) ",
                                                "(cons ",
                                                    "(list ",
                                                        "(list (quote baker) baker) ",
                                                        "(list (quote cooper) cooper) ",
                                                        "(list (quote fletcher) fletcher) ",
                                                        "(list (quote miller) miller) ",
                                                        "(list (quote smith) smith)) ",
                                                    "results)) ",

                                            "(search baker cooper fletcher miller (+ smith 1) results))))))))) ",

            "(search 1 1 1 1 1 (list)))"
        ]
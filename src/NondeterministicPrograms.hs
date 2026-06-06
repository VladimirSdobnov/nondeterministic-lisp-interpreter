module NondeterministicPrograms where

-- ============================================================
-- Task 1: first solution
-- ============================================================

threeNumbersFirst :: String
threeNumbersFirst =
    concat
        [
            "(begin ",

            "(define x (amb 0 1 2 3 4 5 6 7 8 9 10 11)) ",
            "(define y (amb 0 1 2 3 4 5 6 7 8 9 10 11)) ",
            "(define z (amb 0 1 2 3 4 5 6 7 8 9 10 11)) ",

            "(require (distinct? (list x y z))) ",
            "(require (= (+ x y z) 15)) ",
            "(require (< x y)) ",
            "(require (< y z)) ",

            "(list x y z))"
        ]

-- ============================================================
-- Task 1: all solutions
-- ============================================================

threeNumbersAll :: String
threeNumbersAll =
    concat
        [
            "(all-solutions ",
                "(begin ",

                "(define x (amb 0 1 2 3 4 5 6 7 8 9 10 11)) ",
                "(define y (amb 0 1 2 3 4 5 6 7 8 9 10 11)) ",
                "(define z (amb 0 1 2 3 4 5 6 7 8 9 10 11)) ",

                "(require (distinct? (list x y z))) ",
                "(require (= (+ x y z) 15)) ",
                "(require (< x y)) ",
                "(require (< y z)) ",

                "(list x y z)))"
        ]

-- ============================================================
-- Task 2: SICP Multiple Dwelling, first solution
-- ============================================================

multipleDwellingFirst :: String
multipleDwellingFirst =
    concat
        [
            "(begin ",

            "(define baker (amb 1 2 3 4)) ",
            "(define cooper (amb 2 3 4 5)) ",
            "(define fletcher (amb 2 3 4)) ",
            "(define miller (amb 1 2 3 4 5)) ",
            "(define smith (amb 1 2 3 4 5)) ",

            "(require (distinct? (list baker cooper fletcher miller smith))) ",
            "(require (> miller cooper)) ",
            "(require (not (= (abs (- smith fletcher)) 1))) ",
            "(require (not (= (abs (- fletcher cooper)) 1))) ",

            "(list ",
                "(list (quote baker) baker) ",
                "(list (quote cooper) cooper) ",
                "(list (quote fletcher) fletcher) ",
                "(list (quote miller) miller) ",
                "(list (quote smith) smith))",

            ")"
        ]

-- ============================================================
-- Task 2: SICP Multiple Dwelling, all solutions
-- ============================================================

multipleDwellingAll :: String
multipleDwellingAll =
    concat
        [
            "(all-solutions ",
                "(begin ",

                "(define baker (amb 1 2 3 4)) ",
                "(define cooper (amb 2 3 4 5)) ",
                "(define fletcher (amb 2 3 4)) ",
                "(define miller (amb 1 2 3 4 5)) ",
                "(define smith (amb 1 2 3 4 5)) ",

                "(require (distinct? (list baker cooper fletcher miller smith))) ",
                "(require (> miller cooper)) ",
                "(require (not (= (abs (- smith fletcher)) 1))) ",
                "(require (not (= (abs (- fletcher cooper)) 1))) ",

                "(list ",
                    "(list (quote baker) baker) ",
                    "(list (quote cooper) cooper) ",
                    "(list (quote fletcher) fletcher) ",
                    "(list (quote miller) miller) ",
                    "(list (quote smith) smith))",

                "))"
        ]
module Main where

import Test.HUnit
import DAG
import Equivalence
import Interface

tr1 = TestCase (assertEqual "Transform1" (transform 2 [(1, 2)]) [[], [1]])
tr2 = TestCase (assertEqual "Transform2" (transform 4 [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]) [[], [1], [1, 2], [1, 2, 3]])
tr3 = TestCase (assertEqual "Transform3" (transform 3 []) [[], [], []])
tr4 = TestCase (assertEqual "Transform4" (transform 5 [(5, 3), (5, 1), (3, 1), (4, 2)]) [[3, 5], [4], [5], [], []])
tr5 = TestCase (assertEqual "Transform5" (transform 6 [(2, 4), (2, 6), (1, 5), (3, 6), (5, 6)]) [[], [], [], [2], [1], [2, 3, 5]])

transTests = TestList [tr1, tr2, tr3, tr4, tr5]

tp1 = TestCase (assertEqual "Toposort1" (toposort [[], [1]]) [1, 2])
tp2 = TestCase (assertEqual "Toposort2" (toposort [[], [1], [1, 2], [1, 2, 3]]) [1, 2, 3, 4])
tp3 = TestCase (assertEqual "Toposort3" (toposort [[], [], []]) [1, 2, 3])
tp4 = TestCase (assertEqual "Toposort4" (toposort [[5, 3], [4], [5], [], []]) [4, 5, 2, 3, 1])
tp5 = TestCase (assertEqual "Toposort5" (toposort [[], [], [], [2], [1], [2, 3, 5]]) [1, 2, 3, 4, 5, 6])

topoTests = TestList [tp1, tp2, tp3, tp4, tp5]

dag1 = transform 2 [(1, 2)]
dag2 = transform 2 [(2, 1)]
dag3 = transform 3 [(1, 2), (2, 3)]
dag4 = transform 3 [(1, 2), (3, 2)]
dag5 = transform 3 [(2, 1), (2, 3)]
dag6 = transform 3 [(2, 1), (3, 1)]
dag7 = transform 5 []
dag8 = transform 5 []
dag9 = transform 4 [(1, 2), (2, 3), (3, 4), (1, 3)]
dag10 = transform 4 [(4, 3), (3, 2), (2, 1), (4, 2)]
dag11 = transform 4 [(4, 3), (3, 2), (2, 1), (4, 2), (4, 1)]

mt1 = TestCase (assertEqual "dagEqv1" (dagEqv dag1 dag2) True)
mt2 = TestCase (assertEqual "dagEqv2" (dagEqv dag2 dag1) True)
mt3 = TestCase (assertEqual "dagEqv3" (dagEqv dag1 dag3) False)
mt4 = TestCase (assertEqual "dagEqv4" (dagEqv dag3 dag4) False)
mt5 = TestCase (assertEqual "dagEqv5" (dagEqv dag3 dag5) False)
mt6 = TestCase (assertEqual "dagEqv6" (dagEqv dag4 dag5) False)
mt7 = TestCase (assertEqual "dagEqv7" (dagEqv dag6 dag4) True)
mt8 = TestCase (assertEqual "dagEqv8" (dagEqv dag7 dag8) True)
mt9 = TestCase (assertEqual "dagEqv9" (dagEqv dag9 dag10) True)
mt10 = TestCase (assertEqual "dagEqv10" (dagEqv dag9 dag11) False)

matchTests = TestList [mt1, mt2, mt3, mt4, mt5, mt6, mt7, mt8, mt9, mt10]

tests = TestList [transTests, topoTests, matchTests]

main = runTestTT tests

import Test.HUnit

import Tokenizer
import RedBlack

-- HUnit test cases for the tokenize function
testTokenize :: Test
testTokenize = TestList
  [
    TestCase (assertEqual "Handles empty string"
      [] (tokenize "")),
    TestCase (assertEqual "Handles a simple sentence"
      ["hello", "world"] (tokenize "Hello World")),
    TestCase (assertEqual "Handles a double space"
      ["hello", "world"] (tokenize "Hello  World")),
    TestCase (assertEqual "Handles mixed-case and numbers"
      ["test", "123", "abc"] (tokenize "Test 123, ABC")),
    TestCase (assertEqual "Handles special characters and punctuation"
      ["thisis", "a", "test", "case"] (tokenize "This-is a!@# test, case...")),
    TestCase (assertEqual "Handles numbers and special characters"
      ["123", "456"] (tokenize "123!!! 456???"))
  ]

testBalance :: Test
testBalance = TestList
  [
    TestCase (assertEqual "LL-Violation"
      (Node Red (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty)))
      (balance (Node Black (Node Red (Node Red (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black Empty 5 Empty)) 6 (Node Black Empty 7 Empty)))),
    TestCase (assertEqual "LR-Violation"
      (Node Red (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty)))
      (balance (Node Black (Node Red (Node Black Empty 1 Empty) 2 (Node Red (Node Black Empty 3 Empty) 4 (Node Black Empty 5 Empty))) 6 (Node Black Empty 7 Empty)))),
    TestCase (assertEqual "RL-Violation"
      (Node Red (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty)))
      (balance (Node Black (Node Black Empty 1 Empty) 2 (Node Red (Node Red (Node Black Empty 3 Empty) 4 (Node Black Empty 5 Empty)) 6 (Node Black Empty 7 Empty))))),
    TestCase (assertEqual "RR-Violation"
      (Node Red (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty)))
      (balance (Node Black (Node Black Empty 1 Empty) 2 (Node Red (Node Black Empty 3 Empty) 4 (Node Red (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty)))))),
    TestCase (assertEqual "no violation"
      (Node Red (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty)))
      (balance (Node Red (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty)))))
  ]

testInOrder :: Test
testInOrder = TestList
  [
    TestCase (assertEqual "List"
      [1,2,3,4,5,6,7]
      (inOrder (Node Red (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)) 4 (Node Black (Node Black Empty 5 Empty) 6 (Node Black Empty 7 Empty))))),
--    TestCase (assertEqual "Empty"
--      [] :: [Int]
--      (inOrder (Empty :: Tree Int))),
    TestCase (assertEqual "single Element"
      [42]
      (inOrder (Node Black Empty 42 Empty)))
  ]

testInsert :: Test
testInsert = TestList
  [
    TestCase (assertEqual "First"
      (Node Black Empty 1 Empty)
      (insert 1 Empty)),
    TestCase (assertEqual "Unbalanced"
      (Node Black (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty))
      (insert 3 (Node Black Empty 1 (Node Red Empty 2 Empty)))),
    TestCase (assertEqual "Duplicate"
      (Node Black Empty 1 (Node Red Empty 2 Empty))
      (insert 2 (Node Black Empty 1 (Node Red Empty 2 Empty))))
  ]

-- Run the tests
main :: IO ()
main = do
  _ <- runTestTT testTokenize
  _ <- runTestTT testBalance
  _ <- runTestTT testInOrder
  _ <- runTestTT testInsert
  return ()
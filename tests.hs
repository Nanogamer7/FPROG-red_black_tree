import Test.HUnit

import Tokenizer
import RedBlack

-- HUnit test cases for the tokenize function
testTokenize :: Test
testTokenize = TestList
  [ TestCase (assertEqual "Handles empty string"
      [] (tokenize "")),
    TestCase (assertEqual "Handles a simple sentence"
      ["hello", "world"] (tokenize "Hello, World!")),
    TestCase (assertEqual "Handles mixed-case and numbers"
      ["test", "123", "abc"] (tokenize "Test 123, ABC")),
    TestCase (assertEqual "Handles special characters and punctuation"
      ["this", "is", "a", "test", "case"] (tokenize "This-is a!@# test, case...")),
    TestCase (assertEqual "Handles numbers and special characters"
      ["123", "456"] (tokenize "123!!! 456???"))
  ]

-- Run the tests
main :: IO ()
main = do
  _ <- runTestTT testTokenize
  return ()
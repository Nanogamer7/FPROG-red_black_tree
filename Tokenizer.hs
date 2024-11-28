module Tokenizer where

import Data.Char (isAlpha, toLower, isPunctuation, isDigit)

tokenize :: String -> [String]
tokenize str = words (map (\char -> case () of
        _ | isAlpha char -> toLower char
          | isDigit char -> char
          | otherwise    -> ' ') (filter (not . isPunctuation) str))
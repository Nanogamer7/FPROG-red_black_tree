import System.Random (randomRIO)
import System.Random (randomRIO)
import Data.Char (isAlpha, toLower)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map
import System.IO (writeFile)

data Color = Red | Black deriving Show -- `Show` makes it printable, according to the internet
data Tree a = Empty | Node Color (Tree a) a (Tree a) deriving Show -- Tree is either empty, or consists of a Node with properties color, left subtree, value, and  right subtree 

insert :: Ord a => a -> Tree a -> Tree a
insert x t = makeBlack (ins t)
  where
    ins Empty = Node Red Empty x Empty -- if inserting into a new tree, return a red node with two empty sub-trees, and the inserted value
    ins (Node color left y right) -- insert into existing tree
      | x < y = balance (Node color (ins left) y right) -- call recursive on left side if new value is smaller
      | x == y = Node color left y right -- do nothing; possible to increment word count if data type allows it
      | x > y = balance (Node color left y (ins right))
    makeBlack (Node _ left x right) = Node Black left x right -- root node needs to be black -> otherwise black violation if one of children empty

insertList :: Ord a => [a] -> Tree a -> Tree a
insertList [] tree = tree
insertList (x:xs) tree = insertList xs (insert x tree) -- first element inserted, rest passed as list to next iteration

balance :: Tree a -> Tree a -- order stays visually the same - trees and values stay in same order
balance (Node _ (Node Red (Node Red tree1 x tree2) y tree3) z tree4) = Node Red (Node Black tree1 x tree2) y (Node Black tree3 z tree4)
balance (Node _ tree1 x (Node Red tree2 y (Node Red tree3 z tree4))) = Node Red (Node Black tree1 x tree2) y (Node Black tree3 z tree4)
balance (Node _ (Node Red tree1 x (Node Red tree2 y tree3)) z tree4) = Node Red (Node Black tree1 x tree2) y (Node Black tree3 z tree4)
balance (Node _ tree1 x (Node Red (Node Red tree2 y tree3) z tree4)) = Node Red (Node Black tree1 x tree2) y (Node Black tree3 z tree4)
balance (Node Red (Node Red tree1 x tree2) y (Node Red tree3 z tree4)) = Node Red (Node Black tree1 x tree2) y (Node Black tree3 z tree4)
balance tree = tree -- default case

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node _ left x right) = inOrder left ++ [x] ++ inOrder right -- recursively call with left subtree -> append value -> recursively call with right subtree

-- for testing purposes
generateRandomList :: Int -> IO [Int]
generateRandomList n = mapM (const $ randomRIO (1, 100)) [1..n]

insertWord :: Ord a => (a, Int) -> Tree (a, Int) -> Tree (a, Int)
insertWord (word, count) Empty = Node Red Empty (word, count) Empty
insertWord (word, count) (Node color left (w, c) right)
  | word < w  = balance (Node color (insertWord (word, count) left) (w, c) right)
  | word == w = Node color left (w, c + count) right -- Increment count if word matches
  | word > w  = balance (Node color left (w, c) (insertWord (word, count) right))

readWordsFromFile :: FilePath -> IO [String]
readWordsFromFile path = do
  content <- readFile path
  let wordList = splitOneOf " ,.!?;:\n\r" content
  return $ map (map toLower . filter isAlpha) $ filter (not . null) wordList

{-
countWords :: [String] -> Tree (String, Int)
countWords words = foldr insertWord Empty (Map.toList wordCounts)
  where
    wordCounts = Map.fromListWith (+) [(word, 1) | word <- words]
    insertWord (word, count) tree = insert (word, count) tree
-}

countWords :: [String] -> Tree (String, Int)
countWords = foldr (\word tree -> insertWord (word, 1) tree) Empty

main :: IO ()
main = do
  putStrLn "Enter the path of the text file to process:"
  filePath <- getLine
  wordsList <- readWordsFromFile filePath
  let wordTree = countWords wordsList

  --putStrLn "In-order traversal of the tree with word counts:"
  --print (inOrder wordTree)
  let inOrderOutput = inOrder wordTree
  let outputFileName = "countedWords.txt"
  writeFile outputFileName (unlines (map show inOrderOutput))
  putStrLn $ "Word counts saved to: " ++ outputFileName
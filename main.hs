import System.Random (randomRIO)

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

main :: IO ()
main = do
  numbers <- generateRandomList 15
  let tree = insertList numbers Empty

  print tree
  print (inOrder tree)
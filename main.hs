data Color = Red | Black deriving Show -- `Show` makes it printable, according to the internet
data Tree a = Empty | Node Color (Tree a) a (Tree a) deriving Show -- Tree is either empty, or consists of a Node with properties color, left subtree, value, and  right subtree 

insert :: Ord a => a -> Tree a -> Tree a
insert x t = ins t
  where
    ins Empty = Node Black Empty x Empty -- if inserting into a new tree, simply return a black node with two empty sub-trees, and the inserted value
    ins (Node color left y right) -- insert into existing tree
      | x < y = Node color (ins left) y right -- call recursive on left side if new value is smaller
      | x == y = Node color left y right -- do nothing; later increment word count I guess?
      | x > y = Node color left y (ins right)

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node _ left x right) = inOrder left ++ [x] ++ inOrder right -- recursively call with left subtree -> append value -> recursively call with right subtree

main :: IO ()
main = do
  let emptyTree = Empty
  let tree1 = insert 10 emptyTree -- insert first value
  let tree2 = insert 5 tree1 -- insert smaller value
  let tree3 = insert 15 tree2 -- insert larger value
  let tree4 = insert 12 tree3 -- insert larger -> smaller value
  let tree5 = insert 10 tree4 -- insert duplicate


  print tree1
  print tree2
  print tree3
  print tree4
  print tree5
  print (inOrder tree5)
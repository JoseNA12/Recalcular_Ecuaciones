data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)
     deriving (Show)

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert el EmptyTree  = Node el EmptyTree EmptyTree
treeInsert el (Node a left right)
       | el == a = Node el left right
       | el < a = Node a (treeInsert el left) right
       | el > a = Node a left (treeInsert el right)

treeFromList :: Ord a => [a] -> BinaryTree a
treeFromList list = foldr treeInsert EmptyTree list

flattenTree :: BinaryTree a -> [a]
flattenTree EmptyTree = []
flattenTree (Node a left right) = flattenTree left ++ [a] ++ flattenTree right

treeElement :: Ord a => a -> BinaryTree a -> Bool
treeElement el EmptyTree = False
treeElement el (Node a left right) 
            | el == a = True
            | el < a  = treeElement el left
            | el > a  = treeElement el right

treeMinimum :: BinaryTree a -> Maybe a
treeMinimum EmptyTree = Nothing
treeMinimum (Node a EmptyTree right) = Just a
treeMinimum (Node a left right) = treeMinimum left

treeMaximum :: BinaryTree a -> Maybe a
treeMaximum EmptyTree = Nothing
treeMaximum (Node a left EmptyTree) = Just a
treeMaximum (Node a left right) = treeMaximum right

inorder :: Ord a => BinaryTree a -> [a]
inorder = flattenTree

preorder :: Ord a => BinaryTree a -> [a]
preorder EmptyTree = []
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

postorder :: Ord a => BinaryTree a -> [a]
postorder EmptyTree = []
postorder (Node a left right) = postorder left ++ postorder right ++ [a]

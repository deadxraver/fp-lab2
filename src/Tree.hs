module Tree
    ( TreeNode(..),
    fromList,
    insert,
    remove,
    emptyTree
    ) where

data TreeNode = TreeNode Char [TreeNode] | TreeHead [TreeNode] | TreeLeaf deriving (Eq, Show)

emptyTree :: TreeNode
emptyTree = TreeHead [TreeLeaf]

insert :: String -> TreeNode -> TreeNode
insert str node = node -- TODO: 

remove :: String -> TreeNode -> TreeNode
remove str node = node -- TODO: 

fromList' :: [String] -> TreeNode -> TreeNode
fromList' list node = node -- TODO:

fromList :: [String] -> TreeNode
fromList list = fromList' list emptyTree

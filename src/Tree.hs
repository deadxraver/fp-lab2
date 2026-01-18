module Tree
    ( TreeNode
    ) where

data TreeNode = TreeNode Char [TreeNode] | TreeHead [TreeNode] | TreeLeaf deriving (Eq, Show)

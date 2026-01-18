module Tree
    ( TreeNode
    ) where

data TreeNode = TreeNode Char [TreeNode] | TreeHead [TreeNode] | TreeLeaf deriving (Eq, Show)
-- TODO: define show by myself

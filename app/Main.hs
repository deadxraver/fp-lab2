module Main (main) where

import Tree

someNode :: TreeNode
someNode = TreeNode 'A' [TreeLeaf]

headNode :: TreeNode
headNode = TreeHead [someNode]

main :: IO ()
main = do
    print headNode
    print $ toList headNode

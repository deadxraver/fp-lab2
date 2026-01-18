module Main (main) where

import Tree

someNode :: TreeNode
someNode = TreeNode 'A' [LeafNode]

headNode = HeadNode [someNode]

main :: IO ()
main = putStrLn show headNode

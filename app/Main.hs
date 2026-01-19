module Main (main) where

import Tree

someNode :: TreeNode
someNode = TreeNode 'A' [TreeLeaf]

headNode :: TreeNode
headNode = TreeHead [someNode]

nodeFromStr :: TreeNode
nodeFromStr = insert "hell" emptyTree

main :: IO ()
main = do
    print headNode
    print $ toList headNode
    print nodeFromStr
    print $ toList nodeFromStr
    print $ toList $ insert "hello" nodeFromStr
    print $ toList $ remove "hell" $ insert "hello" nodeFromStr
    print $ remove "hello" (fromList ["hello"])
    print $ toList $ fromList ["Abab", "AZaz"]
    print $ toList $ fromList ["hello", "hello"]

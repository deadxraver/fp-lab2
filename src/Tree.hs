module Tree
    ( TreeNode(..),
    fromList,
    toList,
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
fromList' [] node = node
fromList' (word:list) node = insert word (fromList' list node)

fromList :: [String] -> TreeNode
fromList list = fromList' list emptyTree

-- current node -- str acc -- results list
toList' :: TreeNode -> String -> [String]
toList' TreeLeaf str = [str]
toList' (TreeNode sym (node:nodeList)) str = toList' node (str ++ sym:[]) ++ toList' (TreeNode sym nodeList) str
toList' (TreeNode _ []) _ = []
toList' (TreeHead _) _ = error "TreeHead should not be here"

toList :: TreeNode -> [String]
toList (TreeHead []) = []
toList (TreeHead (node:list)) = (toList' node "") ++ (toList (TreeHead list))

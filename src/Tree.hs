module Tree (
    TreeNode (..),
    fromList,
    toList,
    insert,
    remove,
    clear,
    emptyTree,
) where

import Data.List (find)

data TreeNode = TreeNode Char [TreeNode] | TreeHead [TreeNode] | TreeLeaf deriving (Eq, Show)

emptyTree :: TreeNode
emptyTree = TreeHead [TreeLeaf]

getChildren :: TreeNode -> [TreeNode]
getChildren (TreeNode _ list) = list
getChildren (TreeHead list) = list
getChildren TreeLeaf = []

isHead :: TreeNode -> Bool
isHead (TreeHead _) = True
isHead _ = False

hasLetter :: Char -> TreeNode -> Bool
hasLetter c node = any checkChild (getChildren node)
  where
    checkChild (TreeNode c' _) = c' == c
    checkChild _ = False

findChild :: Char -> [TreeNode] -> Maybe TreeNode
findChild c = find checkChild
  where
    checkChild (TreeNode c' _) = c' == c
    checkChild _ = False

isLeaf :: TreeNode -> Bool
isLeaf TreeLeaf = True
isLeaf _ = False

replaceChild :: TreeNode -> TreeNode -> [TreeNode] -> [TreeNode]
replaceChild old new = map replace
  where
    replace x = if x == old then new else x

insert :: String -> TreeNode -> TreeNode
insert "" (TreeHead list) = TreeHead list -- neutral element
insert (c : str) (TreeHead list) =
    -- insert into head
    case findChild c list of
        Nothing -> TreeHead (insert str (TreeNode c []) : list) -- insert new
        Just node -> TreeHead (replaceChild node (insert str (TreeNode c (getChildren node))) list) -- replace & move forward through that node
insert "" (TreeNode c list) =
    -- end of string somewhere
    if any isLeaf list
        then TreeNode c list -- base case
        else TreeNode c (TreeLeaf : list) -- add leaf
insert (c : str) (TreeNode c' list) =
    -- insert to node between head and leaf
    case findChild c list of
        Nothing -> TreeNode c' (insert str (TreeNode c []) : list) -- insert new
        Just node -> TreeNode c' (replaceChild node (insert str (TreeNode c (getChildren node))) list) -- replace & move forward through that node

remove :: String -> TreeNode -> TreeNode
remove "" (TreeHead list) = TreeHead list -- neutral elem
remove "" (TreeNode c list) = TreeNode c (filter (/= TreeLeaf) list)
remove "" TreeLeaf = TreeLeaf
remove _ TreeLeaf = TreeLeaf
remove (c : str) (TreeHead list) =
    case findChild c list of
        Nothing -> TreeHead list
        Just node ->
            if isHead node'
                then TreeHead (filter (/= node) list)
                else TreeHead $ replaceChild node node' list
          where
            node' = remove str node
remove (c : str) (TreeNode c' list) =
    case findChild c list of
        Nothing -> TreeHead list
        Just node ->
            if isHead node'
                then if length list == 1 then TreeHead [] else TreeNode c' (filter (/= node) list)
                else TreeNode c' $ replaceChild node node' list
          where
            node' = remove str node

fromList' :: [String] -> TreeNode -> TreeNode
fromList' list node = foldr insert node list

fromList :: [String] -> TreeNode
fromList list = fromList' list emptyTree

-- current node -- str acc -- results list
toList' :: TreeNode -> String -> [String]
toList' TreeLeaf str = [str]
toList' (TreeNode sym (node : nodeList)) str = toList' node (str ++ [sym]) ++ toList' (TreeNode sym nodeList) str
toList' (TreeNode _ []) _ = []
toList' (TreeHead _) _ = error "TreeHead should not be here"

toList :: TreeNode -> [String]
toList (TreeHead []) = []
toList (TreeHead (node : list)) = toList' node "" ++ toList (TreeHead list)

clear :: TreeNode -> TreeNode
clear (TreeHead _) = emptyTree
clear _ = error "Passed not head"

map' :: (String -> String) -> TreeNode -> TreeNode
map' f (TreeHead list) = fromList (map f (toList (TreeHead list)))
map' _ _ = error "Map should be applied to head"

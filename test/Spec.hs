import Tree

list1dup :: [String]
list1dup = ["hello", "hello"]
list1 :: [String]
list1 = ["hello"]

main :: IO ()
main = do
    setTest

setTest :: IO ()
setTest = print $ if fromList list1 == fromList list1dup then "Set Test passed" else error "Set Test failed"

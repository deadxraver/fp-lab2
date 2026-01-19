import Tree

list1dup :: [String]
list1dup = ["hello", "hello"]
list1 :: [String]
list1 = ["hello"]

main :: IO ()
main = do
    setTest
    mapTest
    removeTest

runTest :: Bool -> String -> IO ()
runTest cond testName = print $ if cond then testName ++ " test passed" else error (testName ++ " test failed")

setTest :: IO ()
setTest = runTest (fromList list1 == fromList list1dup) "Set"

removeTest :: IO ()
removeTest = runTest (emptyTree == remove "hello" (fromList ["hello"])) "Remove"

mapTest :: IO ()
mapTest = runTest (map' toUpperCase (fromList ["hello"]) == fromList ["HELLO"]) "Map"

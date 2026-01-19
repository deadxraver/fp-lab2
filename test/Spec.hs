import Tree

list1dup :: [String]
list1dup = ["hello", "hello"]
list1 :: [String]
list1 = ["hello"]

main :: IO ()
main = do
    setTest
    mapTest
    filterTest
    removeTest
    prefixTest

runTest :: Bool -> String -> IO ()
runTest cond testName = print $ if cond then testName ++ " test passed" else error (testName ++ " test failed")

setTest :: IO ()
setTest = runTest (fromList list1 == fromList list1dup) "Set"

removeTest :: IO ()
removeTest = runTest (emptyTree == remove "hello" (fromList ["hello"])) "Remove"

mapTest :: IO ()
mapTest = runTest (map' toUpperCase (fromList ["hello"]) == fromList ["HELLO"]) "Map"

filterTest :: IO ()
filterTest = runTest (filter' (\s -> length s > 5) (fromList ["1234", "12345", "123456"]) == fromList ["123456"]) "Filter"

prefixTest :: IO ()
prefixTest = runTest (startWith "ab" (fromList ["a", "ab", "aba", "abb", "abba", "ba", "bba"]) == ["ab", "aba", "abb", "abba"]) "Prefix"

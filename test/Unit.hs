module Unit (
    unitTests,
)
where

import Tree

list1dup :: [String]
list1dup = ["hello", "hello"]
list1 :: [String]
list1 = ["hello"]

unitTests :: IO ()
unitTests = do
    putStrLn "\nStarting unit tests..."
    setTest
    mapTest
    filterTest
    removeTest
    prefixTest
    monoTest1
    monoTest2
    putStrLn "All unit tests passed\n"

runTest :: Bool -> String -> IO ()
runTest cond testName = putStrLn $ if cond then testName ++ " test passed" else error (testName ++ " test failed")

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

monoTest1 :: IO ()
monoTest1 = runTest (fromList ["hello"] <> emptyTree == fromList ["hello"]) "Mono1"

monoTest2 :: IO ()
monoTest2 = runTest ((fromList ["str1"] <> fromList ["str2"]) <> fromList ["str3"] == fromList ["str1"] <> (fromList ["str2"] <> fromList ["str3"])) "Mono2"

module PropertyBased (
    propertyBasedTests,
)
where

import Data.Char (isAlpha, toUpper)
import Data.List (isPrefixOf, nub, sort)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Tree

genWord :: Gen String
genWord = listOf1 (elements ['a' .. 'z'])

genWordList :: Gen [String]
genWordList = listOf genWord

genPrefix :: Gen String
genPrefix = listOf (elements ['a' .. 'z'])

newtype TreeWrapper = TreeWrapper TreeNode deriving (Show)

instance Arbitrary TreeWrapper where
    arbitrary = do TreeWrapper . fromList <$> genWordList
    shrink (TreeWrapper tree) =
        let words = toList tree
            smallerLists = shrinkList shrinkWord words
         in map (TreeWrapper . fromList) smallerLists
      where
        shrinkWord s = filter (not . null) (shrink s)

prop_insert_empty :: Property
prop_insert_empty =
    forAll arbitrary $ \(TreeWrapper tree) ->
        insert "" tree === tree

prop_from_to_inverse :: Property
prop_from_to_inverse =
    forAll genWordList $ \words ->
        let uniqueWords = nub words
            tree = fromList uniqueWords
            extracted = toList tree
         in sort extracted === sort uniqueWords

prop_union_correct :: Property
prop_union_correct =
    forAll genWordList $ \words1 ->
        forAll genWordList $ \words2 ->
            let tree1 = fromList words1
                tree2 = fromList words2
                unionTree = tree1 <> tree2
                expected = nub (words1 ++ words2)
             in sort (toList unionTree) === sort expected

prop_empty_neutral :: Property
prop_empty_neutral =
    forAll arbitrary $ \(TreeWrapper tree) ->
        (emptyTree <> tree === tree) .&&. (tree <> emptyTree === tree)

prop_union_associative :: Property
prop_union_associative =
    forAll arbitrary $ \(TreeWrapper t1) ->
        forAll arbitrary $ \(TreeWrapper t2) ->
            forAll arbitrary $ \(TreeWrapper t3) ->
                ((t1 <> t2) <> t3) === (t1 <> (t2 <> t3))

prop_map_applies :: Property
prop_map_applies =
    forAll arbitrary $ \(TreeWrapper tree) ->
        let f = map toUpper
            mappedTree = map' f tree
            originalWords = toList tree
            expectedWords = map f originalWords
         in sort (toList mappedTree) === sort expectedWords

prop_startWith_prefix :: Property
prop_startWith_prefix =
    forAll genPrefix $ \prefix ->
        forAll arbitrary $ \(TreeWrapper tree) ->
            let wordsWithPrefix = startWith prefix tree
             in all (isPrefixOf prefix) wordsWithPrefix

prop_self_containment :: Property
prop_self_containment =
    forAll arbitrary $ \(TreeWrapper tree) ->
        let words = toList tree
            newTree = fromList words
         in tree === newTree

prop_remove_all :: Property
prop_remove_all =
    forAll genWordList $ \words ->
        let tree = fromList words
            emptied = foldr remove tree words
         in toList emptied === []

prop_monoid_left_identity :: Property
prop_monoid_left_identity =
    forAll arbitrary $ \(TreeWrapper tree) ->
        emptyTree <> tree === tree

prop_monoid_right_identity :: Property
prop_monoid_right_identity =
    forAll arbitrary $ \(TreeWrapper tree) ->
        tree <> emptyTree === tree

prop_equality_reflexive :: Property
prop_equality_reflexive =
    forAll arbitrary $ \(TreeWrapper tree) ->
        tree === tree

prop_equality_symmetric :: Property
prop_equality_symmetric =
    forAll arbitrary $ \(TreeWrapper t1) ->
        forAll arbitrary $ \(TreeWrapper t2) ->
            (t1 == t2) === (t2 == t1)

prop_insert_idempotent :: Property
prop_insert_idempotent =
    forAll genWord $ \word ->
        forAll arbitrary $ \(TreeWrapper tree) ->
            let treeOnce = insert word tree
                treeTwice = insert word treeOnce
             in treeOnce === treeTwice

prop_insert_remove_cycle :: Property
prop_insert_remove_cycle =
    forAll genWord $ \w1 ->
        forAll genWord $ \w2 ->
            forAll arbitrary $ \(TreeWrapper tree) ->
                w1 /= w2 ==>
                    let tree1 = insert w1 tree
                        tree2 = insert w2 tree1
                        tree3 = remove w1 tree2
                        tree4 = insert w1 tree3
                     in sort (toList tree4) === sort (nub (w1 : w2 : toList tree))

prop_toUpperCase_applied :: Property
prop_toUpperCase_applied =
    forAll arbitrary $ \(TreeWrapper tree) ->
        let upperTree = map' toUpperCase tree
            originalWords = toList tree
            expectedWords = map (map toUpper) originalWords
         in sort (toList upperTree) === sort expectedWords

allProps :: [(String, Property)]
allProps =
    [ ("insert_empty", prop_insert_empty)
    , ("from_to_inverse", prop_from_to_inverse)
    , ("union_correct", prop_union_correct)
    , ("empty_neutral", prop_empty_neutral)
    , ("union_associative", prop_union_associative)
    , ("map_applies", prop_map_applies)
    , ("startWith_prefix", prop_startWith_prefix)
    , ("self_containment", prop_self_containment)
    , ("remove_all", prop_remove_all)
    , ("monoid_left_identity", prop_monoid_left_identity)
    , ("monoid_right_identity", prop_monoid_right_identity)
    , ("equality_reflexive", prop_equality_reflexive)
    , ("equality_symmetric", prop_equality_symmetric)
    , ("insert_idempotent", prop_insert_idempotent)
    , ("insert_remove_cycle", prop_insert_remove_cycle)
    , ("toUpperCase_applied", prop_toUpperCase_applied)
    ]

propertyBasedTests :: IO ()
propertyBasedTests = do
    putStrLn "\nStarting property-based tests..."
    let runTest (name, prop) = do
            putStrLn $ "test " ++ name
            result <- quickCheckResult prop
            case result of
                Success{} -> putStrLn " Success"
                _ -> error " Failed"
            putStrLn ""
    mapM_ runTest allProps
    putStrLn "All property-based tests passed\n"

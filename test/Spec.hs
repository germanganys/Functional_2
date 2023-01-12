{-# Language ScopedTypeVariables, GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main)   where

import MyGraph (Graph (Graph), nodes, edges, getEmpty, addEdge, addNode, deleteEdge, deleteNode, depthFirstSearch, breadthFirstSearch, existsNode, existsEdge)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

simpleGraphOne :: Graph Int
simpleGraphOne = Graph (
    [
        (1, ["coin"]),
        (2, ["coin"])
    ],
    [(1, 2)])

simpleGraphTwo :: Graph Int
simpleGraphTwo = Graph (
    [
        (1, ["enemy"]),
        (3, ["heal"])
    ],
    [(2, 3), (3, 1)])

allGraphs :: [Graph Int]
allGraphs = [simpleGraphOne, simpleGraphTwo]

mergedGraphs :: Graph Int
mergedGraphs = Graph (
    [
        (1, ["coin", "enemy"]),
        (2, ["coin"]),
        (3, ["heal"])
    ],
    [(1, 2), (2, 3), (3, 1)])

emptyGraph :: Graph Int
emptyGraph = getEmpty

--check insertions 
checkInsertsNode :: TestTree
checkInsertsNode = testCase "test: Insert node in empty graph"  $ assertEqual [] True
    (existsNode 1 $ addNode emptyGraph (1, []))

graphTwoVZeroE :: Graph Int
graphTwoVZeroE = Graph (
    [
        (1, []),
        (2, [])
    ],[])

checkInsertsEdge :: TestTree
checkInsertsEdge = testCase "test: Insert edge in graph of two vertices"  $ assertEqual [] True
    (existsEdge (1, 2) $ addEdge graphTwoVZeroE $ (1, 2))

checkInsertsEdgeIntegrityChecked :: TestTree
checkInsertsEdgeIntegrityChecked = testCase
    "test: Insert edge in graph of two vertices, but edge connects wrong vertices, so edge must be skipped"
    $ assertEqual [] False
    (existsEdge (1, 1000) $ addEdge graphTwoVZeroE $ (1, 1000))

--check erase
checkEraseNode :: TestTree
checkEraseNode = testCase "test: cannot find node after it's erasing" $ assertEqual [] False
    (existsNode 1 $ deleteNode 1 $ addNode emptyGraph (1, []))

checkEraseEdge :: TestTree
checkEraseEdge = testCase "test: cannot find edge after it's erasing" $ assertEqual [] False
    (existsEdge (1, 2) $ deleteEdge (1, 2) $ addEdge graphTwoVZeroE $ (1, 2))

checkBFS :: TestTree
checkBFS = testCase "test: BFS" $ assertEqual [] True
    ((breadthFirstSearch 2 mergedGraphs) == [(2, ["coin"]), (3, ["heal"]), (1, ["coin", "enemy"])])

likeTree :: Graph Int
likeTree = Graph (
    [
        (1, []),
        (2, []),
        (3, []),
        (4, [])
    ],
    [(1, 2), (1, 3), (3, 4)])


checkDFS :: TestTree
checkDFS = testCase "test: DFS" $ assertEqual [] True
    ((breadthFirstSearch 1 likeTree) == [(1, []), (2, []), (3, []), (4, [])])

--check folds graph
checkFold :: TestTree
checkFold = testCase "test: check Foldable" $ assertEqual [] 3
    (foldl (+) 0 simpleGraphOne)

--check merge
checkMonoid :: TestTree
checkMonoid = testCase "test: check Semigroup and Monoid" $ assertEqual [] mergedGraphs
    (foldl (<>) mempty allGraphs)

--check Functor
checkFunctor :: TestTree
checkFunctor = testCase "test: check Functor" $ assertEqual [] True
    (((*3) <$> simpleGraphOne) == Graph (
    [
        (3, ["coin"]),
        (6, ["coin"])
    ],
    [(3, 6)]))


-- check Graph property
instance Arbitrary (Graph Int) where
    arbitrary = sized $ \n ->
        if n == 0
            then
                return (Graph([], []))
            else do
                let v = [(ni, []) | ni <- [1..n]]
                    e = [(ni, ni + 1) | ni <- [1..(n - 1)]] ++ [(n, 1)]
                return (Graph (v, e))


qcTestInsertNode :: TestTree
qcTestInsertNode = QC.testProperty "property based test: insert node => you can find it" $
  \(n :: Int, g :: Graph Int) -> existsNode n $ addNode g (n, [])

qcTestInsertEdge :: TestTree
qcTestInsertEdge = QC.testProperty "property based test: insert edge => you can find it" $
  \(g :: Graph Int) -> let n = length $ nodes g
                           e = (1, 1 + (n - 1))
                       in if
                          | n > 0 -> existsEdge e $ addEdge g $ e
                          | otherwise -> True


qcTestEraseNode :: TestTree
qcTestEraseNode = QC.testProperty "property based test: erase node => you can not find it" $
  \(g :: Graph Int) ->
  if edges g /= [] then
     not $ existsEdge (head $ edges g) (deleteEdge (head $ edges g) g)
  else True

qcTestEraseEdge :: TestTree
qcTestEraseEdge = QC.testProperty "property based test: erase edge => you can not find it" $
  \(g :: Graph Int) -> let n = length $ nodes g
                           e = head $ edges g
                        in if
                           | n > 0 -> not $ existsEdge e (deleteEdge e g)
                           | otherwise -> True


qcTestBfsDfs :: TestTree
qcTestBfsDfs = QC.testProperty "property based test: bfs, dfs in cycle" $
  \(g :: Graph Int) ->
        let n = length $ nodes g
            start_v = 1
        in if
           | n > 0 -> (breadthFirstSearch start_v g) == nodes g && (depthFirstSearch start_v g) == nodes g
           | otherwise -> (breadthFirstSearch start_v g) == [] && (depthFirstSearch start_v g) == []



qcTestBinaryAssociativeOperation :: TestTree
qcTestBinaryAssociativeOperation = QC.testProperty "property based test: monoid" $
 \(t :: Graph Int, k :: Graph Int) ->
    ((k <> t) <> simpleGraphOne) == (k <> (t <> simpleGraphOne)) &&
    (k <> mempty == k) &&
    (mempty <> k == k)


qcTestFmap :: TestTree
qcTestFmap = QC.testProperty "property based test: Functor" $ \(g :: Graph Int, m :: Int, n :: Int) ->
  ((sum $ (*m) <$> g) == ((sum g) * m)) &&
  ((id <$> g) == g) &&
  ((((*m) . (+n)) <$> g) == ((*m) <$> ((+n) <$> g)))



tests :: TestTree
tests = testGroup "Unit tests: Bidirectional Graph"
        [ 
            testGroup "Test Insertion" [checkInsertsNode, checkInsertsEdge, checkInsertsEdgeIntegrityChecked],
            testGroup "Test Erasing" [checkEraseNode, checkEraseEdge],
            testGroup "Test Traverse" [checkBFS, checkDFS],
            testGroup "Test Folds, Monoid, Functor" [checkFold, checkMonoid, checkFunctor],
            testGroup "property based test: Inserting" [qcTestInsertNode, qcTestInsertEdge],
            testGroup "property based test: Erasing" [qcTestEraseNode, qcTestEraseEdge],
            testGroup "property based test: Traverse" [qcTestBfsDfs],
            testGroup "property based test: Monoid property" [qcTestBinaryAssociativeOperation],
            testGroup "property based test: Functor property" [qcTestFmap]
        ]

main :: IO()
main = defaultMain tests

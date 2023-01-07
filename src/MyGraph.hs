module MyGraph (Graph (Graph), Node, Edge, nodes, edges, getEmpty, addEdge, addNode, deleteEdge, deleteNode, adjacents, depthFirstSearch, breadthFirstSearch, existsNode, existsEdge) where

import Data.List
import Data.Function

type Edge t = (t, t)
type Node t = (t, [String])
data Graph t = 
    Graph ([Node t], [Edge t]) deriving (Eq, Show)

-- Get the vertex of a given node
vertex :: Node t -> t
vertex x = fst x

-- Get the data of a given node
getData :: Node t -> [String]
getData x = snd x

-- All nodes of the graph
nodes :: Graph t -> [Node t]
nodes (Graph g) = fst g

-- All edges of the graph
edges :: Graph t -> [Edge t]
edges (Graph g) = snd g

-- All vertices of the graph
vertices :: Graph t -> [t]
vertices g = map vertex (nodes g)


-- Get a node given its vertex id
node :: Eq t => t -> Graph t -> Node t
node v g
  | v == vertex (head (nodes g)) = head (nodes g)
  | otherwise = node v (Graph (tail (nodes g), edges g))

-- List of nodes from a list of vertices
verticesToNodes :: Eq t => [t] -> Graph t -> [Node t]
verticesToNodes [] _ = []
verticesToNodes (v : vs) g = node v g : verticesToNodes vs g

getEmpty = Graph ([], [])

isEmpty :: Eq t => Graph t -> Bool
isEmpty g = g == Graph([], [])

existsNode :: Eq t => t -> Graph t -> Bool
existsNode _ (Graph([], [])) = False
existsNode v g = any (\x -> x == v) (vertices g)

-- Check if an edge between two nodes exists
existsEdge :: Eq t => Edge t -> Graph t -> Bool
existsEdge (u, v) g = any (\(x, y) -> x == u  && y == v) (edges g)

-- Add a node to the graph if its vertex doesn't exist
addNode :: Eq t => Graph t -> Node t -> Graph t
addNode g n
  | existsNode (vertex n) g = g
  | otherwise = Graph (n:(nodes g), edges g)

-- Add a new edge to the graph only if both nodes exist and if the edge already doesn't exist
addEdge :: Eq t => Graph t -> Edge t -> Graph t
addEdge g (u, v)
  | (existsNode u g) && (existsNode v g) && (not(existsEdge (u, v) g)) = Graph (nodes g, (u, v):(edges g))
  | otherwise = g


-- Delete a node only if it is not connected to any node
deleteNode :: Eq t => t -> Graph t -> Graph t
deleteNode v g
  | (existsNode v g) && not (any (\(x, y) -> x == v || y == v) (edges g)) = Graph (deleteN v (nodes g), edges g)
  | otherwise = g
  where deleteN u xs = if (vertex (head xs)) == u
                       then tail xs
                       else head xs : deleteN u (tail xs)
        deleteN u [] = []

-- Delete an edge if exists
deleteEdge :: Eq t => Edge t -> Graph t -> Graph t
deleteEdge (u, v) g
  | existsNode u g && existsNode v g && existsEdge(u, v) g = Graph (nodes g, deleteA u v (edges g))
  | otherwise = g
  where deleteA z k xs = if ((fst (head xs) == z) && (snd (head xs) == k))
                         then tail xs
                         else head xs : deleteA z k (tail xs)
        deleteA z k [] = []



-- List of adjacent nodes of a given vertex
adjacents :: Eq t => t -> Graph t -> [Node t]
adjacents v g
  | existsNode v g = verticesToNodes (adjacentNodes v (edges g)) g
  | otherwise = []
  where adjacentNodes v [] = []
        adjacentNodes v (x : xs)
          | fst x == v = snd x : adjacentNodes v xs
          | otherwise = adjacentNodes v xs


-- Depth First Search
depthFirstSearch :: Eq t => t -> Graph t -> [Node t]
depthFirstSearch v g
  | existsNode v g = if (null (adjacents v g)) then [node v g] else dfs [v] [node v g] g
  | otherwise = []
    where dfs _ [] _ = []
          dfs vs (n:ns) g = n : dfs (vs ++ [vertex n]) (inStackNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g
            where inStackNodes v ns [] xs = xs
                  inStackNodes v ns vs xs = (filter (\x-> (not(any (\y -> x == y || v == (vertex x)) xs))) (filter (\z -> not(elem (vertex z) ns)) vs)) ++ xs

-- Breadth First Search
breadthFirstSearch :: Eq t => t -> Graph t -> [Node t]
breadthFirstSearch v g
  | existsNode v g = if (null(adjacents v g)) then [node v g] else bfs [v] [node v g] g
  | otherwise = []
    where bfs _ [] _ = []
          bfs vs (n:ns) g = n : bfs (vs ++ [vertex n]) (inQueueNodes (vertex n) (vs ++ [vertex n]) (adjacents (vertex n) g) ns) g
            where inQueueNodes v ns [] xs = xs
                  inQueueNodes v ns vs xs = xs ++ (filter (\x -> (not (any (\y->x == y || v == (vertex x)) xs))) (filter (\z-> not (elem (vertex z) ns)) vs))


mapHelper :: Foldable t1 =>
                   (t2 -> a1) -> (t2, t1 [a2]) -> (a1, [a2])
mapHelper f (a, b) = (f a, concat b)

node_grouper :: (Ord b, Ord a) => [(b, [a])] -> [(b, [a])]
node_grouper x = map (mapHelper head) $ map unzip $ groupBy ((==) `on` fst) $ sort x

instance (Ord t) => Semigroup (Graph t) where
    g1 <> g2 = Graph (node_grouper $ (nodes g1 ++ nodes g2), nub (edges g1 ++ edges g2))

instance (Ord t) => Monoid (Graph t) where
    mempty = Graph ([], [])
    mappend = (<>)

-- provide Functor type functionality (use fmap)
instance Functor Graph where
    fmap _ (Graph ([], [])) = Graph ([], [])
    fmap func g = Graph ([(func v, dat) | (v, dat) <- nodes g],  [(func v1, func v2) | (v1, v2) <- edges g])


instance Foldable Graph where
    foldMap _ (Graph ([], _)) = mempty
    foldMap f (Graph (h:n, _)) = (f (vertex h)) `mappend` (foldMap f (Graph (n, [])))

-- Paths in graph. The program reads graph from the file (directed or not, with weights in edges)
-- and finds the path between every pair of verticles with the lowest weight.
--
-- Structure of an input file:
-- 1 -- 1 - directed, 0 not
-- x -- label of the start vertex
-- x y 4 -- edges in format: labelFrom labelTo edgeWeight, weight's type is double.

import System.IO

main = do
  fileName <- getLine
  fileHandle <- openFile fileName ReadMode
  --fileHandle <- openFile "graph1.txt" ReadMode

  fileContents <- hGetContents fileHandle
  let fileLines = lines fileContents

  let isDirected = fileLines !! 0 !! 0
  let secondLine = splitString delimiter (fileLines !! 1)
  let startLabel = secondLine !! 0


  let edgeLines = drop 2 fileLines

  ------------------
  -- Bellman-Ford --
  ------------------
  let edges = initializeEdges isDirected edgeLines
  -- list of vertices, starting with weight 0, others with Infinite weight
  let vertices = initializeVerticesFromLabels startLabel $ distinct $ getVertexLabelsFromEdges edges
  -- create graph object
  let graph = initializeGraph vertices edges
  -- relax edges
  let relaxedGraph = relaxGraph graph (length vertices - 1)
  -- check for negative-cycles
  let checkedGraph = graphHasNegativeCycles relaxedGraph

  ------------------
  --    Output    --
  ------------------
  putStrLn $ graphToString checkedGraph

  -- preety print (as tree)
  let tree = graphToTree checkedGraph startLabel
  putStrLn $ drawTree tree

  -- print shortest path from start label to end label if exists
  if (length secondLine) == 2 then
    putStrLn $ graphGetPathString checkedGraph startLabel (secondLine !! 1)
  else
    putStrLn "Last vertex was not given"

-- Types
type Graphs = [Graph]
type Edges = [Edge]
type Vertices = [Vertex]

type WeightType = Double
type DistanceType = WeightType
type Label = String
type Labels = [Label]

-- Datas
data Graph = GraphNil | Graph Vertices Edges
  deriving (Show)

data Vertex = VertexNil | Vertex Label Distance Label
  deriving (Show)

data Edge = EdgeNil | Edge Label Label WeightType
  deriving (Show)

data Distance = DistanceInf | Distance DistanceType
  deriving ( Eq )


-- Instances
instance Num Graph where
  (+) (Graph vertices1 edges1) (Graph vertices2 edges2) = (Graph vertices2 (edges1++edges2))
  (-) g1 g2 = error "Operation not supported"
  (*) g1 g2 = error "Operation not supported"
  abs g = error "Operation not supported"
  signum g = error "Operation not supported"
  fromInteger g = error "Operation not supported"

instance Eq Vertex where
  (Vertex label1 distance1 visitor1) == (Vertex label2 distance2 visitor2) = label1 == label2

instance Eq Edge where
  (Edge vertexFrom1 vertexTo1 weight1) == (Edge vertexFrom2 vertexTo2 weight2) =
    vertexFrom1 == vertexFrom2 && vertexTo1 == vertexTo2

instance Num Distance where
  (+) DistanceInf DistanceInf = DistanceInf
  (+) (Distance val) DistanceInf = DistanceInf
  (+) DistanceInf (Distance val) = DistanceInf

  (+) (Distance val1) (Distance val2) = (Distance (val1+val2))
  (-) (Distance val1) (Distance val2) = (Distance (val1-val2))
  (*) (Distance val1) (Distance val2) = (Distance (val1*val2))
  abs (Distance val1) = if val1 < 0 then (Distance (-1*val1)) else (Distance val1)
  fromInteger int = (Distance (fromInteger int) )
  signum val1 = val1

instance Ord Distance where
  (<) (Distance _) DistanceInf = True
  (<) DistanceInf (Distance _)  = False
  (<) DistanceInf DistanceInf = False
  (<) (Distance val1) (Distance val2) = val1 < val2
  (<=) DistanceInf DistanceInf = True
  (<=) (Distance val1) (Distance val2) = val1 <= val2
  (>) DistanceInf DistanceInf = False
  max (Distance _) DistanceInf = DistanceInf
  min (Distance val1) DistanceInf = (Distance val1)

instance Show Distance where
  show (Distance distance) = show distance
  show DistanceInf = show "Infinite"

-- Graph methods
initializeEdges :: Char -> [String] -> Edges
initializeEdges _ [] = []
initializeEdges isDirected (x:xs) =
  if isDirected == '1' then edge:initializeEdges isDirected xs else edge:edge2:initializeEdges isDirected xs
    where
      edge       = (Edge labelFrom labelTo weight)
      edge2      = (Edge labelTo labelFrom weight)
      labelFrom  = splitString delimiter x !! 0
      labelTo    = splitString delimiter x !! 1
      weight     = read (splitString delimiter x !! 2) :: WeightType

initializeVerticesFromLabels :: Label -> Labels -> Vertices
initializeVerticesFromLabels _ [] = []
initializeVerticesFromLabels startLabel (label:labels) = vertex : initializeVerticesFromLabels startLabel labels
  where
    vertex = (Vertex label distance [])
    distance = if startLabel == label then (Distance 0) else DistanceInf

getVertexLabelsFromEdges :: Edges -> Labels
getVertexLabelsFromEdges [] = []
getVertexLabelsFromEdges ((Edge labelFrom labelTo weight):edges) = labelFrom:labelTo: getVertexLabelsFromEdges edges

getVertexLabelFromEdge :: Edge -> Label
getVertexLabelFromEdge (Edge vertexFromLabel vertexToLabel weight) = vertexFromLabel
getVertexLabelFromEdge _ = []

initializeGraph :: Vertices -> Edges -> Graph
initializeGraph vertices edges = (Graph vertices edges)

relaxGraph :: Graph -> Int -> Graph
relaxGraph graph n = if n > 0 then relaxGraph (relaxEdges graph) (n-1) else graph
--relaxGraph graph n = last $ take n $ iterate (relaxEdges) graph

relaxEdges :: Graph -> Graph
relaxEdges graph@(Graph vertices []) = graph
relaxEdges (Graph vertices ((edge@(Edge labelU labelV weight)):edges)) =
  if ((getVertexDistance vertices labelU) + (Distance weight)) < (getVertexDistance vertices labelV) then
    (Graph vertices [edge]) +
    (relaxEdges (Graph
      (setVertexDistance
        (setVertexPredecessor vertices labelV labelU)
        labelV
        ((getVertexDistance vertices labelU) + (Distance weight))
      )
      edges
    ))
  else
    (Graph vertices [edge]) + (relaxEdges (Graph vertices edges))


getVertexPredecessor :: Vertices -> Label -> Label
getVertexPredecessor [] _ = error "Path not found"
getVertexPredecessor ((Vertex label distance predecessor):vertices) searchLabel =
  if label == searchLabel then label else getVertexPredecessor vertices searchLabel

getVertexDistance :: Vertices -> Label -> Distance
getVertexDistance [] _ = error "Path not found"
getVertexDistance ((Vertex label distance predecessor):vertices) searchLabel =
  if label == searchLabel then distance else getVertexDistance vertices searchLabel

setVertexPredecessor :: Vertices -> Label -> Label -> Vertices
setVertexPredecessor ( (vertex@(Vertex label distance predecessor)):vertices) searchLabel newPredecessor =
  if label == searchLabel then
    (Vertex label distance newPredecessor) : vertices
  else
    vertex : setVertexPredecessor vertices searchLabel newPredecessor

setVertexDistance :: Vertices -> Label -> Distance -> Vertices
setVertexDistance ( (vertex@(Vertex label distance predecessor)):vertices) searchLabel newDistance =
  if label == searchLabel then
    (Vertex label newDistance predecessor) : vertices
  else
    vertex : setVertexDistance vertices searchLabel newDistance

graphHasNegativeCycles :: Graph -> Graph
graphHasNegativeCycles graph@(Graph vertices []) = graph
graphHasNegativeCycles (Graph vertices ((edge@(Edge labelFrom labelTo weight)):edges)) =
  if ((getVertexDistance vertices labelFrom) + (Distance weight)) < (getVertexDistance vertices labelTo) then
    error "Graph contains a negative-weight cycle"
  else
    (Graph vertices [edge]) + (graphHasNegativeCycles (Graph vertices edges))


-- Output methods
graphToString :: Graph -> String
graphToString (Graph vertices edges) = unlines $ verticesToStringList vertices

verticesToStringList :: Vertices -> [String]
verticesToStringList [] = []
verticesToStringList ((Vertex label distance visitor):vertices) =
  [visitor ++ " --> " ++ label ++ "  (" ++ (show distance) ++ ")"]
  ++ verticesToStringList vertices

graphToTree :: Graph -> Label -> Tree String
graphToTree (Graph vertices edges) startLabel = buildTree vertices startLabel

buildTree :: Vertices -> Label -> Tree String
buildTree vertices label = (Node label (map (\x -> buildTree vertices x) (vertexGetSuccessors vertices label)))

vertexGetSuccessors :: Vertices -> Label -> Labels
vertexGetSuccessors [] _ = []
vertexGetSuccessors ((Vertex label distance visitor):vertices) searchLabel =
  if visitor == searchLabel then
    label:vertexGetSuccessors vertices searchLabel
  else
    vertexGetSuccessors vertices searchLabel

graphGetPathString :: Graph -> Label -> Label -> String
graphGetPathString graph startLabel endLabel = concat $
  [startLabel] ++ ( map (\x -> " -> " ++ x) (reverse $ graphGetPath graph startLabel endLabel))

graphGetPath :: Graph -> Label -> Label -> Labels
graphGetPath graph@(Graph vertices edges) startLabel endLabel =
  if startLabel /= endLabel then
    endLabel : (graphGetPath graph startLabel (verticesGetPredecessor vertices endLabel))
  else
    []

verticesGetPredecessor :: Vertices -> Label -> Label
verticesGetPredecessor [] _ = error "Path not found"
verticesGetPredecessor ((Vertex label distance predecessor):vertices) searchLabel =
  if label == searchLabel then
    predecessor
  else
    verticesGetPredecessor vertices searchLabel

data Tree a = Node {rootLabel :: a, subForest :: Forest a}
  deriving (Eq, Read, Show)

type Forest a = [Tree a]

drawTree :: Tree String -> String
drawTree  = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] = "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) = "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)


-- HELPERS
distinct :: (Eq a) => [a] -> [a]
distinct [] = []
distinct (x:xs) = if elem x xs then distinct xs else x:distinct xs

delimiter :: Char
delimiter = ' '

splitString :: Char -> String -> [String]
splitString delimiter string = wordsWhen (==delimiter) string

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

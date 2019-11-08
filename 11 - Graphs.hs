--graphs

--graphs using inxdexed collections of nodes and edges
type Vertex = Int
type Edge = (Vertex, Vertex)
type Table a = Array Vertex a
type Graph = Table [Vertex]

--graphs using cyclic dependencies
data List a = Empty | Node a (List a) (List a)

makeList :: [a] -> List a -> List a
makeList [] previous = Empty
makeList (x:xs) previous =
    let current = Node x (previous) (makeList xs current)
    in current

mkList :: [a] -> List a
mkList xs = makeList xs Empty

data Graph a = GNode a (Graph a)
makeGraph :: [(a,Int)] -> Graph a
makeGraph xys = table !! 0
    where table = map (\(x,ns) -> GNode x (map (table !!) ns)) xys

--inductive graphs
empty :: Graph a b
embed :: Context a b -> Graph a b -> Graph a b

type Adj b = [(b,Node)]
type Context a b = (Adj b, Node, a, Adj b)

match :: Node -> Graph a b -> Decomp a b
type Decomp a b -> (Maybe (Context a b), Graph a b)

gmap :: (Context a b) -> (Context c b) -> Graph a b -> Graph c b
gmap f g 
    | isEmpty g = empty
    | otherwise = embed (f c) (gmap f g’)
    where (c,g’) = matchAny g 


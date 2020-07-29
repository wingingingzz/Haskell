-- 1. Create a data type to represent fruits
data Fruit = Apple | Pear | Orange | Plum | Banana deriving (Show, Eq)

-- 1. Create a list of fruit
f1 :: [Fruit]
f1 = [Pear, Apple, Plum]
f2 :: [Fruit]
f2 = [Pear, Plum, Pear, Apple]

-- 2. Create a data type to express three transformations: remove, duplicate and exchange
data Tran = Id | Rm | Dup | Exc deriving Show

-- 2. Create the list of four operations
t1 :: [(Tran, Int)]
t1 = [(Rm,2), (Dup,0), (Id,0), (Exc,1)]

-- 3. function: "applyTran" that implements each of the transformations in lists of fruit
-- Input: an operation/transaction, a list of Fruit
-- Output: a list of Fruit
applyTran :: (Tran,Int) -> [Fruit] -> [Fruit]
applyTran (Id,_) xs = xs
applyTran (Rm,n) xs = (take n xs) ++ (drop (n+1) xs)
applyTran (Dup,n) xs = (take (n+1) xs) ++ [(!!) xs n] ++ (drop (n+1) xs)
applyTran (Exc,n) xs = (take n xs) ++ [(!!) xs (n+1)] ++ [(!!) xs n] ++ (drop (n+2) xs)

-- 4. function: "applySeries" 
-- Input: a sequence (list) of transactions, a list of fruit
-- Output: a list of fruit
applySeries :: [(Tran, Int)] -> [Fruit] -> [Fruit]
applySeries [] xs = xs
applySeries (tn:trans) xs = applySeries trans (applyTran tn xs)

-- 5. function: "applyPar" that applies a list of transactions in parallel to a list of fruit
-- Input: a sequence (list) of transactions, a list of fruit
-- Output: a list of fruit
applyPar :: [(Tran, Int)] -> [Fruit] -> [[Fruit]]
applyPar [] xs = []
-- applyPar ((Id,_):trans) xs = (applyPar trans xs) -- requirement is changed
applyPar (tn:trans) xs = (applyTran tn xs) : (applyPar trans xs)

-- 6. function: "transSeq"
-- Input: a type of transaction t of type Tran, an integer n
-- Output: the list of transaction pairs, [(t,0), … , (t,n)]
-- Requirement: use list comprehension
transSeq :: Tran -> Int -> [(Tran, Int)]
-- method without list comprehension (pattern matching):
-- transSeq t 0 = [(t,0)]
-- transSeq t n = transSeq t (n-1) ++ [(t,n)]
transSeq t n = [(t, x) | x <- [0..n]] -- method with list comprehension

-- 7. function: "transSeqAll"
-- Input: an integer n
-- Output: the list of all possible operations, except Id
transSeqAll :: Int -> [(Tran,Int)]
transSeqAll n = (transSeq Rm n) ++ (transSeq Dup n) ++ (transSeq Exc (n-1))

-- 8. function: "initTree" to implement initial tree which has only root node
-- Input: a list of fruit
-- Output: initial tree
initTree :: [Fruit] -> (Int, [(Int, (Tran,Int), [Fruit])])
initTree xs = (0, [(-1, (Id,0), xs)])

-- 9. function: "nodeParent", "nodeTran", "nodeOutcome" 
-- Input: a 3-tuple node
-- Output: each of the three components of the node
nodeParent :: (Int, (Tran,Int), [Fruit]) -> Int
nodeParent (parent,_,_) = parent
nodeTran :: (Int, (Tran,Int), [Fruit]) -> (Tran,Int)
nodeTran (_,trans,_) = trans
nodeOutcome :: (Int, (Tran,Int), [Fruit]) -> [Fruit]
nodeOutcome (_,_,xs) = xs

-- 10. auxiliary function: "nodeTuple" to create a standard 3-tuple node for a child node
-- Input: parent node, list of transactions that is available for parent node, 
--        correspongding list of fruit lists that is generated after applying list of transactions to fruit list of parent node
-- Output: list of child nodes
nodeTuple :: Int -> [(Tran, Int)] -> [[Fruit]] -> [(Int, (Tran, Int), [Fruit])]
nodeTuple pos [] _ = []
nodeTuple pos (tn:trans) (fs:fss) = (pos,tn,fs) : (nodeTuple pos trans fss)

-- 10. auxiliary function: "treeNodeToExp" use position index in tree to retreive a node to be expanded
-- Input: a (pos,tree) pair
-- Output: a node to be expanded (a parent node)
treeNodeToExp :: (Int, [(Int, (Tran,Int), [Fruit])]) -> (Int, (Tran,Int), [Fruit])
treeNodeToExp (pos,ns) = (!!) ns pos

-- 10. auxiliary function: "childNodes" to expand the node and create a list of its child nodes
-- Input: a (pos,tree) pair
-- Output: a list of child nodes of parent node that is selected by "pos" in tree
childNodes :: (Int, [(Int, (Tran,Int), [Fruit])]) -> [(Int, (Tran, Int), [Fruit])]
childNodes (pos,ns) = nodeTuple pos (transSeqAll (length (nodeOutcome (treeNodeToExp (pos,ns))) - 1)) (applyPar (transSeqAll (length (nodeOutcome (treeNodeToExp (pos,ns))) - 1)) (nodeOutcome (treeNodeToExp (pos,ns))))

-- 10. function: "processCurrent" 
-- Input: a (pos,tree1) pair
-- Output: an updated (pos+1, tree2) pair, with the additional nodes 
--      when all possible transformations from the node pos are made added to the end of tree1 to form tree2
processCurrent :: (Int, [(Int, (Tran,Int), [Fruit])]) -> (Int, [(Int, (Tran,Int), [Fruit])])
processCurrent (pos,ns) = (pos + 1, ns ++ (childNodes (pos,ns)))

-- 11. auxiliary function: "findNodeIndex"
-- Input: a counter to count the index of node, a fruit list, list of nodes in tree
-- Output: the index of node in which fruit list is the same as given list
findNodeIndex :: Int -> [Fruit] -> [(Int, (Tran,Int), [Fruit])] -> Int
findNodeIndex count fs [] = -1
findNodeIndex count fs (n:ns) | fs == nodeOutcome(n) = count
                              | otherwise = findNodeIndex (count + 1) fs ns

-- 11. function: "findFruits"
-- Input: a counter to count the index of node, a fruit list, list of nodes in tree
-- Output: the index of node in which fruit list is the same as given list
findFruits :: [Fruit] -> [(Int, (Tran,Int), [Fruit])] -> Int
findFruits fs ns = findNodeIndex 0 fs ns

-- 12. function: "processRepeat", to repeatedly execute processCurrent and expanding the tree 
--      until either f2 is found amongst the nodes, as the tree expands, 
--      or the size of the tree (measured as number of nodes) exceeds maxiter.
-- Input: an integer maxiter, a list of fruit, a (pos,tree) pair
-- Output: (f2_pos, (pos’,tree’))
--      where (pos’,tree’) is the output of the last execution of processCurrent 
--      and f2_pos is the position at which f2 was found. 
--      If f2 was not found then output with f2_pos=-1.
processRepeat :: Int -> [Fruit] -> (Int, [(Int, (Tran,Int), [Fruit])]) -> (Int, (Int, [(Int, (Tran,Int), [Fruit])]))
processRepeat maxiter fs (pos, ns) | ((length ns > maxiter) || (findFruits fs ns >= 0)) = (findFruits fs ns, (pos, ns))
                                   | otherwise = processRepeat maxiter fs (processCurrent (pos, ns))

-- 13. function: "findTransFrom", to solve the task of moving from list of fruits f1 to f2 in the shortest number of operations
-- Input: an integer maxiter, an initial fruit list, a target fruit list
-- Output: the pair (f2_pos, (pos’,tree’))
findTransFrom :: Int -> [Fruit] -> [Fruit] -> (Int, (Int, [(Int, (Tran,Int), [Fruit])]))
findTransFrom maxiter f1 f2 = processRepeat maxiter f2 (initTree f1)

-- 14. function: "transPath", shows the path of operations and list of fruit at each stage of the solution 
--      (if one has been found).
-- Input: (pos, (pos, tree))
-- Output: list of pairs of transaction and fruit list.
transPath :: (Int, (Int, [(Int, (Tran,Int), [Fruit])])) -> [((Tran,Int), [Fruit])]
transPath search1 = reverse (transPath2 (fst search1) (snd (snd search1)) )
    where
        transPath2 pos stree1
            | pos == -1 = []
            | otherwise = (nodeTran n1, nodeOutcome n1) : (transPath2 (nodeParent n1) stree1)
            where
                n1 = stree1!!pos

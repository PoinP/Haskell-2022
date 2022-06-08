data BTree = Nil | Node Int BTree BTree 

singleCousin :: BTree -> [Int]

tree :: BTree
tree = Node 1 (Node 2 (Node 4 (Node 8 Nil Nil) (Node 9 Nil Nil)) (Node 5 Nil (Node 10 Nil Nil))) (Node 3 (Node 6 Nil (Node 11 Nil Nil)) (Node 7 Nil (Node 12 Nil Nil)))
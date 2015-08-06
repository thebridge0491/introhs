module CollectionsCase (tcases) where

{-
import qualified System.IO as IO
import qualified Text.Printf as Printf
import qualified Control.Exception as Exception
-}
import qualified Data.Maybe as Maybe
import Data.List

import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import qualified Data.HashMap.Lazy as H
import qualified Data.Dequeue as D
import qualified Data.Concurrent.Deque.Reference as DequeRef
import qualified Data.Concurrent.Deque.Class as DequeCls
import qualified Data.PQueue.Min as MinHeap

import Test.Framework.Providers.HUnit
import Test.HUnit

--import qualified Data.Introhs.Util as Util
{-
withFixture1 :: String -> IO a -> IO ()
withFixture1 nm f = Exception.bracket setUp (\_ -> f) tearDown
  where
    setUp = Printf.hPrintf IO.stderr "(%s) SetUp ...\n" nm
    tearDown _ = Printf.hPrintf IO.stderr "... TearDown (%s)\n" nm
-}
--epsilon = 0.001
(lst, revlst) = ([0..4], reverse (enumFromTo 0 4))

caseLists = do
    assertBool "make-list" $ 1 : [] == [1]
    assertBool "equal" $ lst == [0, 1, 2, 3, 4]
    assertBool "inequal" $ [] /= [0, 1, 2]
    assertBool "empty" $ null []
    assertBool "hd" $ head [2, 1, 0] == 2
    assertBool "nth" $ [2, 1, 0] !! 2 == 0
    assertBool "length" $ length [2, 1, 0] == 3
    assertBool "append" $ [2, 1, 0] ++ [9, 9, 9] == [2, 1, 0, 9, 9, 9]
    assertBool "reverse" $ reverse [2, 1, 0] == [0, 1, 2]
    assertBool "mem" $ elem 1 [2, 1, 0]
    assertBool "any" $ any (\x -> 0 == mod x 2) [1, 4, 3]
    assertBool "all" $ all (\x -> 0 == mod x 2) [6, 4, 2]
    assertBool "filter" $ filter (\x -> 0 == mod x 2) lst == [0, 2, 4]
    assertBool "remove" $ snd (partition (\x -> 0 == mod x 2) lst) == [1, 3]
    assertBool "fold_left" $ (foldl (\a e -> a + e) 0 lst) == 10
    assertBool "fold_right" $ (foldr (\e a -> e - a) 0 lst) == 2
    assertBool "map" $ (map (\e -> e + 2) lst) == [2,3,4,5,6]
    mapM_ (\e -> putStr $ show e) lst
    assertBool "iter" $ True
    assertBool "to_vector" $ V.fromList [0, 1, 2] == V.enumFromTo 0 2
    assertBool "sort" $ sortBy (\a b -> compare b a) [3, 2, 4, 0, 1] == revlst

case1darrays = do
    assertBool "make-vector" $ let _ = V.fromList [0.0,1.0,2.0,3.0,4.0] 
        in True
    assertBool "equal" $ vec == V.generate 5 (\i -> fromIntegral i)
    assertBool "inequal" $ vec /= V.empty
    assertBool "empty" $ V.null V.empty
    assertBool "get" $ vec V.! 3 == 3.0
    assertBool "length" $ V.length vec == 5
    assertBool "append" $ V.fromList [0.0,1.0,2.0] V.++ 
        V.fromList [3.0,4.0] == vec
    assertBool "set" $ V.update vec (V.fromList [(2,2.5)]) == 
        V.fromList [0.0,1.0,2.5,3.0,4.0]
    assertBool "fold_left" $ V.foldl (\a e -> a - e) 0.0 vec == -10.0
    assertBool "map" $ V.imap (\_ e -> e + 2.0) vec == 
        V.fromList [2.0,3.0,4.0,5.0,6.0]
    assertBool "to_list" $ V.toList vec == [0.0, 1.0, 2.0, 3.0, 4.0]
  where vec = V.fromList [0.0, 1.0, 2.0, 3.0, 4.0]

case2darrays = do
    assertBool "make-array" $ let _ = A.array ((0,0), (2,1)) alst in True
    putStr $ unlines [unwords [show (arr2d A.! (y,x)) | x <- [0..1]] | 
        y <- [0..2]]
    assertBool "iter" $ True
    assertBool "set" $ arr2d A.// [((1,0),25)] == 
        A.listArray ((0,0), (2,1)) [0,1,25,3,4,5]
    putStr $ unlines [unwords [show (arr2d A.! (y,x)) | x <- [0..1]] | 
        y <- [0..2]]
    assertBool "iter" $ True
    assertBool "to_alist" $ A.assocs arr2d == alst
    assertBool "to_list" $ A.elems arr2d == [0,1,2,3,4,5]
  where arr2d = A.listArray ((0,0), (2,1)) [0,1,2,3,4,5]
        alst = [((0,0),0),((0,1),1),((1,0),2),((1,1),3),((2,0),4),((2,1),5)]

caseAlists = do
    assertBool "make-alist" $ let _ = zip ['a','b','c'] [0,1,2] in True
    assertBool "assoc" $ maybe (-1) id (lookup 'b' alst) == 1
    assertBool "mem_assoc" $ maybe ('z', -1) id
        (find (\(h,_) -> h == 'c') alst) == ('c', 2)
    assertBool "remove_assoc" $ snd (partition (\(h,_) -> h == 'a') alst) 
        == [('b', 1), ('c', 2)]
  where alst = [('a', 0), ('b', 1), ('c', 2)]

caseSets = do
    assertBool "make-set" $ let _ = S.empty in True
    assertBool "adjoin" $ S.insert 'i' (S.empty) == S.singleton 'i'
    assertBool "union" $ sort_res (S.union set1 set2) == 
        ['a','e','k','p','q','u','z']
    assertBool "intersection" $ sort_res (S.intersection set1 set2) == 
        ['p','u']
    assertBool "difference" $ sort_res (S.difference set1 set2) == 
        ['a','e','k']
    assertBool "xor" $ sort_res (xor_set set1 set2) == 
        ['a','e','k','q','z']
    assertBool "to_list" $ sort_res set1 == ['a','e','k','p','u']
  where lst_chars = ['k','p','a','e','u','k','a']
        (set1, set2) = (S.fromList lst_chars, S.fromList ['q','p','z','u'])
        sort_res setA = sortBy compare $ S.toList setA
        xor_set setA setB = S.difference set_union set_inter
          where set_union = S.union setA setB
                set_inter = S.intersection setA setB

caseMaps = do
    assertBool "make-map" $ let _ = M.empty in True
    assertBool "empty" $ M.null M.empty
    assertBool "add" $ (M.insert "ltr 20" 'Z' map1) == 
        (M.fromList $ zip ("ltr 20" : lst_strs) ('Z' : lst_chars))
    assertBool "find" $ M.findWithDefault '#' "ltr 02" map1 == 'b'
    assertBool "remove" $ M.toList (M.delete "ltr 02" map1) ==
        [("ltr 01", 'a'), ("ltr 03", 'c')]
    assertBool "length" $ M.size map1 == 3
    assertBool "to_alist" $ M.assocs map1 == zip lst_strs lst_chars
  where lst_chars = ['a', 'b', 'c']
        lst_strs = ["ltr 01", "ltr 02", "ltr 03"]
        map1 = M.fromList $ zip lst_strs lst_chars

caseHashtbls = do
    assertBool "make-hashtbl" $ let _ = H.empty in True
    assertBool "empty" $ H.null H.empty
    assertBool "length" $ H.size htbl1 == 3
    assertBool "replace" $ (H.insert "ltr 20" 'Z' htbl1) == 
        (H.fromList $ zip ("ltr 20" : lst_strs) ('Z' : lst_chars))
    assertBool "find" $ H.lookupDefault '#' "ltr 02" htbl1 == 'b'
    assertBool "mem" $ H.member "ltr 02" htbl1
    assertBool "remove" $ sortBy (\(ah,_) (bh,_) -> compare ah bh) 
        (H.toList (H.delete "ltr 02" htbl1)) ==
        [("ltr 01", 'a'), ("ltr 03", 'c')]
    assertBool "to_alist" $ sortBy (\(ah,_) (bh,_) -> compare ah bh) 
        (H.toList htbl1) == zip lst_strs lst_chars
  where lst_chars = ['a', 'b', 'c']
        lst_strs = ["ltr 01", "ltr 02", "ltr 03"]
        htbl1 = H.fromList $ zip lst_strs lst_chars :: H.HashMap String Char

caseDequeues1 = do
    assertBool "make-queue" $ let _ = D.empty :: D.BankersDequeue Float 
        in True
    assertBool "empty" $ D.null queue == False
    assertBool "peek" $ (Maybe.fromJust $ D.first queue) == 25.7
    assertBool "enqueue" $ let _ = D.pushBack queue (-5.0) in True
    assertBool "dequeue" $ fst (Maybe.fromJust (D.popFront queue)) == 25.7
  where lst_floats = [25.7, 0.1, 78.5, 52.3]
        queue = D.fromList lst_floats :: D.BankersDequeue Float

caseDequeues2 = do
    queue <- DequeCls.newQ :: IO (DequeRef.SimpleDeque Float)
    mapM_ (\e -> DequeCls.pushL queue e) lst_floats
    q <- DequeRef.newQ :: IO (DequeRef.SimpleDeque Float)
    assertBool "make-queue" True
    isNull <- DequeRef.nullQ q
    assertBool "empty" isNull
    isNull <- DequeCls.nullQ queue
    assertBool "enqueue" $ isNull == False
    x <- DequeCls.tryPopR queue
    assertBool "dequeue" $ Maybe.fromJust x == 25.7
  where lst_floats = [25.7, 0.1, 78.5, 52.3]

caseHeaps = do
    assertBool "make-heap" $ let _ = MinHeap.empty -- :: MinHeap.MinQueue Float
        in True
    assertBool "size" $ MinHeap.size minheap == 4
    assertBool "add" $ MinHeap.toList (MinHeap.insert (-5.0) minheap) ==
        [-5.0, 0.1, 25.7, 52.3, 78.5]
    assertBool "extract" $ let _ = MinHeap.deleteMin minheap in True
    assertBool "top" $ MinHeap.getMin minheap == Just 0.1
    assertBool "to_list" $ MinHeap.toList minheap == [0.1,25.7,52.3,78.5]
  where lst_floats = [25.7, 0.1, 78.5, 52.3]
        minheap = MinHeap.fromList lst_floats -- :: MinHeap.MinQueue Float


--tcases = map (\(nm, f) -> uncurry testCase $ (nm, withFixture1 nm f))
tcases = map (uncurry testCase)
    [("caseLists", caseLists), ("case1darrays", case1darrays)
    , ("case2darrays", case2darrays), ("caseAlists", caseAlists)
    , ("caseSets", caseSets), ("caseMaps", caseMaps)
    , ("caseHashtbls", caseHashtbls)
    , ("caseDequeues1", caseDequeues1), ("caseDequeues2", caseDequeues2)
    , ("caseHeaps", caseHeaps)
    ]

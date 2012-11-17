 A Sukodku solver by Chris Kuklewicz (haskell (at) list (dot) mightyreason (dot) com)
 The usual BSD license applies, copyright 2006.
 Uploaded to HaskellWiki as DancingSudoku.lhs

 I compile on a powerbook G4 (Mac OS X, ghc 6.4.2) using
 ghc -optc-O3 -funbox-strict-fields -O2 --make -fglasgow-exts

 This is a translation of Knuth's GDANCE from dance.w / dance.c

 http://www-cs-faculty.stanford.edu/~uno/preprints.html
 http://www-cs-faculty.stanford.edu/~uno/programs.html
 http://en.wikipedia.org/wiki/Dancing_Links

 I have an older verison that uses lazy ST to return the solutions on
 demand, which was more useful when trying to generate new puzzles to
 solve.

> module Main where

> import Prelude hiding (read)
> import Control.Monad
> import Control.Monad.Fix
> import Data.Array.IArray
> import Control.Monad.ST.Strict
> import Data.STRef.Strict
> import Data.Char(intToDigit,digitToInt)
> import Data.List(unfoldr,intersperse,inits)

> new = newSTRef
> {-# INLINE new #-}
> read = readSTRef
> {-# INLINE read #-}
> write = writeSTRef
> {-# INLINE write #-}
> modify = modifySTRef
> {-# INLINE modify #-}

 Data types to prevent mixing different index and value types

> type A = Int
> newtype R = R A deriving (Show,Read,Eq,Ord,Ix,Enum)
> newtype C = C A deriving (Show,Read,Eq,Ord,Ix,Enum)
> newtype V = V A deriving (Show,Read,Eq,Ord,Ix,Enum)
> newtype B = B A deriving (Show,Read,Eq,Ord,Ix,Enum)

 Sudoku also has block constraints, so we want to look up a block
 index in an array:

> lookupBlock :: Array (R,C) B
> lookupBlock = listArray bb [ toBlock ij | ij <- range bb ]
>     where ra :: Array Int B
>           ra = listArray (0,pred (rangeSize b)) [B (fst b) .. B (snd b)]
>           toBlock (R i,C j) = ra ! ( (div (index b j) 3)+3*(div (index b i) 3) )

 The values for an unknown location is 'u'.
 The bound and range are given by b and rng.  And bb is a 2D bound.

> u = V 0  -- unknown value
> b :: (Int,Int)
> b = (1,9) -- min and max bounds
> rng = enumFromTo (fst b)  (snd b)  -- list from '1' to '9'
> bb = ((R (fst b),C (fst b)),(R (snd b),C (snd b)))

  A Spec can be turned into a parsed array with ease:

> type Hint = ((R,C),V)
> newtype Spec = Spec [Hint] deriving (Eq,Show)

> type PA = Array (R,C) V

> parse :: Spec -> PA
> parse (Spec parsed) = let acc old new = new
>                       in accumArray acc u bb parsed

 The dancing links algorithm depends on a sparse 2D node structure.
 Each column represents a constraint.  Each row represents a Hint.
 The number of possible hints is 9x9x9 = 271

> type (MutInt st)  = (STRef st) Int

 The pointer types:

> type (NodePtr st) = (STRef st) (Node st)
> type (HeadPtr st)  = (STRef st) (Head st)

 The structures is a 2D grid of nodes, with Col's on the top of
 columns and a sparse collection of nodes.  Note that topNode of Head
 is not a strict field.  This is because the topNode needs to refer to
 the Head, and they are both created monadically.

> type HeadName = (Int,Int,Int) -- see below for meaning

> data Head st = Head {headName:: !HeadName
>                     ,topNode:: (Node st) -- header node for this column
>                     ,len:: !(MutInt st)  -- number of nodes below this head
>                     ,next,prev:: !(HeadPtr st)  -- doubly-linked list
>                     }

> data Node st = Node {getHint:: !Hint
>                     ,getHead:: !(Head st)  -- head for the column this node is in
>                     ,up,down,left,right :: !(NodePtr st)  -- two doubly-linked lists
>                     }

> instance Eq (Head st) where
>     a == b = headName a == headName b

> instance Eq (Node st) where
>     a == b = up a == up b

 To initialize the structures is a bit tedious.  Knuth's code reads in
 the problem description from a data file and builds the structure
 based on that.  Rather than short strings, I will use HeadName as the
 identifier.
 
 The columns are (0,4,5) for nodes that put some value in Row 4 Col 5
                 (1,2,3) for nodes that put Val 3 in Row 2 and some column
                 (2,7,4) for nodes that put Val 4 in Col 7 and some row
                 (3,1,8) for nodes that put Val 8 in some (row,column) in Block 1

 The first head is (0,0,0) which is the root.  The non-root head data
 will be put in an array with the HeadName as an index.

> headNames :: [HeadName]
> headNames = let names = [0,1,2,3] 
>             in (0,0,0):[ (l,i,j) | l<-names,i<-rng,j<-rng]

 A "row" of left-right linked nodes is a move.  It is defined by a
 list of head names.

> type Move = [(Hint,HeadName)]

 Initial hints are enforced by making them the only legal move for
 that location.  Blank entries with value 'u = V 0' have a move for
 all possible values [V 1..V 9].

> parseSpec :: Spec -> [Move]
> parseSpec spec =
>   let rowsFrom :: Hint -> [Move]
>       rowsFrom (rc@(R r,C c),mv@(V v')) = 
>           if mv == u then [ rsyms v | v <- rng ]
>           else [ rsyms v' ]
>         where (B b) = lookupBlock ! rc
>               rsyms :: A -> Move
>               rsyms v = map ( (,) (rc,V v) ) [(0,r,c),(1,r,v),(2,c,v),(3,b,v)]
>   in concatMap rowsFrom (assocs (parse spec))

 mkDList creates doubly linked lists using a monadic smart
 constructor and the recursive "mdo" notation as documented at
 http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#mdo-notation
 http://www.cse.ogi.edu/PacSoft/projects/rmb/

 For more fun with this, see the wiki page at
 http://haskell.org/hawiki/TyingTheKnot

> mkDList :: (MonadFix m) => (b -> a -> b -> m b) -> [a] -> m b
> mkDList _ [] = error "must have at least one element"
> mkDList mkNode xs = mdo (first,last) <- go last xs first
>                         return first
>   where go prev []     next = return (next,prev)
>         go prev (x:xs) next = mdo this <- mkNode prev x rest
>                                   (rest,last) <- go this xs next
>                                   return (this,last)

 toSimple takes a function and a header node and iterates (read . function)
 until the header is reached again, but does not return the header
 itself.

> toSingle step header = loop =<< (read . step) header
>     where loop y = if header/=y then liftM (y:) (read (step y) >>= loop)
>                                 else return []
>        

 forEach is an optimization of (toSimple step header >>= mapM_ act)

> forEach step header act = loop =<< (read . step) header
>      where loop y = if header/=y then (act y >> (read (step y)) >>= loop)
>                                  else return ()

 Now make the root node and all the head nodes. This also exploits mdo:

> makeHeads :: [HeadName] -> (ST st) (Head st)
> makeHeads names = mkDList makeHead names
>     where makeHead before name after = mdo
>             ~newTopNode <- liftM4 (Node ((R 0,C 0),V 0) newHead) (new newTopNode) (new newTopNode)
>                                                                 (new newTopNode) (new newTopNode)
>             newHead <- liftM3 (Head name newTopNode)                         
>                                    (new 0) (new after) (new before)
>             return newHead

 The Head nodes will be places in an array for easy lookup while building moves:

> type HArray st = Array HeadName (Head st)
> hBounds = ((0,1,1),(3,9,9))
> type Root st =  (Head st,HArray st)

 The addMove function creates the (four) nodes that represent a move and adds
 them to the data structure.  The HArray in Root makes for a fast
 lookup of the Head data.

> addMove :: forall st. (Root st) -> Move -> (ST st) (Node st)
> addMove (_,ha) move = mkDList addNode move
>     where addNode :: (Node st) -> (Hint,HeadName) -> (Node st) -> (ST st) (Node st)
>           addNode before (hint,name) after = do
>             let head = ha ! name
>             let below = topNode head
>             above <- read (up below)
>             newNode <- liftM4 (Node hint head) (new above) (new below)
>                                                (new before) (new after)
>             write (down above) newNode
>             write (up below) newNode
>             modify (len head) succ
>             l <- read (len head)
>             seq l (return newNode)

 Create the column headers, including the fast lookup array.  These
 will be resused between puzzles.

> initHA :: (ST st) (Root st)
> initHA = do
>   root <- makeHeads headNames
>   heads <- toSingle next root
>   let ha = array hBounds (zip (map headName heads) heads)
>   return (root,ha)

 Take the Root from initHA and a puzzle Spec and fill in all the Nodes.

> initRoot :: (Root st) -> Spec -> (ST st) ()
> initRoot root spec = do
>   let moves = parseSpec spec
>   mapM_ (addMove root) moves

  Return the column headers to their condition after initHA

> resetRoot :: (Root st) -> (ST st) ()
> resetRoot (root,ha) = do
>   let heads@(first:_) = elems ha
>   let resetHead head = do
>         write (len head) 0
>         let node = topNode head
>         write (down node) node
>         write (up node) node
>       reset (last:[]) = do
>         write (prev root) last
>         write (next root) first
>       reset (before:xs@(head:[])) = do
>         resetHead head
>         write (prev head) before
>         write (next head) root
>         reset xs
>       reset (before:xs@(head:after:_)) = do
>         resetHead head
>         write (prev head) before
>         write (next head) after
>         reset xs
>   reset (root:heads)

 getBest iterates over the unmet constraints (i.e. the Head that are
 reachable from root). It locates the one with the lowest number of
 possible moves that will solve it, aborting early if it finds 0 or 1
 moves.

> getBest :: (Head st) -> (ST st) (Maybe (Head st))
> getBest root = do
>   first <- read (next root)
>   if first == root then return Nothing
>     else do
>       let findMin m best head | head == root = return (Just best)
>                               | otherwise = do
>             l <- read (len head)
>             if l <= 1 then return (Just head)
>               else if l < m then findMin l head =<< read (next head)
>                      else findMin l best =<< read (next head)
>       findMin 10 first first

 The unlink and relink operations are from where Knuth got the name
 "dancing links".  So long as "a" does not change in between, the
 relink call will undo the unlink call.  Similarly, the unconver will
 undo the changes of cover and unconverOthers will undo coverOthers.

> unlink :: (a->STRef st a) -> (a->STRef st a) -> a -> (ST st) ()
> unlink prev next a = do
>   before <- read (prev a)
>   after <- read (next a)
>   write (next before) after
>   write (prev after) before

> relink :: (a->STRef st a) -> (a->STRef st a) -> a -> (ST st) ()
> relink prev next a = do
>   before <- read (prev a)
>   after <- read (next a)
>   write (next before) a
>   write (prev after) a

> cover :: (Head st) -> (ST st) ()
> cover head = do
>   unlink prev next head
>   let eachDown rr = forEach right rr eachRight
>       eachRight nn = do
>         unlink up down nn
>         modify (len $ getHead nn) pred
>   forEach down (topNode head) eachDown

> uncover :: (Head st) -> (ST st) ()
> uncover head = do
>   let eachUp rr = forEach left rr eachLeft
>       eachLeft nn = do
>         modify (len $ getHead nn) succ
>         relink up down nn
>   forEach up (topNode head) eachUp
>   relink prev next head

> coverOthers :: (Node st) -> (ST st) ()
> coverOthers node = forEach right node (cover . getHead)

> uncoverOthers :: (Node st) -> (ST st) ()
> uncoverOthers node = forEach left node (uncover . getHead)

 A helper function for gdance:

> choicesToSpec :: [(Node st)] -> Spec
> choicesToSpec = Spec . (map getHint)

 This is the heart of the algorithm.  I have altered it to return only
 the first solution, or produce an error if none is found.

 Knuth used several goto links to do what is done below with tail
 recursion.

> gdance :: (Head st) -> (ST st) Spec -- [Spec]
> gdance root =
>     let
>         forward choices = do
>             maybeHead <- getBest root
>             case maybeHead of
>                 Nothing -> if null choices
>                              then error "No choices in forward" -- return [] -- for [Spec]
>                              else do -- nextSols <- recover choices -- for [Spec]
>                                      return $ (choicesToSpec choices) -- :nextSols -- for [Spec]
>                 Just head -> do cover head
>                                 startRow <- readSTRef (down (topNode head))
>                                 advance (startRow:choices)
> 
>         advance choices@(newRow:oldChoices) = do
>             let endOfRows = topNode (getHead newRow)
>             if (newRow == endOfRows)
>               then do uncover (getHead newRow)
>                       if (null oldChoices)
>                         then error "No choices in advance" -- return [] -- for [Spec]
>                         else recover oldChoices
>               else do coverOthers newRow
>                       forward choices
> 
>         recover (oldRow:oldChoices) = do
>             uncoverOthers oldRow
>             newRow <- readSTRef (down oldRow)
>             advance (newRow:oldChoices)
> 
>     in forward []


 Convert a text board into a Spec

> parseBoard :: String -> Spec
> parseBoard s = Spec (zip rcs vs'check)
>   where rcs :: [(R,C)]
>         rcs = [ (R r,C c) | r <- rng, c <- rng ]
>         isUnset c = (c=='.') || (c==' ') || (c=='0')
>         isHint c = ('1'<=c) && (c<='9')
>         cs = take 81 $ filter (\c -> isUnset c || isHint c) s
>         vs :: [V]
>         vs = map (\c -> if isUnset c then u else (V $ digitToInt c)) cs
>         vs'check = if 81==length vs then vs else error ("parse of board failed\n"++s)

 This is quite useful as a utility function which partitions the list into groups of n elements.
 Used by showSpec.

> groupTake :: Int->[a]->[[a]]
> groupTake n b = unfoldr foo b
>     where foo [] = Nothing
>           foo b = Just (splitAt n b)
 
 Make a nice 2D ascii board from the Spec (not used at the moment)

> showSpec :: Spec -> String
> showSpec spec = let pa = parse spec
>                     g = groupTake 9 (map (\(V v) -> if v == 0 then '.' else intToDigit v) $ elems pa)
>                     addV line = concat $ intersperse "|" (groupTake 3 line)
>                     addH list = concat $ intersperse ["---+---+---"] (groupTake 3 list)
>                 in unlines $ addH (map addV g)

  One line display

> showCompact spec = map (\(V v) -> intToDigit v) (elems (parse spec))

 The main routine is designed to handle the input from http://www.csse.uwa.edu.au/~gordon/sudoku17

> main = do
>   all <- getContents
>   let puzzles = zip [1..] (map parseBoard (lines all))
>   root <- stToIO initHA
>   let act :: (Int,Spec) -> IO ()
>       act (i,spec) = do
>         answer <- stToIO (do initRoot root spec 
>                              answer <- gdance (fst root) 
>                              resetRoot root
>                              return answer)
>         print (i,showCompact  answer)
>   mapM_ act puzzles

> inits' xn@(_:_) = zipWith take [0..] $ map (const xn) $ undefined:xn
> inits' _        = undefined

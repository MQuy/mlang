module TI.Print where

import           Data.List                      ( sort )
import           Types
import           Print
import           TI.State

-- Show results
showResults :: [TiState] -> String
showResults states =
  iDisplay (iConcat [iLayn (map showState states), showStats (last states)])

showState :: TiState -> Iseq
showState (stack, _, heap, _, _) = iConcat [showStack heap stack, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack = iConcat
  [ iStr "Stk ["
  , iIndent (iInterleave iNewline (map showStackItem stack))
  , iStr " ]"
  ]
 where
  showStackItem addr =
    iConcat [showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr)]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp funAddr argAddr) = iConcat
  [ iStr "NAp "
  , showFWAddr funAddr
  , showDetail funAddr
  , iStr " "
  , showFWAddr argAddr
  , showDetail argAddr
  ]
 where
  showDetail addr = iConcat [iStr " (", showNode (hLookup heap addr), iStr ")"]
showStkNode _ node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) =
  iConcat [iStr "NAp ", showAddr a1, iStr " ", showAddr a2]
showNode (NSupercomb name _ _) = iStr ("NSupercomb " ++ name)
showNode (NNum n             ) = (iStr "NNum ") `iAppend` (iNum n)
showNode (NPrim name _       ) = (iStr "NPrim ") `iAppend` (iStr name)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (genSpaces (4 - toInteger (length str)) ++ str)
  where str = show addr

showStats :: TiState -> Iseq
showStats (_, _, heap, _, stats) = iConcat
  [ iStr "Total number of steps = "
  , iNum (tiStatGetSteps stats)
  , iNewline
  , iNewline
  , showHeap heap
  ]

-- | Print out the contents of the heap
showHeap :: TiHeap -> Iseq
showHeap heap = iConcat
  [iStr "Heap:", iIndent (iConcat (map showHeapAddr addrs))]
 where
  addrs = sort $ hAddresses heap
  showHeapAddr addr =
    iConcat [iNewline, iNum addr, iStr " ", showNode (hLookup heap addr)]

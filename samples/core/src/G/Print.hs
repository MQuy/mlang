module G.Print where

import           Types
import           Print
import           G.State
import           G.Compile

showResults :: [GMState] -> String
showResults states = iDisplay
  (iConcat
    [ iStr "Supercombinator definitions"
    , iNewline
    , iInterleave iNewline (map (showSC s) (getGlobals s))
    , iNewline
    , iNewline
    , iStr "State transitions"
    , iNewline
    , iNewline
    , iLayn (map showState states)
    , iNewline
    , iNewline
    , showStats (last states)
    ]
  )
  where (s : ss) = states

showSC :: GMState -> (String, Addr) -> Iseq
showSC s (name, addr) = iConcat
  [ iStr "Code for "
  , iStr name
  , iNewline
  , showInstructions code
  , iNewline
  , iNewline
  ]
  where (NGlobal arity code) = (hLookup (getHeap s) addr)

showInstructions :: GMCode -> Iseq
showInstructions is = iConcat
  [ iStr "  Code:{"
  , iIndent (iInterleave iNewline (map showInstruction is))
  , iStr "}"
  , iNewline
  ]

showInstruction :: Instruction -> Iseq
showInstruction (Pushglobal f) = iStr "Pushglobal " `iAppend` iStr f
showInstruction (Push       n) = iStr "Push " `iAppend` iNum (toInteger n)
showInstruction (Pushint    n) = iStr "Pushint " `iAppend` iNum (toInteger n)
showInstruction (Update     n) = iStr "Update " `iAppend` iNum (toInteger n)
showInstruction (Pop        n) = iStr "Pop " `iAppend` iNum (toInteger n)
showInstruction (Slide      n) = iStr "Slide " `iAppend` iNum (toInteger n)
showInstruction (Alloc      n) = iStr "Alloc " `iAppend` iNum (toInteger n)
showInstruction (Cond i1 i2  ) = iConcat
  [ iStr "Cond [2: "
  , shortShowInstructions 2 i1
  , iStr ", 1: "
  , shortShowInstructions 2 i2
  , iStr "]"
  ]
showInstruction (Pack t a) =
  iConcat [iStr "Pack ", iNum (fromIntegral t), iStr " ", iNum (fromIntegral a)]
showInstruction (Casejump nis) =
  iStr "Casejump " `iAppend` showAlternatives nis
showInstruction (Split n) = iStr "Split " `iAppend` iNum (fromIntegral n)
showInstruction inst      = iStr $ show inst

showAlternatives :: [(Int, GMCode)] -> Iseq
showAlternatives nis = iConcat
  [iStr "[", iInterleave (iStr ", ") (map showLabelInstructions nis), iStr "]"]
 where
  showLabelInstructions (tag, code) =
    iConcat [iNum (fromIntegral tag), iStr ": ", shortShowInstructions 2 code]

showState :: GMState -> Iseq
showState s = iConcat
  [ showOutput s
  , iNewline
  , showStack s
  , iNewline
  , showVStack s
  , iNewline
  , showDump s
  , iNewline
  , showInstructions (getCode s)
  , iNewline
  ]

showOutput :: GMState -> Iseq
showOutput s = iConcat [iStr "Output:\"", iStr (getOutput s), iStr "\""]

showStack :: GMState -> Iseq
showStack s = iConcat
  [ iStr " Stack:["
  , iIndent
    (iInterleave iNewline (map (showStackItem s) (reverse (getStack s))))
  , iStr "]"
  ]

showStackItem :: GMState -> Addr -> Iseq
showStackItem s a =
  iConcat [iStr (showAddr a), iStr ": ", showNode s a (hLookup (getHeap s) a)]

showVStack :: GMState -> Iseq
showVStack s =
  iConcat [iStr "Vstack:[", iInterleave (iStr ", ") (map iInt (getVStack s))]
    `iAppend` iStr "]"

showDump :: GMState -> Iseq
showDump s = iConcat
  [ iStr "  Dump:["
  , iIndent (iInterleave iNewline (map showDumpItem (reverse (getDump s))))
  , iStr "]"
  ]

showDumpItem :: (GMCode, GMStack) -> Iseq
showDumpItem (code, stack) = iConcat
  [ iStr "<"
  , shortShowInstructions 3 code
  , iStr ", "
  , shortShowStack stack
  , iStr ">"
  ]

shortShowInstructions :: Int -> GMCode -> Iseq
shortShowInstructions number code = iConcat
  [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
 where
  codes = map showInstruction (take number code)
  dotcodes | length code > number = codes ++ [iStr "..."]
           | otherwise            = codes

shortShowStack :: GMStack -> Iseq
shortShowStack stack = iConcat
  [iStr "[", iInterleave (iStr ", ") (map (iStr . showAddr) stack), iStr "]"]

showNode :: GMState -> Addr -> Node -> Iseq
showNode s a (NNum n     ) = iNum (toInteger n)
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
  where v = head [ n | (n, b) <- getGlobals s, a == b ]
showNode s a (NAp a1 a2) =
  iConcat [iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2)]
showNode s a (NInd a1     ) = iConcat [iStr "Ind ", iStr (showAddr a1)]
showNode s a (NConstr t as) = iConcat
  [ iStr "Cons "
  , iNum (fromIntegral t)
  , iStr " ["
  , iInterleave (iStr ", ") (map (iStr . showAddr) as)
  , iStr "]"
  ]

showStats :: GMState -> Iseq
showStats s =
  iConcat [iStr "Steps taken = ", iNum (toInteger (statGetSteps (getStats s)))]

compileArgs :: [(String, Expr)] -> GMEnvironment -> GMEnvironment
compileArgs defs env =
  zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs

boxInteger :: Int -> GMState -> GMState
boxInteger n state = putStack (a : getStack state) (putHeap h' state)
  where (h', a) = hAlloc (getHeap state) (NNum n)

unboxInteger :: Addr -> GMState -> Int
unboxInteger a state = ub (hLookup (getHeap state) a)
 where
  ub (NNum i) = i
  ub n        = error "Unboxing a non-integer"

pop :: Int -> GMState -> GMState
pop n state = putStack (drop n (getStack state)) state

showAddr :: Addr -> String
showAddr a = "#" ++ show a

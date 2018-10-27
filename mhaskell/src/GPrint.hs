module GPrint where

import           Type
import           GCompile
import           GEval

data Iseq =
  INil
  | IStr String
  | IAppend Iseq Iseq
  | IIndent Iseq
  | INewline

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr = IStr

iNum :: Int -> Iseq
iNum n = IStr (show n)

iInt :: Int -> Iseq
iInt n = iStr (show n)

iAppend :: Iseq -> Iseq -> Iseq
iAppend = IAppend

iIndent :: Iseq -> Iseq
iIndent = IIndent

iNewline :: Iseq
iNewline = INewline

iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq, 0)]

genSpaces :: Int -> String
genSpaces n = replicate (fromIntegral n) ' '

flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten _ ((INewline, indent) : seqs) =
  "\n" ++ genSpaces indent ++ flatten indent seqs
flatten col ((INil     , _) : seqs) = flatten col seqs
flatten col ((IIndent s, _) : seqs) = flatten col ((s, col + 2) : seqs)
flatten col ((IStr    s, _) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)

-- FixedWith Number	: Display a number with a fixed width
iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (genSpaces (width - length digits) ++ digits)
  where digits = show n

-- Layout numbered: Layout a list of Iseqs numbered with 1) 2) 3) etc
iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (zipWith lay_item [1 ..] seqs)
 where
  lay_item n iseq = iConcat [iFWNum 4 n, iStr ") ", iIndent iseq, iNewline]

-- support functions to for the pretty printer
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []       = INil
iInterleave _   [i     ] = i      -- no 'ins' at end of list
iInterleave ins (i : is) = (i `iAppend` ins) `iAppend` iInterleave ins is

pprExpr :: Expr -> Iseq
pprExpr (ENum n               ) = iNum n
pprExpr (EVar v               ) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1 `iAppend` iStr " ") `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr) = iConcat
  [ iStr keyword
  , iNewline
  , iIndent (pprDefns defns)
  , iNewline
  , iIndent (iStr "in ")
  , pprExpr expr
  ]
 where
  keyword | isrec     = "letrec"
          | otherwise = "let"

pprExpr (ELam vars expr) =
  iStr "\\ "
    `iAppend` iInterleave (iStr " ") (map iStr vars)
    `iAppend` iStr " . "
    `iAppend` pprExpr expr
pprExpr (EConst tag arity) =
  iConcat [iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}"]
pprExpr (ECase expr as) =
  iStr "case " `iAppend` pprExpr expr `iAppend` iStr " of " `iAppend` iIndent
    (pprCases as)

pprAExpr :: Expr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = (iStr "(" `iAppend` pprExpr e) `iAppend` iStr ")"

pprDefns :: [(String, Expr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [iStr ";", iNewline]

pprDefn :: (String, Expr) -> Iseq
pprDefn (name, expr) =
  iConcat [iStr name, iStr " ", iStr " = ", iIndent (pprExpr expr)]

pprProgram :: [ScDefn] -> Iseq
pprProgram scDefs = iInterleave sep (map pprScDef scDefs)
  where sep = iConcat [iStr ";", iNewline]

pprScDef :: (String, [String], Expr) -> Iseq
pprScDef (name, vars, expr) = iConcat
  [ iStr name
  , iStr " "
  , iInterleave (iStr " ") (map iStr vars)
  , iStr " = "
  , iIndent (pprExpr expr)
  ]

pprCases :: [EAlter] -> Iseq
pprCases as = iInterleave sep (map pprCase as)
  where sep = iConcat [iStr " ;", iNewline]

pprCase :: EAlter -> Iseq
pprCase (tag, as, expr) =
  prTag `iAppend` iInterleave (iStr " ") (map iStr as) `iAppend` pprExpr expr
  where prTag = iStr "<" `iAppend` iStr (show tag) `iAppend` iStr ">"

pprint :: Program -> String
pprint prog = iDisplay (pprProgram prog)

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
showInstruction (Push       n) = iStr "Push " `iAppend` iNum n
showInstruction (Pushint    n) = iStr "Pushint " `iAppend` iNum n
showInstruction (Update     n) = iStr "Update " `iAppend` iNum n
showInstruction (Pop        n) = iStr "Pop " `iAppend` iNum n
showInstruction (Slide      n) = iStr "Slide " `iAppend` iNum n
showInstruction (Alloc      n) = iStr "Alloc " `iAppend` iNum n
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
showNode s a (NNum n     ) = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
  where v = head [ n | (n, b) <- getGlobals s, a == b ]
showNode s a (NAp a1 a2) =
  iConcat [iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2)]
showNode s a (NInd a1    ) = iConcat [iStr "Ind ", iStr (showAddr a1)]
showNode s a (NConst t as) = iConcat
  [ iStr "Cons "
  , iNum (fromIntegral t)
  , iStr " ["
  , iInterleave (iStr ", ") (map (iStr . showAddr) as)
  , iStr "]"
  ]

showStats :: GMState -> Iseq
showStats s = iConcat [iStr "Steps taken = ", iNum (statGetSteps (getStats s))]

showAddr :: Addr -> String
showAddr a = "#" ++ show a

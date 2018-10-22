module Print where

import           Types

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

iNum :: Integer -> Iseq
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

genSpaces :: Integer -> String
genSpaces n = replicate (fromIntegral n) ' '

flatten :: Integer -> [(Iseq, Integer)] -> String
flatten _ [] = ""
flatten _ ((INewline, indent) : seqs) =
  "\n" ++ genSpaces indent ++ flatten indent seqs
flatten col ((INil     , _) : seqs) = flatten col seqs
flatten col ((IIndent s, _) : seqs) = flatten col ((s, col + 2) : seqs)
flatten col ((IStr    s, _) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)

-- FixedWith Number	: Display a number with a fixed width
iFWNum :: Integer -> Integer -> Iseq
iFWNum width n = iStr (genSpaces (width - toInteger (length digits)) ++ digits)
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

pprCases :: [Alter] -> Iseq
pprCases as = iInterleave sep (map pprCase as)
  where sep = iConcat [iStr " ;", iNewline]

pprCase :: Alter -> Iseq
pprCase (tag, as, expr) =
  prTag `iAppend` iInterleave (iStr " ") (map iStr as) `iAppend` pprExpr expr
  where prTag = iStr "<" `iAppend` iStr (show tag) `iAppend` iStr ">"

pprint :: Program -> String
pprint prog = iDisplay (pprProgram prog)

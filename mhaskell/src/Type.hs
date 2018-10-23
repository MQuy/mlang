module Type where

import qualified Data.Map                      as M

data Expr =
  EVar String
  | ENum Int
  | EConst Int Int
  | EAp Expr Expr
  | ECase Expr [EAlter]
  | ELet Bool [EDef] Expr
  | ELam [String] Expr
  deriving Show

type EAlter = (Int, [String], Expr)
type EDef = (String, Expr)

type Program = [ScDefn]
type ScDefn = (String, [String], Expr)

-- Infor which is used during parsing {currentTag, {name: (tag, arity)}}
data PEInfo = PEInfo {
  currentTag :: Int,
  tName :: M.Map String (Int, Int)
}

-- Supercombinator's first letter has to be character, therefore, we use tag as sc's name
peInfo :: PEInfo
peInfo = PEInfo { currentTag = 16, tName = M.empty }

peTagInc :: String -> Int -> PEInfo -> PEInfo
peTagInc dName arity PEInfo { currentTag = tag, tName = tName } =
  PEInfo { currentTag = tag + 1, tName = M.insert dName (tag, arity) tName }

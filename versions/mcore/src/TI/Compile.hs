module TI.Compile where

import           Data.List                      ( mapAccumL )
import           Types
import           TI.State

-- Compile
compile :: Program -> TiState
compile program =
  (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
 where
  initialStack           = [addressOfMain]
  (initialHeap, globals) = buildInitialHeap scDefs
  addressOfMain          = aLookup globals "main" (error "main is not defined")
  scDefs                 = program ++ preludeDefs ++ extraPreludeDefs

buildInitialHeap :: [ScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap1, sc_addrs ++ pri_addrs)
 where
  (heap , sc_addrs ) = mapAccumL allocateSc hInitial scDefs
  (heap1, pri_addrs) = mapAccumL allocatePrim heap primitives

allocateSc :: TiHeap -> ScDefn -> (TiHeap, (String, Addr))
allocateSc heap (name, args, body) = (heap1, (name, addr))
  where (heap1, addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (String, Primitive) -> (TiHeap, (String, Addr))
allocatePrim heap (name, prim) =
  let (heap1, addr) = hAlloc heap (NPrim name prim) in (heap1, (name, addr))

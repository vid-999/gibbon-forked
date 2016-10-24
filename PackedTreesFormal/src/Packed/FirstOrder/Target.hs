{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Packed.FirstOrder.Target where

--------------------------------------------------------------------------------

import Data.Word (Word8)
import Data.List (foldl1')

import Data.Loc (noLoc)
import Language.C.Quote.C (cexp, cstm, cinit, cdecl)
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C

import Packed.HigherOrder.L1_Source (T1 (..))

--------------------------------------------------------------------------------
-- * AST definition

type Var = String
type Tag = Word8

data Triv
    = VarTriv Var
    | IntTriv Int
    | TagTriv Tag
  deriving (Show)

data Tail
    = RetValsT [Triv] -- ^ Only in tail position, for returning from a function.
    | LetCallT { binds  :: [(Var,Ty)],
                 rator  :: Var,
                 rands  :: [Triv],
                 typ    :: Ty,
                 tmpNam :: Maybe Var,  -- hack, needed a name for the returned struct
                 fields :: Maybe [Var], -- names of all the fields in the struct
                 bod    :: Tail }
    | LetPrimCallT { binds :: [(Var,Ty)],
                     prim  :: Prim,
                     rands :: [Triv],
                     bod   :: Tail }
    | Switch Triv [(Tag,Tail)] (Maybe Tail)
    -- ^ For casing on numeric tags.
    | TailCall Var [Triv]
  deriving (Show)

data Ty
    = IntTy
    | TagTy -- ^ A single byte.
    | SymTy -- ^ Symbols used in writing compiler passes.
            --   It's an alias for Int, an index into a symbol table.
    | CursorTy -- ^ A byte-indexing pointer.
    | ProdTy [Ty]
    | SymDictTy T1
      -- ^ We allow built-in dictionaries from symbols to a value type.
  deriving (Show,Eq,Ord)

data Prim
    = AddP
    | SubP
    | MulP
    | DictInsertP -- ^ takes k,v,dict
    | DictLookupP -- ^ takes k,dict, errors if absent
    | NewBuf 
    -- ^ Allocate a new buffer, return a cursor.
    | WriteTag
    -- ^ Write a static tag value, takes a cursor to target.
    | WriteInt
    -- ^ Write (leaf) data, takes cursor,int
    | ReadTag
    -- ^ Read one byte from the cursor and advance it.
    | ReadInt
      -- ^ Read an 8 byte Int from the cursor and advance.
  deriving (Show)

data FunDecl = FunDecl Var [(Var,Ty)] Ty Tail
  deriving (Show)

--------------------------------------------------------------------------------
-- * C codegen

newtype Cg = Cg { unwrapCg :: () }

codegenTriv :: Triv -> C.Exp
codegenTriv (VarTriv v) = C.Var (C.toIdent v noLoc) noLoc
codegenTriv (IntTriv i) = [cexp| $i |]
codegenTriv (TagTriv i) = [cexp| $i |]

codegenTail :: Tail -> C.Type -> C.Exp -> [C.BlockItem]
codegenTail (RetValsT [tr]) _ty ret =
    let assn t = [ C.BlockStm [cstm| $ret = $t; |] ]
    in assn $ codegenTriv tr
codegenTail (RetValsT ts) ty ret =
    [ C.BlockStm [cstm| $ret = $(C.CompoundLit ty args noLoc); |] ]
    where args = map (\a -> (Nothing,C.ExpInitializer (codegenTriv a) noLoc)) ts
codegenTail (Switch _tr [] Nothing) _ty _ret = []
codegenTail (Switch _tr [] (Just t)) ty ret =
    codegenTail t ty ret
codegenTail (Switch tr ((ctag,ctail):cs) def) ty ret =
    [ C.BlockStm $ C.If comp thenTail elseTail noLoc ]
    where comp = [cexp| $(codegenTriv tr) == $ctag |]
          thenTail = mkBlock $ codegenTail ctail ty ret
          elseTail = Just $ mkBlock $ codegenTail (Switch tr cs def) ty ret
codegenTail (TailCall v ts) _ty ret =
    [ C.BlockStm [cstm| $ret = $( C.FnCall (cid v) args noLoc); |] ]
    where args = map codegenTriv ts
codegenTail (LetCallT bnds rator rnds typ (Just nam) (Just fields) bod) ty ret =
    let init = [ C.BlockDecl [cdecl| $ty:(codegenTy typ) $id:nam; |]
               , C.BlockStm [cstm| $nam  = $(C.FnCall (cid rator) (map codegenTriv rnds) noLoc); |] ]
        assn t x y = C.BlockDecl [cdecl| $ty:t $id:x = $exp:y; |]
        bind (v,t) f = assn (codegenTy t) v (C.Member (cid nam) (C.toIdent f noLoc) noLoc)
    in init ++ (zipWith bind bnds fields) ++ (codegenTail bod ty ret)
codegenTail (LetPrimCallT bnds prim rnds bod) ty ret =
    let bod' = codegenTail bod ty ret
    in
      case prim of
        AddP -> let [(outV,outT)] = bnds
                    [pleft,pright] = rnds
                in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV; |]
                   , C.BlockStm [cstm| $id:outV = $(codegenTriv pleft) + $(codegenTriv pright); |]]
        SubP -> let (outV,outT) = head bnds
                    [pleft,pright] = rnds
                in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV; |]
                   , C.BlockStm [cstm| $id:outV = $(codegenTriv pleft) - $(codegenTriv pright); |]]
        MulP -> let [(outV,outT)] = bnds
                    [pleft,pright] = rnds
                in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV; |]
                   , C.BlockStm [cstm| $id:outV = $(codegenTriv pleft) * $(codegenTriv pright); |]]
        DictInsertP -> undefined
        DictLookupP -> undefined
        NewBuf -> undefined
        WriteTag -> let [(outV,CursorTy)] = bnds
                        [(TagTriv tag),(VarTriv cur)] = rnds
                    in [ C.BlockStm [cstm| *($id:cur) = $tag; |]
                       , C.BlockDecl [cdecl| char* $id:outV = $id:cur + 1; |] ]
        WriteInt -> let [(outV,CursorTy)] = bnds
                        [val,(VarTriv cur)] = rnds
                    in [ C.BlockStm [cstm| *($id:cur) = $(codegenTriv val); |]
                       , C.BlockDecl [cdecl| char* $id:outV = (char*)((int*)($id:cur) + 1); |] ]
        ReadTag -> let [(tagV,TagTy),(curV,CursorTy)] = bnds
                       [(VarTriv cur)] = rnds
                   in [ C.BlockDecl [cdecl| $ty:(codegenTy TagTy) $id:tagV = *($id:cur); |]
                      , C.BlockDecl [cdecl| char* $id:curV = $id:cur + 1; |] ]
        ReadInt -> let [(valV,IntTy),(curV,CursorTy)] = bnds
                       [(VarTriv cur)] = rnds
                   in [ C.BlockDecl [cdecl| int $id:valV = *($id:cur); |]
                      , C.BlockDecl [cdecl| char* $id:curV = (char*)((int*)($id:cur) + 1); |] ]

codegenTy :: Ty -> C.Type
codegenTy = undefined
                                                        
mkBlock :: [C.BlockItem] -> C.Stm
mkBlock ss = C.Block ss noLoc

cid :: Var -> C.Exp
cid v = C.Var (C.toIdent v noLoc) noLoc

leafTag :: Word8
leafTag = fromIntegral 0

nodeTag :: Word8
nodeTag = fromIntegral 1

exadd1 :: FunDecl
exadd1 = FunDecl "add1" [("t",CursorTy),("tout",CursorTy)] CursorTy exadd1Tail 

exadd1Tail :: Tail
exadd1Tail =
    LetPrimCallT [("ttag",TagTy),("t2",CursorTy)] ReadTag [VarTriv "t"]
  $ Switch (VarTriv "ttag")
           [(leafTag,leafCase),
            (nodeTag,nodeCase)]
           Nothing
    where leafCase =
              LetPrimCallT [("tout2",CursorTy)] WriteTag [TagTriv leafTag, VarTriv "tout"]
            $ LetPrimCallT [("n",IntTy),("t3",CursorTy)] ReadInt [VarTriv "t2"]
            $ LetPrimCallT [("tout3",CursorTy)] WriteInt [VarTriv "n", VarTriv "tout2"]
            $ RetValsT [VarTriv "t3", VarTriv "tout3"]
          nodeCase =
              LetPrimCallT [("tout2",CursorTy)] WriteTag [TagTriv nodeTag, VarTriv "tout"]
            $ LetCallT [("t3",CursorTy),("tout3",CursorTy)] "add1" [VarTriv "t2", VarTriv "tout2"] (ProdTy [CursorTy,CursorTy]) (Just "tmp1") (Just ["tmpleaf","tmpnode"])
            $ TailCall "add1" [VarTriv "t3", VarTriv "tout3"]

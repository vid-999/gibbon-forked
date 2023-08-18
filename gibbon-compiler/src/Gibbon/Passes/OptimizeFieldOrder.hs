module Gibbon.Passes.OptimizeFieldOrder
  ( shuffleDataCon
  ) where


-- Gibbon imports
import           Gibbon.Common
import           Gibbon.Language
import           Gibbon.Language.Syntax
import           Gibbon.Passes.AccessPatternsAnalysis (DataConAccessMap,
                                                       FieldMap,
                                                       generateAccessGraphs)
import           Gibbon.Passes.CallGraph              (ProducersMap (..),
                                                       generateProducerGraph)
import           Gibbon.Passes.ControlFlowGraph       (getFunctionCFG)
import           Gibbon.Passes.DefinitionUseChains    (DefUseChainsFunctionMap (..),
                                                       generateDefUseChainsFunction,
                                                       progToVEnv)
import           Gibbon.Passes.SolveLayoutConstrs     (solveConstrs)
import           Gibbon.Pretty

import           Control.Exception                    (evaluate)
import           Data.List                            as L

-- Haskell imports
import           Data.Map                             as M
import           Data.Maybe                           as Maybe
import           Data.Time.Clock                      (diffUTCTime,
                                                       getCurrentTime)
import           Prelude                              as P
import           System.CPUTime
import           System.IO.Unsafe                     as U
import           System.TimeIt
import           Text.PrettyPrint.GenericPretty


-- | Data structure to store output from ILP solver.
-- | Maps DataCon to bijection of new indices -> fields of datacon
type FieldOrder = M.Map DataCon [Integer]


-- TODO: Make FieldOrder an argument passed to shuffleDataCon function.
shuffleDataCon ::
     (FreeVars (e l d), Ord l, Ord d, Ord (e l d), Out d, Out l, Show d)
  => Prog (PreExp e l d)
  -> PassM (Prog (PreExp e l d))
shuffleDataCon prg@Prog {ddefs, fundefs, mainExp}
    -- Get the DataCons  (con for now) from the fundefs
    -- Get the functions (function for now) from the fundefs
    --let dcons = P.concat $ P.map (\fn@(FunDef{funMeta = FunMeta { funOptLayout = layout }  }) -> case layout of
    --                                                                              Single dcon -> [dcon]
    --                                                                              _           -> []
    --                                                                              ) (M.elems fundefs)
    
--let funcs = P.concat $ P.map (\fn@(FunDef{funName, funMeta = FunMeta { funOptLayout = layout }  }) -> case layout of
    --                                                                                                 Single dcon -> [(fromVar funName)]
    --                                                                                                 _           -> []
    --                                                                                                 ) (M.elems fundefs)
    
-- Tuple (funcName, datconstructor) for which soft constraints need to be generated.
 = do
  let defuseChainsMap =
        P.map (generateDefUseChainsFunction (progToVEnv prg)) (M.elems fundefs)
    
--dbgTraceIt (sdoc (M.keys (P.head defuseChainsMap))) dbgTraceIt ("\n") pure prg
  let producersMap = generateProducerGraph prg
    
--dbgTraceIt (sdoc (producersMap)) dbgTraceIt ("\n") pure prg
  let list_pair_func_dcon =
        P.concat $
        P.map
          (\fn@(FunDef {funName, funMeta = FunMeta {funOptLayout = layout}}) ->
             case layout of
               Single dcon -> [(fromVar funName, dcon)]
                                                                                  -- only handles optimizing a single dcon for a function right now.
                                                                                  -- its okay for the current examples
                                                                                  -- but should be extended
               _           -> [])
          (M.elems fundefs)
    
-- If a total ordering is defined for a function (by the user), then we should just use that instead.
    -- The total ordering defined by the user should just override
    -- get the function for which the total ordering is defined and get the corresponding total ordering.
    
-- Tuple (funcName, map) map : map from data constructor to the user defined ordering.
  let list_pair_func_total_ordering =
        P.concat $
        P.map
          (\fn@(FunDef { funName
                       , funMeta = FunMeta {userConstraintsDataCon = totalOrdering}
                       }) ->
             case totalOrdering of
               Nothing -> []
               Just m  -> [(fromVar funName, m)])
          (M.elems fundefs)
    --pure prg
  case list_pair_func_total_ordering of
    [] ->
      case list_pair_func_dcon of
        [] -> pure prg
        (funcName, dcon):xs -> do
          let [fundef] =
                P.concatMap
                  (\fn@(FunDef {funName}) ->
                     if ((fromVar funName) == funcName)
                       then [fn]
                       else [])
                  (M.elems fundefs)
          let (ddefs', fundef', fieldorder) =
                optimizeFunctionWRTDataCon ddefs fundef dcon
          let fundefs' = M.delete (toVar funcName) fundefs
          let fundefs'' = M.insert (toVar funcName) fundef' fundefs'
          let venv = progToVEnv prg
          let pmap = generateProducerGraph prg
          let p = prg {ddefs = ddefs', fundefs = fundefs'', mainExp = mainExp}
          let prg' =
                genNewProducersAndRewriteProgram
                  (toVar funcName)
                  (dcon ++ "tmp")
                  fieldorder
                  venv
                  pmap
                  p
          return prg'
          -- (funcName, dconMap):xs -> case list_pair_func_dcon of
          --                                   [] ->
          --                                         do
          --                                         -- assumption, only one data constructor to optimize for.
          --                                         let [dcon] = M.keys dconMap
          --                                         let field_len = P.length $ snd . snd $ lkp ddefs dcon
          --                                         let [fundef] = P.concatMap (\fn@(FunDef{funName}) -> if ((fromVar funName) == funcName) then [fn] else []) (M.elems fundefs)
          --                                         let fieldorder = optimizeDataConOrderFunc (M.empty) dconMap fundef [(dcon, field_len)] (M.empty)
          --                                         let shuffled_ddefs = optimizeDataCon fieldorder ddefs
          --                                         fds' <- mapM (shuffleDataConFunBody fieldorder) (M.elems fundefs)
          --                                         let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
          --                                         mainExp' <- case mainExp of
          --                                                         Nothing -> return Nothing
          --                                                         Just (mn, ty) -> Just . (,ty) <$> shuffleDataConExp fieldorder mn
          --                                         let l1 = prg {   ddefs = shuffled_ddefs
          --                                                       , fundefs = fundefs'
          --                                                       , mainExp = mainExp'
          --                                                     }
          --                                         pure prg
          --                                   _  ->    -- check if there are soft constraints for function
          --                                            -- if so generate cfg, access patterns...
          --                                         do
          --                                         -- The assumption is there is only one data constructor to optimize for
          --                                         -- TODO : case for more that one.
          --                                         let [dcon] = P.concatMap (\(f, d) -> if f == funcName
          --                                                                     then [d]
          --                                                                     else []
          --                                                               ) list_pair_func_dcon
          --                                         let [fundef] = P.concatMap (\fn@(FunDef{funName}) -> if ((fromVar funName) == funcName) then [fn] else []) (M.elems fundefs)
          --                                         let (cfg, fieldMap) = generateCfgFunctions (M.empty) (M.empty) [fundef] dcon
          --                                         let field_len = P.length $ snd . snd $ lkp ddefs dcon
          --                                         let fieldorder = optimizeDataConOrderFunc fieldMap dconMap fundef [(dcon, field_len)] (M.empty)
          --                                         -- Make a new Type for the
          --                                         let shuffled_ddefs = optimizeDataCon fieldorder ddefs
          --                                         fds' <- mapM (shuffleDataConFunBody fieldorder) (M.elems fundefs)
          --                                         let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
          --                                         mainExp' <- case mainExp of
          --                                                         Nothing -> return Nothing
          --                                                         Just (mn, ty) -> Just . (,ty) <$> shuffleDataConExp fieldorder mn
          --                                         let l1 = prg {   ddefs = shuffled_ddefs
          --                                                       , fundefs = fundefs'
          --                                                       , mainExp = mainExp'
          --                                                     }
          --                                         pure prg
    
-- -- let remaining_orderings =  P.concatMap (\(a, b)  -> if (existsTuple list_pair_func_total_ordering a)
    -- --                                                     then []
    -- --                                                     else [(a, b)]
    -- --                                        ) list_pair_func_dcon
    
-- -- -- In case there are soft constraints with no strong or /after constraints
    -- -- case remaining_orderings of
    -- --   [] -> pure prg
    -- --   (funcName, dcon):xs -> do
    -- --                          let [fundef] = P.concatMap (\fn@(FunDef{funName}) -> if ((fromVar funName) == funcName) then [fn] else []) (M.elems fundefs)
    -- --                          let (cfg, fieldMap) = generateCfgFunctions (M.empty) (M.empty) [fundef] dcon
    -- --                          let field_len = P.length $ snd . snd $ lkp ddefs dcon
    -- --                          let fieldorder = optimizeDataConOrderFunc fieldMap (M.empty) fundef [(dcon, field_len)] (M.empty)
    -- --                          let shuffled_ddefs = optimizeDataCon fieldorder ddefs
    -- --                          fds' <- mapM (shuffleDataConFunBody fieldorder) (M.elems fundefs)
    -- --                          let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
    -- --                          mainExp' <- case mainExp of
    -- --                                            Nothing -> return Nothing
    -- --                                            Just (mn, ty) -> Just . (,ty) <$> shuffleDataConExp fieldorder mn
    -- --                          let l1 = prg {   ddefs = shuffled_ddefs
    -- --                                         , fundefs = fundefs'
    -- --                                         , mainExp = mainExp'
    -- --                                       }
    -- --                          pure l1
    
-- -- case list_pair_func_dcon of
    -- --        [] -> let fieldMap = genEdgesFromTotalOrdering list_pair_func_total_ordering (M.elems fundefs) (M.empty)
    -- --                in case list_pair_func_total_ordering of
    -- --                           [] -> pure prg
    -- --                           x:xs -> do let (fn, m') = x
    -- --                                       -- assuming that there is only one data con per function but this needs to be amended
    -- --                                       -- account for multiple data constructors per function.
    -- --                                      let dcons  = P.concat $ P.map (\(fundef, map) -> M.keys map) (M.toList fieldMap)
    -- --                                      let [dcon] = M.keys m'
    -- --                                      let field_len  = P.length $ snd . snd $ lkp ddefs dcon
    -- --                                      let fieldorder = locallyOptimizeFieldOrdering fieldMap dcons (M.elems fundefs) fn field_len (M.empty) 1
    -- --                                      let shuffled_ddefs = optimizeDataCon fieldorder ddefs
    -- --                                      fds' <- mapM (shuffleDataConFunBody fieldorder) (M.elems fundefs)
    -- --                                      let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
    -- --                                      mainExp' <- case mainExp of
    -- --                                                   Nothing -> return Nothing
    -- --                                                   Just (mn, ty) -> Just . (,ty) <$> shuffleDataConExp fieldorder mn
    -- --                                      let l1 = prg {   ddefs = shuffled_ddefs
    -- --                                                     , fundefs = fundefs'
    -- --                                                     , mainExp = mainExp'
    -- --                                                   }
    -- --                                      pure l1 --dbgTraceIt (sdoc field_len) dbgTraceIt ("\n")
    
-- --        x:xs -> do
    -- --                let (fname, dcon) = x
    -- --                let [fundef] = P.concat $ P.map (\fn@(FunDef{funName}) -> if ((fromVar funName) == fname) then [fn] else []) (M.elems fundefs)
    -- --                let (cfg, fieldMap) = generateCfgFunctions (M.empty) (M.empty) [fundef] dcon
    -- --                let field_len = P.length $ snd . snd $ lkp ddefs dcon
    -- --                let fieldorder = locallyOptimizeFieldOrdering fieldMap [dcon] [fundef] fname field_len (M.empty) 0
    -- --                let shuffled_ddefs = optimizeDataCon fieldorder ddefs
    -- --                fds' <- mapM (shuffleDataConFunBody fieldorder) (M.elems fundefs)
    -- --                let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
    -- --                mainExp' <- case mainExp of
    -- --                  Nothing -> return Nothing
    -- --                  Just (mn, ty)-> Just . (,ty) <$> shuffleDataConExp fieldorder mn
    -- --                let l1 = prg { ddefs = shuffled_ddefs
    -- --                               , fundefs = fundefs'
    -- --                               , mainExp = mainExp'
    -- --                             }
    -- --                pure l1 --dbgTraceIt (sdoc fieldorder) dbgTraceIt ("\n")
    
-- --let (cfgs, fieldMap) = generateCfgFunctions (M.empty) (M.empty) (M.elems fundefs) (L.head dcons)
    -- -- TODO: probably better to make this a map from dcon to its num fields.
    -- --let field_len = P.length $ snd . snd $ lkp ddefs (L.head dcons)
    -- -- Instead of explicitly passing the function name, this should come from a annotation at the front end or something like that.
    -- --let fieldorder = locallyOptimizeFieldOrdering fieldMap dcons (M.elems fundefs) (L.head funcs) field_len (M.empty)
    -- --let functions  = M.elems fundefs
    -- -- NOTE : shuffling ddefs makes a lot of assumptions right now.
    -- -- Mainly that we are just doing it for one function
    -- -- So we do not care out the globally optimal layout of the data constructor
    
-- -- TODO: Use the strong user constraints that were parsed and make the correct field order for it.
    -- -- Check if all fields are specified, if yes then bypass the solver.
    -- -- If no, then insert constraints in the solver that cannot be over-ruled and check first if the constraints are satisfiable or not.
    -- -- Have user constraints as edges with MaxInt weight.
    
-- --let shuffled_ddefs = optimizeDataCon fieldorder ddefs
    -- --fds' <- mapM (shuffleDataConFunBody fieldorder) (M.elems fundefs)
    -- --let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
    -- --mainExp' <- case mainExp of
    -- --    Nothing -> return Nothing
    -- --    Just (mn, ty)-> Just . (,ty) <$> shuffleDataConExp fieldorder mn
    -- --let l1 = prg { ddefs = shuffled_ddefs
    -- --           , fundefs = fundefs'
    -- --           , mainExp = mainExp'
    -- --           }
    -- --pure l1 --dbgTraceIt (sdoc fieldorder) dbgTraceIt ("\n")
    -- -- dbgTraceIt (sdoc fieldorder) dbgTraceIt ("\n") dbgTraceIt (sdoc funcs) dbgTraceIt ("\n") dbgTraceIt (sdoc dcons)

optimizeFunctionWRTDataCon ::
     (FreeVars (e l d), Ord l, Ord d, Ord (e l d), Out d, Out l)
  => DDefs (TyOf (PreExp e l d))
  -> FunDef (PreExp e l d)
  -> DataCon
  -> (DDefs (TyOf (PreExp e l d)), FunDef (PreExp e l d), FieldOrder)
optimizeFunctionWRTDataCon ddefs fundef@FunDef { funName
                                               , funBody
                                               , funTy
                                               , funArgs
                                               } datacon =
  let cfg = getFunctionCFG fundef
      fieldMap = generateAccessGraphs cfg M.empty fundef [datacon]
      field_len = P.length $ snd . snd $ lkp' ddefs datacon
      fieldorder =
        optimizeDataConOrderFunc
          fieldMap
          (M.empty)
          fundef
          [(datacon, field_len)]
          (M.empty)
      -- make a function to generate a new data con as a value instead of changing the order of fields in the original one.
      [(dcon, order)] = M.toList fieldorder
      (newDDefs, newDcon) = optimizeDataCon (dcon, order) ddefs
      fundef' = shuffleDataConFunBody fieldorder fundef newDcon
   in (newDDefs, fundef', fieldorder)

changeCallNameInRecFunction ::
     Var -> FunDef (PreExp e l d) -> FunDef (PreExp e l d)
changeCallNameInRecFunction newFunName f@FunDef { funName
                                                , funArgs
                                                , funTy
                                                , funBody
                                                , funMeta
                                                } =
  case funMeta of
    FunMeta {funRec} ->
      case funRec of
        Rec ->
          f
            { funName = newFunName
            , funArgs = funArgs
            , funTy = funTy
            , funBody = (fixExp funBody)
            , funMeta
            }
        _ ->
          f
            { funName = newFunName
            , funArgs = funArgs
            , funTy = funTy
            , funBody = funBody
            , funMeta
            }
  where
    fixExp funBody =
      case funBody of
        DataConE loc dcon args -> DataConE loc dcon (P.map fixExp args)
        VarE {} -> funBody
        LitE {} -> funBody
        CharE {} -> funBody
        FloatE {} -> funBody
        LitSymE {} -> funBody
        AppE f locs args ->
          if f == funName
            then AppE newFunName locs (P.map fixExp args)
            else AppE f locs (P.map fixExp args)
        PrimAppE f args -> PrimAppE f (P.map fixExp args)
        LetE (v, loc, ty, rhs) bod ->
          let rhs' = fixExp rhs
              bod' = fixExp bod
           in LetE (v, loc, ty, rhs') bod'
        CaseE scrt mp ->
          let mp' = P.map (\(a, b, c) -> (a, b, fixExp c)) mp
           in CaseE scrt mp'
        IfE a b c -> IfE (fixExp a) (fixExp b) (fixExp c)
        MkProdE xs -> MkProdE (P.map fixExp xs)
        ProjE i e -> error "getExpTyEnv: TODO ProjE"
        TimeIt e ty b -> error "getExpTyEnv: TODO TimeIt"
        WithArenaE v e -> error "getExpTyEnv: TODO WithArenaE"
        SpawnE f locs args -> error "getExpTyEnv: TODO SpawnE"
        SyncE -> error "getExpTyEnv: TODO SyncE"
        Ext _ -> error "getExpTyEnv: TODO Ext"
        MapE {} -> error "getExpTyEnv: TODO MapE"
        FoldE {} -> error "getExpTyEnv: TODO FoldE"

genNewProducersAndRewriteProgram ::
     (FreeVars (e l d), Ord l, Ord d, Ord (e l d), Out d, Out l, Show d)
  => Var
  -> DataCon
  -> FieldOrder
  -> Env2 (TyOf (PreExp e l d))
  -> ProducersMap (PreExp e l d)
  -> Prog (PreExp e l d)
  -> Prog (PreExp e l d)
genNewProducersAndRewriteProgram funName newDataConName newdataConOrder venv pmap prg@Prog { ddefs
                                                                                           , fundefs
                                                                                           , mainExp
                                                                                           } =
  case mainExp of
    Nothing ->
      error "genNewProducersAndRewriteProgram : Program has no main expression."
    Just (mexp, ty) ->
      let variablesAndProducers = getVariableAndProducer funName pmap venv mexp
       in case variablesAndProducers of
            [] -> error "no variable and producers found to modify"
            (var, producer):[] ->
              let newProducerName = toVar ((fromVar producer) ++ "_new")
                  oldProducerBody = (M.lookup producer fundefs)
               in case oldProducerBody of
                    Just body ->
                      let newProducerBody@FunDef { funName
                                                 , funBody
                                                 , funTy
                                                 , funArgs
                                                 } =
                            shuffleDataConFunBody
                              newdataConOrder
                              body
                              newDataConName
                          newProducerBody' =
                            changeCallNameInRecFunction
                              newProducerName
                              newProducerBody
                          fundefs' =
                            M.insert newProducerName newProducerBody' fundefs
                          newMainExp =
                            callNewProducerForVarInMain
                              var
                              False
                              producer
                              newProducerName
                              mexp
                       in prg
                            { ddefs = ddefs
                            , fundefs = fundefs'
                            , mainExp = Just (newMainExp, ty)
                            }
                    _ -> error ""
            x:xs -> error "more than one variable and producer not handled yet."


-- Function to find the the variable/s that have the type that's being optimized for the given function f
-- Also return the producer of those variable/s
-- Arguments
-- Var -> Name of the function being optimized
-- pmap -> variable to producer map
-- (PreExp e l d) -> expression to search over
-- Return
-- [(Var, Producer)]
getVariableAndProducer ::
     (Ord l, Ord d, Ord (e l d), Out d, Out l)
  => Var
  -> ProducersMap (PreExp e l d)
  -> Env2 (TyOf (PreExp e l d))
  -> (PreExp e l d)
  -> [(Var, Var)]
getVariableAndProducer funName pMap venv@Env2 {vEnv, fEnv} exp =
  case exp of
    DataConE loc dcon args ->
      P.concatMap (getVariableAndProducer funName pMap venv) args
    VarE {} -> []
    LitE {} -> []
    CharE {} -> []
    FloatE {} -> []
    LitSymE {} -> []
    AppE f locs args ->
      P.concatMap (getVariableAndProducer funName pMap venv) args
    PrimAppE f args ->
      P.concatMap (getVariableAndProducer funName pMap venv) args
    LetE (v, loc, ty, rhs) bod ->
      let varOf =
            case rhs of
              AppE f locs args ->
                if f == funName
                  then let potentialVarsOfTy =
                             P.map
                               (\exp ->
                                  case exp of
                                    VarE v ->
                                      case (lookupVEnv' v venv) of
                                        Just e  -> dbgTraceIt (sdoc v) Just v
                                                                                                                                          --in if (isPackedTy d)
                                                                                                                                          --   then Just v
                                                                                                                                          --   else Nothing
                                        Nothing -> Just v --Nothing
                                    _ -> dbgTraceIt ("here") Nothing)
                               args
                           justVariables = Maybe.catMaybes potentialVarsOfTy
                        in if (P.length justVariables) == 0
                             then error "getVariableAndProducer: no args found!"
                             else if (P.length justVariables) > 1
                                    then dbgTraceIt
                                           (sdoc justVariables)
                                           Just
                                           (justVariables !! 1) --error "getVariableAndProducer: Not implemented!"
                                    else Just (P.head justVariables)
                  else dbgTraceIt (sdoc f) Nothing
              _ -> Nothing
          producers =
            (getVariableAndProducer funName pMap venv rhs) ++
            (getVariableAndProducer funName pMap venv bod)
       in case varOf of
            Just var ->
              let varType = lookupVEnv' var venv
               in case varType of
                    Just ty ->
                      let producerExp = M.lookup (var, ty) pMap
                       in case producerExp of
                            Just (AppE f locs args) -> [(var, f)] ++ producers
                            _ ->
                              error
                                "getVariableAndProducer: producer other than a function call not expected."
                    Nothing -> dbgTraceIt (sdoc (M.elems vEnv)) []
            Nothing -> producers
    -- a == DataCon
    -- b == [(Var, loc)]
    -- c == Case Body
    CaseE scrt mp ->
      P.concatMap (\(a, b, c) -> getVariableAndProducer funName pMap venv c) mp
    IfE a b c ->
      let producersA = getVariableAndProducer funName pMap venv a
          producersB = getVariableAndProducer funName pMap venv b
          producersC = getVariableAndProducer funName pMap venv c
       in producersA ++ producersB ++ producersC
    MkProdE xs -> P.concatMap (getVariableAndProducer funName pMap venv) xs
    ProjE i e -> error "getExpTyEnv: TODO ProjE"
    TimeIt e ty b -> error "getExpTyEnv: TODO TimeIt"
    WithArenaE v e -> error "getExpTyEnv: TODO WithArenaE"
    SpawnE f locs args -> error "getExpTyEnv: TODO SpawnE"
    SyncE -> error "getExpTyEnv: TODO SyncE"
    Ext _ -> error "getExpTyEnv: TODO Ext"
    MapE {} -> error "getExpTyEnv: TODO MapE"
    FoldE {} -> error "getExpTyEnv: TODO FoldE"

rewriteMain ::
     (FreeVars (e l d), Ord l, Ord d, Ord (e l d), Out d, Out l, Show d)
  => Var
  -> DataCon
  -> FieldOrder
  -> Env2 (TyOf (PreExp e l d))
  -> ProducersMap (PreExp e l d)
  -> DDefs (TyOf (PreExp e l d))
  -> FunDefs (PreExp e l d)
  -> (PreExp e l d)
  -> (Maybe (PreExp e l d), FunDefs (PreExp e l d))
rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs mainExp =
  (Just mainExp, fundefs) --case mainExp of

--     DataConE loc dcon args -> let ret = P.map (rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs) args
--                                   mainExp' = P.head $ Maybe.catMaybes $ P.map fst ret
--                                   fds' = M.unions $ P.map snd ret
--                                 in (Just mainExp', fds')
--     VarE{}    -> (Nothing, M.empty)
--     LitE{}    -> (Nothing, M.empty)
--     CharE{}   -> (Nothing, M.empty)
--     FloatE{}  -> (Nothing, M.empty)
--     LitSymE{} -> (Nothing, M.empty)
--     AppE f locs args -> let ret   = P.map (rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs) args
--                             mainExp' = P.head $ Maybe.catMaybes $ P.map fst ret
--                             fds'  = M.unions $ P.map snd ret
--                           in (Just mainExp', fds')
--     PrimAppE f args  -> let ret = P.map (rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs) args
--                             mainExp' = P.head $ Maybe.catMaybes $ P.map fst ret
--                             fds'  = M.unions $ P.map snd ret
--                           in (Just mainExp', fds')
--     LetE (v,loc,ty,rhs) bod -> case rhs of
--                                   AppE f1 locs1 args1 -> if f1 == funName
--                                                           then let getTyDcon  = (getTyOfDataCon ddefs newDataCon) -- you will need to make a seperate one ? PackedTy (getTyOfDataCon ddfs c) loc
--                                                                    getVarOfTy = P.concatMap (\exp -> case exp of
--                                                                                                     VarE v    -> let tyVar = lookupVEnv v venv
--                                                                                                                      isSameType = if (show tyVar) == getTyDcon then (dbgTraceIt (show tyVar)) True else (dbgTraceIt (show tyVar)) False
--                                                                                                                    in if isSameType then [v]
--                                                                                                                                     else [ ]
--                                                                                                     LitSymE v -> let tyVar = lookupVEnv v venv
--                                                                                                                      isSameType = if (show tyVar) == getTyDcon then True else False
--                                                                                                                    in if isSameType then [v]
--                                                                                                                                     else [ ]
--                                                                                             ) args1
--                                                                   in if P.length getVarOfTy == 0 then error "genNewProducersAndRewriteProgram : No variables of type found in the arguments of the function call."
--                                                                      else if P.length getVarOfTy > 1 then error "genNewProducersAndRewriteProgram : multiple variables of same type not implemented yet."
--                                                                      else
--                                                                        case (M.lookup ((P.head getVarOfTy, lookupVEnv (P.head getVarOfTy) venv)) pmap) of
--                                                                                                         Nothing  -> error "genNewProducersAndRewriteProgram : No producer found for variable."
--                                                                                                         Just exp -> case exp of
--                                                                                                                         AppE f1 locs1 args1 -> let fundef   = M.lookup f1 fundefs
--                                                                                                                                                  in case fundef of
--                                                                                                                                                         Nothing -> error ""
--                                                                                                                                                         Just fndef -> let
--                                                                                                                                                                         -- Need to make the function name unique here.
--                                                                                                                                                                         f'@FunDef{funName=fN, funBody=fB, funTy=fTy, funArgs=fArgs} = shuffleDataConFunBody dataConOrder fndef newDataCon
--                                                                                                                                                                         newVarNameForProducer = toVar ((fromVar fN) ++ "_tmp")
--                                                                                                                                                                         f'' = f' {funName=newVarNameForProducer, funBody=fB, funTy=fTy, funArgs=fArgs}
--                                                                                                                                                                         fundefs' = M.insert newVarNameForProducer f'' fundefs
--                                                                                                                                                                         mainExp' =  callNewProducerForVarInMain (P.head getVarOfTy) False f1 newVarNameForProducer mainExp
--                                                                                                                                                                        in (Just mainExp', fundefs')
--                                                                                                                         PrimAppE f2 args2 -> error "genNewProducersAndRewriteProgram : Case not implemented."
--                                                                                                                         _ -> error "genNewProducersAndRewriteProgram : Case not implemented."
--                                                           else
--                                                             let (mainExprhs, fundefsrhs) = rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs rhs
--                                                                 (mainExpbod, fundefsbod) = rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs bod
--                                                                 newMain = catMaybes

--                                   PrimAppE f2 args2 -> error "genNewProducersAndRewriteProgram : TODO PrimAppE"
--                                   _ -> error "genNewProducersAndRewriteProgram : Case not implemented."
--     -- a == DataCon
--     -- b == [(Var, loc)]
--     -- c == Case Body
--     CaseE scrt mp -> let ret = P.map (\(a, b, c) -> rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs c) mp
--                          mainExp' = P.head $ Maybe.catMaybes $ P.map fst ret
--                          fds' = M.unions $ P.map snd ret
--                        in (Just mainExp', fds')
--     IfE a b c -> let ret1 = rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs a
--                      ret2 = rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs b
--                      ret3 = rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs c
--                      mainExp' = P.head $ Maybe.catMaybes $ P.map fst [ret1, ret2, ret3]
--                      fds'     = M.unions $ P.map snd [ret1, ret2, ret3]
--                    in (Just mainExp', fds')
--     MkProdE xs -> let ret = P.map (rewriteMain funName newDataCon dataConOrder venv pmap ddefs fundefs) xs
--                       mainExp' = P.head $ Maybe.catMaybes $ P.map fst ret
--                       fds'  = M.unions $ P.map snd ret
--                     in (Just mainExp', fds')
--     ProjE i e -> error "genNewProducersAndRewriteProgram: TODO ProjE"
--     TimeIt e ty b -> error "genNewProducersAndRewriteProgram: TODO TimeIt"
--     WithArenaE v e -> error "genNewProducersAndRewriteProgram: TODO WithArenaE"
--     SpawnE f locs args -> error "genNewProducersAndRewriteProgram: TODO SpawnE"
--     SyncE   -> error "genNewProducersAndRewriteProgram: TODO SyncE"
--     Ext _   -> error "genNewProducersAndRewriteProgram: TODO Ext"
--     MapE{}  -> error "genNewProducersAndRewriteProgram: TODO MapE"
--     FoldE{} -> error "genNewProducersAndRewriteProgram: TODO FoldE"

-- For a variable that's produced change the old producer to the new producer.
-- Args
-- Var  -> Variable whose producer needs to be changed
-- Bool -> a switch for modifying the call or not.
-- Var  -> Name of the old producer
-- Var  -> Name of the new producer
-- (PreExp e l d) -> Main Expression
-- Return value
-- (PreExp e l d) -> New Main Expression
callNewProducerForVarInMain ::
     Var -> Bool -> Var -> Var -> (PreExp e l d) -> (PreExp e l d)
callNewProducerForVarInMain var boolModify oldProducer newProducer mainExp =
  case mainExp of
    DataConE loc dcon args ->
      let args' =
            P.map
              (callNewProducerForVarInMain
                 var
                 boolModify
                 oldProducer
                 newProducer)
              args
       in DataConE loc dcon args'
    VarE {} -> mainExp
    LitE {} -> mainExp
    CharE {} -> mainExp
    FloatE {} -> mainExp
    LitSymE {} -> mainExp
    AppE f locs args ->
      let args' =
            P.map
              (callNewProducerForVarInMain
                 var
                 boolModify
                 oldProducer
                 newProducer)
              args
       in if (f == oldProducer) && boolModify
            then AppE newProducer locs args'
            else AppE f locs args'
    PrimAppE f args ->
      let args' =
            P.map
              (callNewProducerForVarInMain
                 var
                 boolModify
                 oldProducer
                 newProducer)
              args
       in PrimAppE f args'
    LetE (v, loc, ty, rhs) bod ->
      let rhs' =
            if v == var
              then callNewProducerForVarInMain
                     var
                     True
                     oldProducer
                     newProducer
                     rhs
              else callNewProducerForVarInMain
                     var
                     False
                     oldProducer
                     newProducer
                     rhs
          bod' =
            callNewProducerForVarInMain
              var
              boolModify
              oldProducer
              newProducer
              bod
       in LetE (v, loc, ty, rhs') bod'
      -- a == DataCon
      -- b == [(Var, loc)]
      -- c == Case Body
    CaseE scrt mp ->
      let mp' =
            P.map
              (\(a, b, c) ->
                 let c' =
                       callNewProducerForVarInMain
                         var
                         boolModify
                         oldProducer
                         newProducer
                         c
                  in (a, b, c'))
              mp
       in CaseE scrt mp'
    IfE a b c ->
      let a' =
            callNewProducerForVarInMain var boolModify oldProducer newProducer a
          b' =
            callNewProducerForVarInMain var boolModify oldProducer newProducer b
          c' =
            callNewProducerForVarInMain var boolModify oldProducer newProducer c
       in IfE a' b' c'
    MkProdE xs ->
      let xs' =
            P.map
              (callNewProducerForVarInMain
                 var
                 boolModify
                 oldProducer
                 newProducer)
              xs
       in MkProdE xs'
    ProjE i e -> error "getExpTyEnv: TODO ProjE"
    TimeIt e ty b -> error "getExpTyEnv: TODO TimeIt"
    WithArenaE v e -> error "getExpTyEnv: TODO WithArenaE"
    SpawnE f locs args -> error "getExpTyEnv: TODO SpawnE"
    SyncE -> error "getExpTyEnv: TODO SyncE"
    Ext _ -> error "getExpTyEnv: TODO Ext"
    MapE {} -> error "getExpTyEnv: TODO MapE"
    FoldE {} -> error "getExpTyEnv: TODO FoldE"

genEdgesFromTotalOrdering ::
     [(String, DataConMap)] -> [FunDef (PreExp e l d)] -> FieldMap -> FieldMap
genEdgesFromTotalOrdering lstTotalOrdering fundefs mapIn =
  case lstTotalOrdering of
    [] -> mapIn
    x:xs ->
      let (funcName, map) = x
          [fundef@(FunDef {funName})] =
            P.concat $
            P.map
              (\fn@(FunDef {funName}) ->
                 if ((fromVar funName) == funcName)
                   then [fn]
                   else [])
              fundefs
          dconOrderings = M.toList map
          dconOrderings' =
            P.map
              (\(dcon, strongOrderings) ->
                 let edges =
                       P.map
                         (\strongOrder ->
                            case strongOrder of
                              Strong a b -> ((a, b), (P.toInteger 100)))
                         strongOrderings
                  in (dcon, edges))
              dconOrderings
          orderMap = M.fromList dconOrderings'
          newMap = M.insert funName orderMap mapIn
          newMap' = genEdgesFromTotalOrdering xs fundefs newMap
       in newMap'

existsTuple :: [(String, a)] -> String -> Bool
existsTuple lst name =
  case lst of
    [] -> False
    (x, y):xs ->
      if x == name
        then True
        else existsTuple xs name


-- This is pointless and just goes through the function we are locally optimizing for maybe a cleverer way to do in haskell
-- Since this problem is to locally optimize for a particular function right now we are not concerned with finding the best
-- optimal layout for the complete program.
locallyOptimizeFieldOrdering ::
     FieldMap
  -> [DataCon]
  -> [FunDef (PreExp e l d)]
  -> String
  -> Int
  -> FieldOrder
  -> Int
  -> FieldOrder
locallyOptimizeFieldOrdering fieldMap dcons fundefs funcName field_len orderIn mode =
  case fundefs of
    [] -> orderIn
    x:xs ->
      let map' =
            generateLocallyOptimalOrderings
              fieldMap
              dcons
              x
              funcName
              field_len
              orderIn
              mode
          map'' =
            locallyOptimizeFieldOrdering
              fieldMap
              dcons
              xs
              funcName
              field_len
              map'
              mode
       in map''


-- type DataConAccessMap = M.Map DataCon [ ((Integer, Integer) , Integer ) ]
-- type FieldMap = M.Map FunDef (PreExp e l d) DataConAccessMap

-- type DataConMap = M.Map DataCon [UserOrdering]

-- type FieldOrder = M.Map DataCon [Integer]

-- To solve constraints for one function we should have a function like this.
-- optimizeDataCons :: DataConAccessMap -> DataConMap -> fundef -> String(functionName) -> [DataCon] -> FieldOrder -> FieldOrder
genUserConstrs :: [UserOrdering] -> [Constr]
genUserConstrs userOrdering =
  case userOrdering of
    (Strong a b):xs    -> [Absolute (a, b)] ++ genUserConstrs xs
    (Immediate a b):xs -> [Imm (a, b)] ++ genUserConstrs xs
    []                 -> []
 
-- Takes in Field access map
 -- Takes in user constraints for data con for that function
 -- takes in the function def for the function
 -- Takes in [(DataCon, Int)], ie, data con name, and number of fields for data constructors that need to be optimized
 -- Returns optimal order of fields of a data constructor.


-- Timing for filtering the blogs based on a keyword
timeSolver ::
     (IO [(Int, Int)] -> [(Int, Int)])
  -> IO [(Int, Int)]
  -> IO ([(Int, Int)], Double)
timeSolver f f' = do
  t1 <- getCurrentTime
  a <- evaluate $ (f f')
  t2 <- getCurrentTime
  let delt = fromRational (toRational (diffUTCTime t2 t1))
  putStrLn ("iter time: " ++ show delt)
  return $! (a, delt)

optimizeDataConOrderFunc ::
     FieldMap
  -> DataConMap
  -> FunDef (PreExp e l d)
  -> [(DataCon, Int)]
  -> FieldOrder
  -> FieldOrder
optimizeDataConOrderFunc dconAccessMap dconUserConstr fundef@FunDef { funName
                                                                    , funBody
                                                                    , funTy
                                                                    , funArgs
                                                                    } datacons orderIn =
  let lstDconEdges = M.findWithDefault M.empty funName dconAccessMap
   in case datacons of
        [] -> orderIn
        [(x, field_len)] ->
          let softEdges = M.findWithDefault [] x lstDconEdges
              softConstrs = P.map Soft softEdges
              userOrdering = M.findWithDefault [] x dconUserConstr
              userConstrs = genUserConstrs userOrdering
              allConstrs = softConstrs ++ userConstrs
               --field_len    = P.length $ snd . snd $ lkp ddefs x
           in case allConstrs of
                [] -> orderIn
                _ ->
                  let (layout, t) =
                        U.unsafePerformIO $
                        timeSolver U.unsafePerformIO (solveConstrs allConstrs)
                                               --let layout = U.unsafePerformIO (solveConstrs allConstrs)
                                             -- In case we don't get orderings for some of the fields in the data con
                                             -- to be safe we should complete the layout orderings of the missing fields.
                      fix_missing =
                        if (P.length layout) < field_len
                          then let indices = [0 .. (field_len - 1)]
                                   minuslist = makeneg field_len
                                   partial = fillList minuslist layout
                                   avail = P.map (\(a, b) -> a) layout
                                   navail = deleteMany avail indices
                                   new = fillminus1 partial navail
                                in new
                          else let layout' = L.sort layout
                                in P.map (\(a, b) -> b) layout' --dbgTraceIt (sdoc layout') dbgTraceIt ("\n")
                      fieldorder = M.insert x (integerList fix_missing) orderIn
                   in fieldorder --dbgTraceIt (sdoc allConstrs) dbgTraceIt ("\n") dbgTraceIt (sdoc fieldorder) dbgTraceIt ("\n")
        _ ->
          error
            "OptimizeFieldOrder: optimizeDataConOrderFunc more that one data constructor per function not implemented yet."


-- for the function for which we are locally optimizing for, find the optimal layout of the data constructors that we care about.
-- "Locally optimizing for the function"
generateLocallyOptimalOrderings ::
     FieldMap
  -> [DataCon]
  -> FunDef (PreExp e l d)
  -> String
  -> Int
  -> FieldOrder
  -> Int
  -> FieldOrder
generateLocallyOptimalOrderings fieldMap datacons fundef@FunDef { funName
                                                                , funBody
                                                                , funTy
                                                                , funArgs
                                                                } funcName field_len orderIn mode =
  if (fromVar funName) == funcName
    then let lstDconEdges = M.findWithDefault M.empty funName fieldMap
          in case datacons of
               [] -> orderIn
               x:xs ->
                 let dconEdges = M.findWithDefault [] x lstDconEdges
                     dconEdges' = P.map (\a -> Soft a) dconEdges
                  in case dconEdges' of
                       [] -> orderIn
                       _ ->
                         let layout =
                               U.unsafePerformIO $ (solveConstrs dconEdges')
                                             -- In case we don't get orderings for some of the fields in the data con
                                             -- to be safe we should complete the layout orderings of the missing fields.
                             fix_missing =
                               if (P.length layout) < field_len
                                 then let indices = [0 .. (field_len - 1)]
                                          minuslist = makeneg field_len
                                          partial = fillList minuslist layout
                                          avail = P.map (\(a, b) -> a) layout
                                          navail = deleteMany avail indices
                                          new = fillminus1 partial navail
                                       in new
                                 else let layout' = L.sort layout
                                       in P.map (\(a, b) -> b) layout' --dbgTraceIt (sdoc layout') dbgTraceIt ("\n")
                             fieldorder =
                               M.insert x (integerList fix_missing) orderIn
                             fieldorder' =
                               generateLocallyOptimalOrderings
                                 fieldMap
                                 xs
                                 fundef
                                 funcName
                                 field_len
                                 fieldorder
                                 mode
                          in dbgTraceIt
                               (sdoc dconEdges)
                               dbgTraceIt
                               ("\n")
                               dbgTraceIt
                               (sdoc fieldorder')
                               dbgTraceIt
                               ("\n")
                               fieldorder' -- dbgTraceIt (sdoc dconEdges) dbgTraceIt ("\n") dbgTraceIt (sdoc fieldorder') dbgTraceIt ("\n")
    else orderIn --dbgTraceIt (sdoc funName)

makeneg :: Int -> [Int]
makeneg len =
  if len <= 0
    then []
    else (makeneg (len - 1)) ++ [-1]

integerList :: [Int] -> [Integer]
integerList lst =
  case lst of
    []   -> []
    x:xs -> [P.toInteger x] ++ (integerList xs)

fillList :: [Int] -> [(Int, Int)] -> [Int]
fillList old vals =
  case vals of
    [] -> old
    x:xs ->
      let (a, b) = x
          edited = (L.take b old) ++ [a] ++ (L.drop (b + 1) old)
       in fillList edited xs


-- https://www.reddit.com/r/haskell/comments/u841av/trying_to_remove_all_the_elements_that_occur_in/
deleteOne :: Eq a => a -> [a] -> [a]
deleteOne _ [] = [] -- Nothing to delete
deleteOne x (y:ys)
  | x == y = ys -- Drop exactly one matching item
deleteOne x (y:ys) = y : deleteOne x ys -- Drop one, but not this one (doesn't match).

deleteMany :: Eq a => [a] -> [a] -> [a]
deleteMany []     = id -- Nothing to delete
deleteMany (x:xs) = deleteMany xs . deleteOne x -- Delete one, then the rest.

fillminus1 :: [Int] -> [Int] -> [Int]
fillminus1 lst indices =
  case lst of
    [] -> []
    x:xs ->
      case indices of
        [] -> lst
        y:ys ->
          if x == -1
            then [y] ++ fillminus1 xs ys
            else [x] ++ fillminus1 xs indices

shuffleDataConFunBody ::
     FieldOrder -> FunDef (PreExp e l d) -> DataCon -> (FunDef (PreExp e l d))
shuffleDataConFunBody fieldorder f@FunDef {funBody} newDataCon =
  let funBody' = shuffleDataConExp fieldorder newDataCon funBody
   in f {funBody = funBody'}

shuffleDataConExp :: FieldOrder -> DataCon -> (PreExp e l d) -> (PreExp e l d)
shuffleDataConExp fieldorder newDataCon ex =
  case ex of
    DataConE loc dcon args ->
      let args' = shuffleDataConArgs fieldorder dcon args
          newCon =
            if (M.member dcon fieldorder)
              then newDataCon
              else dcon
       in DataConE loc newCon args'
    VarE {} -> ex
    LitE {} -> ex
    CharE {} -> ex
    FloatE {} -> ex
    LitSymE {} -> ex
    AppE f locs args ->
      AppE f locs (P.map (shuffleDataConExp fieldorder newDataCon) args)
    PrimAppE f args ->
      PrimAppE f (P.map (shuffleDataConExp fieldorder newDataCon) args)
    LetE (v, loc, ty, rhs) bod ->
      let rhs' = shuffleDataConExp fieldorder newDataCon rhs
          bod' = shuffleDataConExp fieldorder newDataCon bod
       in LetE (v, loc, ty, rhs') bod'
    IfE a b c ->
      let a' = shuffleDataConExp fieldorder newDataCon a
          b' = shuffleDataConExp fieldorder newDataCon b
          c' = shuffleDataConExp fieldorder newDataCon c
       in IfE a' b' c'
    MkProdE xs -> MkProdE (P.map (shuffleDataConExp fieldorder newDataCon) xs)
    ProjE i e -> ProjE i (shuffleDataConExp fieldorder newDataCon e)
    CaseE scrt mp ->
      let mp' =
            P.map
              (\(a, b, c) ->
                 let b' = shuffleDataConCase fieldorder a b
                     c' = shuffleDataConExp fieldorder newDataCon c
                     a' =
                       if (M.member a fieldorder)
                         then newDataCon
                         else a
                  in (a', b', c'))
              mp
       in CaseE scrt mp'
    TimeIt e ty b ->
      let e' = shuffleDataConExp fieldorder newDataCon e
       in TimeIt e' ty b
    WithArenaE v e ->
      let e' = shuffleDataConExp fieldorder newDataCon e
       in WithArenaE v e'
    SpawnE f locs args ->
      SpawnE f locs (P.map (shuffleDataConExp fieldorder newDataCon) args)
    SyncE -> SyncE
    Ext _ -> ex
    MapE {} -> error "shuffleFieldOrdering: TODO MapE"
    FoldE {} -> error "shuffleFieldOrdering: TODO FoldE"

shuffleDataConArgs ::
     FieldOrder -> DataCon -> [(PreExp e l d)] -> [(PreExp e l d)]
shuffleDataConArgs fieldorder dcon exps =
  if (M.member dcon fieldorder)
    then permute (findWithDefault [] dcon fieldorder) exps
    else exps

shuffleDataConCase :: FieldOrder -> DataCon -> [(Var, loc)] -> [(Var, loc)]
shuffleDataConCase fieldorder dcon vs =
  if (M.member dcon fieldorder)
    then permute (findWithDefault [] dcon fieldorder) vs
    else vs


-- optimizeDataCon :: FieldOrder -> DDefs (UrTy a) -> DDefs (UrTy a)
-- optimizeDataCon fieldorder ddefs = M.fromList (go (M.toList ddefs))
--     where
--         go list = case list of
--                     [] -> []
--                     x:xs -> case x of
--                                 (var, ddef) -> let (new_ddef = permuteDDef fieldorder ddef
--                                                    in [(var, new_ddef)] ++ (go xs)
optimizeDataCon ::
     (DataCon, [Integer])
  -> DDefs (TyOf (PreExp e l d))
  -> (DDefs (TyOf (PreExp e l d)), DataCon)
optimizeDataCon (dcon, newindices) ddefs =
  let (tycon, (_, fields)) = lkp' ddefs dcon
      newFields = permute newindices fields
      newDcon = dcon ++ "tmp" -- Change this to use gensym
      DDef {tyName, tyArgs, dataCons} = lookupDDef' ddefs (fromVar tycon)
      newDDef =
        DDef
          { tyName = tyName
          , tyArgs = tyArgs
          , dataCons = (dataCons ++ [(newDcon, newFields)])
          }
      ddefs' = M.delete tycon ddefs
      ddefs'' = insertDD newDDef ddefs'
   in (ddefs'', newDcon)


-- | Lookup a Datacon.  Return (TyCon, (DataCon, [flds]))
lkp' ::
     DDefs (TyOf (PreExp e l d))
  -> DataCon
  -> (Var, (DataCon, [(IsBoxed, TyOf (PreExp e l d))]))
lkp' dds con
   -- Here we try to lookup in ALL datatypes, assuming unique datacons:
 =
  case [ (tycon, variant)
       | (tycon, DDef {dataCons}) <- M.toList dds
       , variant <- L.filter ((== con) . fst) dataCons
       ] of
    [] ->
      error $ "OptimizeFieldOrder -> lookupDataCon: could not find constructor!"
    [hit] -> hit
    _ ->
      error $
      "OptimizeFieldOrder -> lookupDataCon: found multiple occurences of constructor!"


-- | Lookup a ddef in its entirety
lookupDDef' ::
     DDefs (TyOf (PreExp e l d)) -> TyCon -> DDef (TyOf (PreExp e l d))
lookupDDef' mp tycon =
  case M.lookup (toVar tycon) mp of
    Just x  -> x
    Nothing -> error $ "OptimizeFieldOrder: lookupDDef' failed!"


-- permuteDDef :: FieldOrder -> DDef (UrTy a) -> (DDef (UrTy a) , DataCon)
-- permuteDDef fieldorder DDef{tyName, tyArgs, dataCons} =  case tyName of
--     _ -> let (newDataCons, newDconName) = reverse_dataCons fieldorder dataCons
--            in (DDef{tyName, tyArgs, dataCons=newDataCons},newDconName)

-- reverse_dataCons :: FieldOrder -> [(DataCon, [(IsBoxed, UrTy a)])] -> ([(DataCon, [(IsBoxed, UrTy a)])], DataCon)
-- reverse_dataCons fieldorder list = case list of
--     [] -> []
--     (layout_name, fields):xs -> if (M.member layout_name fieldorder)
--                                     then let rev_fields  = permute (findWithDefault [] layout_name fieldorder) fields
--                                              newDconName = gensym layout_name
--                                            in ([(layout_name, fields)] ++ [(newDconName, rev_fields)] ++ (reverse_dataCons fieldorder xs) , newDconName)
--                                 else
--                                     ([(layout_name, fields)] ++ (reverse_dataCons fieldorder xs) , "")
permute :: [Integer] -> [a] -> [a]
permute indices list =
  case indices of
    []   -> []
    x:xs -> [list !! (P.fromInteger x)] ++ permute xs list

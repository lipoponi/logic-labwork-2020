module D.Solver (main) where
  import qualified Data.List as List
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  import D.Common
  import D.Templates

  generateProof :: Map.Map Exp Bool -> Exp -> [Exp]
  generateProof p x = snd $ go (structuredEval p x)
    where
      go :: VExp -> (Exp,[Exp])
      go vx@(VVar v name)  = let x = EVar name in (x,[if v then x else ENeg x])
      go vx@(VNeg v inner) = let
        (ix,iproof) = go inner
        x = ENeg ix

        template = if v then const [] else template13
        proof = (reverse $ template ix) ++ iproof
        in (x,proof)
      go vx@(VConj v l r)  = let
        (lx,lproof) = go l
        (rx,rproof) = go r
        x = EConj lx rx

        template =
          if value l && value r then template4
          else if value l then template3
          else if value r then template2
          else template1
        proof = (reverse $ template lx rx) ++ rproof ++ lproof
        in (x,proof)
      go vx@(VDisj v l r)  = let
        (lx,lproof) = go l
        (rx,rproof) = go r
        x = EDisj lx rx

        template =
          if value l && value r then template8
          else if value l then template7
          else if value r then template6
          else template5
        proof = (reverse $ template lx rx) ++ rproof ++ lproof
        in (x,proof)
      go vx@(VImpl v l r)  = let
        (lx,lproof) = go l
        (rx,rproof) = go r
        x = EImpl lx rx

        template =
          if value l && value r then template12
          else if value l then template11
          else if value r then template10
          else template9
        proof = (reverse $ template lx rx) ++ rproof ++ lproof
        in (x,proof)

  generate :: Exp -> Bool -> [Exp] -> [Exp]
  generate x r hypos = go (Map.fromList $ zip hypos (repeat r)) variables
    where
      variables :: [Exp]
      variables = extractVars x List.\\ hypos

      a :: Exp
      a = if r then x else ENeg x

      go :: Map.Map Exp Bool -> [Exp] -> [Exp]
      go p [] = reverse $ generateProof p x
      go p (v:vs) = fproof ++ tproof ++ (excludedMiddleLaw v) ++ [
          (EImpl (EImpl v a) (EImpl (EImpl (ENeg v) a) (EImpl (EDisj v (ENeg v)) a))),
          (EImpl (EImpl (ENeg v) a) (EImpl (EDisj v (ENeg v)) a)),
          (EImpl (EDisj v (ENeg v)) a),
          a
        ]
        where
          actualHypos = (Map.foldlWithKey' (\s k r -> Set.insert (if r then k else ENeg k) s) Set.empty p)
          fproof = unhypothesize (ENeg v) actualHypos $ go (Map.insert v False p) vs
          tproof = unhypothesize v actualHypos $ go (Map.insert v True p) vs

  solve :: String -> [String]
  solve line = case minimalSolution statement of
    Nothing    -> lines ":("
    Just (r,h) -> show statement' : (map show $ generate statement r h)
      where statement' = if r then Statement statement h else Statement (ENeg statement) (map ENeg h)
    where
      statement :: Exp
      statement = parse line

  main :: IO ()
  main = do
    line <- getLine
    putStr $ (unlines . solve) line
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Treeish where

import Debug.Trace
import Data.Text(Text)
import Data.Text.Metrics(levenshteinNorm)
import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Or(zipOrWith,Or)
import qualified Data.Or as Or

data TreeVal
  = VBool Bool
  | VText Text
  | VDouble Double
  | VDict (Map Text TreeVal)
  | VArray [TreeVal]
  deriving (Eq,Show)

--

-- | a Portion must always be between 0.0 and 1.0 inclusive
newtype Portion = Portion { unportion :: Double }
  deriving (Eq,Show)
  deriving newtype (Num,Fractional)

data Step
  = DictStep !Text
  | ArrStep !Integer
  deriving (Eq,Show)

type Diff = [(Portion, [Step])]

collapse :: Diff -> Portion
collapse [] = 1.0
collapse xs = average $ map fst xs

average :: [Portion] -> Portion
average xs = sum xs / (fromIntegral $ length xs)

data MapProvenance a = First !a | Second !a | Both !a !a
  deriving Show

diff :: TreeVal -> TreeVal -> Diff
diff a_orig b_orig = --trace (unlines ["reducing",show a, show b]) $
  case (a_orig,b_orig) of
    (VBool a,VBool b) ->  [(if a==b then 1.0 else 0.0,[])]
    (VDouble a,VDouble b) ->
      -- avoiding div-by-zero
      [(if a == 0.0 && b == 0.0
       then 1.0
       else realToFrac $ 1.0 - ((abs (a - b)) / (abs a + abs b))
      ,[])]

    (VText a,VText b) -> [(realToFrac $ levenshteinNorm a b,[])]
    -- looks weird, just applying an index.
    (VArray a, VArray b) -> concat $  zipWith (\i f -> f i) [0..] $ zipOrWith orComp a b
    (VDict a,VDict b) ->
      concatMap assemble . Map.toList $ Map.unionWith recur (fmap First a) (fmap Second b)
    _ -> [(0.0,[])]

  where
        recur :: MapProvenance TreeVal
              -> MapProvenance TreeVal
              -> MapProvenance TreeVal
        recur (First a') (Second b') =  Both a' b'
        recur (Second b') (First a')  =  Both  b' a'
        recur a' b' = trace "oops" $ error $ "broken: " <> show (a',b')

        orComp :: Or TreeVal TreeVal -> Integer -> [(Portion, [Step])]
        orComp res index = case res of
          Or.Fst _ -> [(0.0,[ArrStep index])]
          Or.Snd _ -> [(0.0,[ArrStep index])]
          Or.Both e1 e2 -> map (fmap (ArrStep index:)) $ diff e1 e2
        -- orComp :: Or TreeVal TreeVal -> (Portion,[Step])
        -- orComp = \case
        --   Fst _ -> [(


        assemble :: (Text,MapProvenance TreeVal) -> [(Portion, [Step])]
        assemble (key, First _) = missing key
        assemble (key, Second _) = missing key
        -- portion doesn't change, if we care about the steps taken
        -- to get here that adjustment happens later.
        assemble (key, Both a' b') = fmap (DictStep key:) <$> diff a' b'

        -- if it's missing, it can't be anything but a 0.0 match
        missing :: Text -> Diff
        missing key =  [(0.0, [DictStep key])]

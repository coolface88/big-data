module BackTracking where
 
import Data.Function (on)
import Data.List (minimumBy, delete)
import Control.Monad (guard)
import Prelude hiding (Word) 

type Word = String
type Path = [String]

wordF :: [Word] -> Word -> Word -> Path
wordF words start end = 
    start : minimumBy (compare `on` length) (generatePaths words start end)

-- Use the list monad to do the nondeterminism and backtracking.
-- Returns a list of all paths that lead from `start` to `end` 
-- in steps that `differByOne`.
generatePaths :: [Word] -> Word -> Word -> [Path]
generatePaths words start end = do
  -- Choose one of the words, nondeterministically
  word <- words

  -- If the word doesn't `differByOne` from `start`, reject the choice
  -- and backtrack.
  guard $ differsByOne word start

  if word == end
  then return [word]
  else do 
        next <- generatePaths (delete word words) word end
        return $ word : next

differsByOne :: Word -> Word -> Bool
differsByOne "" "" = False
differsByOne (a:as) (b:bs) 
    | a == b = differsByOne as bs
    | otherwise = as == bs

module LSystems where

import Protolude hiding (Symbol, iterate)
import Turtle
import Data.Map.Strict ((!?), Map)
import qualified Data.Map.Strict as M

class LSymbol a where
  interpretSymbol :: (MonadTurtleState m) => a -> m ()

iterate :: (a -> a) -> Int -> a -> a
iterate _ 0 x = x
iterate f n x = iterate f (n-1) (f x)

runLSystem :: (MonadTurtleState m, LSymbol a) => (a -> [a]) -> Int -> [a] -> m ()
runLSystem rules n init = forM_ (iterate (>>= rules) n init) $ interpretSymbol

makeRules :: (Ord a) => Map a [a] -> a -> [a]
makeRules m sym = case m !? sym of
  (Just syms) -> syms
  Nothing -> [sym]

data DragonSymbol =
    F1
  | FR
  | Plus
  | Minus
  deriving (Eq, Ord)

instance LSymbol DragonSymbol where
  interpretSymbol s = case s of
    F1 -> moveF 3
    FR -> moveF 3
    Plus -> turn (Left ())
    Minus -> turn (Right ())

dragonRules :: DragonSymbol -> [DragonSymbol]
dragonRules = makeRules $ M.fromList $ [
    (F1, [F1, Plus, FR, Plus])
  , (FR, [Minus, F1, Minus, FR]) ]

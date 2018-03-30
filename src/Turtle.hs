module Turtle where

import Prelude (error)
import Protolude hiding (zero)
import Data.Int (Int8)
import Linear.Vector
import Linear.V2
import Linear.V3
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.MArray
import Control.Monad.Trans.Writer
import Linear.Matrix
import Control.Lens

type BlockID = Int8

type BlockData = UArray (V3 Int) BlockID

class (Monad m) => MonadBlockWriter m where
  writeBlock :: BlockID -> V3 Int -> m ()

newtype BlockWriterT m a = BlockWriterT {
  unBlockWriter :: WriterT [(V3 Int, BlockID)] m a
} deriving (Functor, Applicative, Monad)

type BlockWriter = BlockWriterT Identity

instance (Monad m) => MonadBlockWriter (BlockWriterT m) where
  writeBlock bid idx = BlockWriterT $ tell [(idx, bid)]

expandRange :: (Applicative f, Ord a) => V2 (f a) -> f a -> V2 (f a)
expandRange p b = liftA2 <$> V2 min max <*> pure b <*> p

runBlockWriterT :: (Monad m) => BlockWriterT m a -> m (a, BlockData)
runBlockWriterT action = do
  (res, w) <- runWriterT $ unBlockWriter action
  let (V2 min max) = foldl expandRange (V2 zero zero) (fmap fst w)
  let arr = runSTUArray $ do
        marr <- newArray (min, max) 0
        forM_ w $ \(p, bid) -> writeArray marr p bid
        return marr
  return (res, arr)

class (Monad m) => MonadTurtleState m where
  push :: m ()
  pop :: m ()
  turn :: (Either () ()) -> m ()
  moveF :: Int -> m ()
  moveG :: Int -> m ()

data TState = TState { _pos :: V3 Int, _orientation :: V3 Int }

makeLenses ''TState

newtype TurtleStateT m a = TurtleStateT {
  runTurtleStateT :: StateT (TState, [TState]) m a
} deriving (Functor, Applicative, Monad)

rightTurnM :: M33 Int
rightTurnM = V3 (V3 0 (-1) 0) (V3 1 0 0) (V3 0 0 1)

leftTurnM :: M33 Int
leftTurnM = V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)

defBlockId :: BlockID
defBlockId = 17

instance (Monad m, MonadBlockWriter m) => MonadTurtleState (TurtleStateT m) where
  push = TurtleStateT $ modify $ \(x, xs) -> (x, x:xs)
  pop = TurtleStateT $ modify $ \(x, xs) -> case xs of
    (y:ys) -> (y, ys)
    [] -> error "popped an empty state"
  turn d = TurtleStateT $ modify $ \s -> over (_1 . orientation) (mat !*) s
    where mat = either (const leftTurnM) (const rightTurnM) d
  moveF n = TurtleStateT $ do
    dir <- use $ _1 . orientation
    forM_ [1..n] $ \_ -> do
      p <- use $ _1 . pos
      lift $ writeBlock defBlockId p
      _1 . pos %= (^+^ dir)
  moveG n = TurtleStateT $ do
    dir <- use $ _1 . orientation
    _1 . pos %= (^+^ (n *^ dir))

drawTurtle :: TurtleStateT BlockWriter () -> BlockData
drawTurtle action = view _2 $ runIdentity $
  runBlockWriterT $ execStateT (runTurtleStateT action) (TState (V3 0 0 0) (V3 1 0 0), [])

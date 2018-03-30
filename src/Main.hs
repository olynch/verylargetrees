{-# LANGUAGE BangPatterns #-}

module Main where

import Protolude
import Turtle
import LSystems
import MCSchematic
import Linear.V3
import Data.Array.IArray
import Prelude (putChar)
import Linear.Vector

bidToChar :: BlockID -> Char
bidToChar i
  | i == 0 = ' '
  | i == 17 = '#'
  | otherwise = '%'

main :: IO ()
main = do
  let !arr = drawTurtle (runLSystem dragonRules 12 [F1])
  let (V3 mx my mz, V3 mX mY mZ) = bounds arr
  print (bounds arr)
  let (V3 w h l) = (+1) <$> fromIntegral <$> (uncurry $ flip (^-^)) (bounds arr)
  print (V3 w h l)
  forM_ [my..mY] $ \y -> do
    forM_ [mx..mX] $ \x -> do
      putChar $ bidToChar $ arr ! (V3 x y 0)
    putChar '\n'
  writeSchematic arr "dragon.schematic"

{-# LANGUAGE TemplateHaskell #-}

module Game.Robo.Core.Collision
       ( entityGrid
       , findCollisions
       , PointEntity, pointEntity
       , EntityGrid
       ) where

import           Game.Robo.Core.Types.Maths
import           Game.Robo.Maths

import           Data.Vector                (Vector)
import qualified Data.Vector                as V

import           Control.Monad              (guard)
import           Data.Bifunctor             (first)

import           Lens.Micro.Platform

-- | Wrapper type to allow access to an object's position.
data PointEntity a =
  PE
  { _pePos :: !Vec
  , _peVal :: !a
  }

-- | Create a 'PointEntity' from an object and a function to get its position.
pointEntity :: (a -> Vec) -> a -> PointEntity a
pointEntity f x = PE (f x) x

data EntityGrid a =
  PM
  { _pmNumCells :: !(Int, Int)
  , _pmCellSize :: !Vec
  , _pmEntities :: !(Vector (Vector (PointEntity a)))
  }
  deriving (Show)

instance Show a => Show (PointEntity a) where
  show (PE _ v) = show v

makeLenses ''EntityGrid

peVal :: Lens' (PointEntity a)  a
peVal f (PE g v) = fmap (PE g) (f v)

pePos :: PointEntity a -> Vec
pePos (PE p _) = p

-- | Finds entities in the given 'EntityGrid' which are inside the given 'Rect'.
findCollisions :: Rect -> EntityGrid a -> Vector a
findCollisions rect pts =
  let corners = rectCorners rect
      numCells@(cellsAcross, _) = pts^.pmNumCells
      cellSize = pts^.pmCellSize
      entities = pts^.pmEntities

      cornerCells = pointToCell cellSize <$> V.fromList corners
      relevantCells = fillCellGaps numCells cornerCells

      relevantIndices = fmap (cellToIndex cellsAcross) relevantCells

      relevantTargets = relevantIndices >>= (entities V.!)
  in naiveFindCollisions rect relevantTargets

-- | Constructs an 'EntityGrid' given a minimum cell size, the world size, and a
-- vector of all the entities.
entityGrid :: Scalar -> Vec -> Vector (PointEntity a) -> EntityGrid a
entityGrid minSize (Vec worldWidth worldHeight) entities =
  let divisor = 20
      cellWidth = max minSize (worldWidth / divisor)
      cellHeight = max minSize (worldHeight / divisor)
      cellSize = Vec cellWidth cellHeight

      cellsAcross = ceiling (worldWidth / cellWidth)
      cellsDown   = ceiling (worldHeight / cellHeight)

      pairWith f x = (f x, x)

      entitiesWithPoints = fmap (pairWith (pointToCell cellSize . pePos)) entities

      emptyGrid = V.replicate (cellsAcross * cellsDown) mempty

      entitiesWithIndices =
        first (cellToIndex cellsAcross) <$>
        entitiesWithPoints

      entitiesOnGrid =
        V.fromList <$>
        V.accumulate (flip (:)) emptyGrid entitiesWithIndices

  in PM (cellsAcross, cellsDown) cellSize entitiesOnGrid

-- | Naively finds each entity in the given 'Vector' whose position is within the given 'Rect'.
naiveFindCollisions :: Rect -> Vector (PointEntity a) -> Vector a
naiveFindCollisions rect = fmap (view peVal) . V.filter (withinRect rect . pePos)

-- | Takes a list of points in a grid and adds on those points necessary to fill
-- in the holes and complete a rectangle. Also makes sure not to include any
-- points which are outside the bounds.
fillCellGaps :: (Int, Int) -> Vector (Int, Int) -> Vector (Int, Int)
fillCellGaps (cellsAcross, cellsDown) =
  do xs <- fmap fst
     ys <- fmap snd

     let allXs = V.fromList [minimum xs .. maximum xs]
         allYs = V.fromList [minimum ys .. maximum ys]

     return $ do
       x <- allXs
       guard (x >= 0 && x < cellsAcross)
       y <- allYs
       guard (y >= 0 && y < cellsDown)
       return (x, y)

cellToIndex :: Int -> (Int, Int) -> Int
cellToIndex cellsAcross (x,  y) =
  y * cellsAcross + x

pointToCell :: Vec -> Vec -> (Int, Int)
pointToCell (Vec cellWidth cellHeight) (Vec x y) =
  (x / cellWidth, y / cellHeight) & over each floor

-- testMinSize = 0.1

-- testWorldSize = Vec 1.0 1.0

-- testRect :: Rect
-- testRect = Rect (Vec 0.1 0.10) (Vec 0.07 0.07) 0

-- testEntities :: Vector (PointEntity Vec)
-- testEntities = V.fromList . fmap (PE id) $ [Vec 0 0, Vec 0.11 0.12, Vec 0.1 0.1, Vec 0.3 0.4, Vec 0.6 0.2]

-- testDivision = entityGrid testMinSize testWorldSize testEntities

-- test = findCollisions testRect testDivision

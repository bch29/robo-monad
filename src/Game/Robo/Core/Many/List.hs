module Game.Robo.Core.Many.List where

import           Control.Monad (replicateM, zipWithM, zipWithM_)
import           Data.List     (partition)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Control.Parallel.Strategies

type Many = []

manyFromList :: [a] -> Many a
manyFromList = id

manyToList :: Many a -> [a]
manyToList = id

filterMany :: (a -> Bool) -> Many a -> Many a
filterMany = filter

listToVector :: Many a -> Vector a
listToVector = V.fromList

listFromVector :: Vector a -> Many a
listFromVector = V.toList

unzipMany :: Many (a, b) -> (Many a, Many b)
unzipMany = unzip

partitionMany :: (a -> Bool) -> Many a -> (Many a, Many a)
partitionMany = partition

imapMany :: (Enum a, Num a) => (a -> b -> c) -> Many b -> Many c
imapMany f = zipWith f [0..]

zipManyWith :: (a -> b -> c) -> Many a -> Many b -> Many c
zipManyWith = zipWith

zipMany :: Many a -> Many b -> Many (a, b)
zipMany = zip

zipMany3 :: Many a -> Many b -> Many c -> Many (a, b, c)
zipMany3 = zip3

mapManyM :: Monad m => (a -> m b) -> Many a -> m (Many b)
mapManyM = mapM

replicateManyM :: Monad m => Int -> m a -> m (Many a)
replicateManyM = replicateM

zipWithManyM :: Monad m => (a -> b -> m c) -> Many a -> Many b -> m (Many c)
zipWithManyM = zipWithM

zipWithManyM_ :: Monad m => (a -> b -> m c) -> Many a -> Many b -> m ()
zipWithManyM_ = zipWithM_

parMany :: NFData a => Strategy (Many a)
parMany = parList rpar

module Game.Robo.Core.Many.Vector where

import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V

type Many = Vector

manyFromList :: [a] -> Many a
manyFromList = V.fromList

manyToList :: Many a -> [a]
manyToList = V.toList

filterMany :: (a -> Bool) -> Many a -> Many a
filterMany = V.filter

listToVector :: Many a -> Vector a
listToVector = id

listFromVector :: Vector a -> Many a
listFromVector = id

unzipMany :: Many (a, b) -> (Many a, Many b)
unzipMany = V.unzip

partitionMany :: (a -> Bool) -> Many a -> (Many a, Many a)
partitionMany = V.partition

imapMany :: (Int -> b -> c) -> Many b -> Many c
imapMany = V.imap

zipManyWith :: (a -> b -> c) -> Many a -> Many b -> Many c
zipManyWith = V.zipWith

zipMany :: Many a -> Many b -> Many (a, b)
zipMany = V.zip

zipMany3 :: Many a -> Many b -> Many c -> Many (a, b, c)
zipMany3 = V.zip3

mapManyM :: Monad m => (a -> m b) -> Many a -> m (Many b)
mapManyM = V.mapM

replicateManyM :: Monad m => Int -> m a -> m (Many a)
replicateManyM = V.replicateM

zipWithManyM :: Monad m => (a -> b -> m c) -> Many a -> Many b -> m (Many c)
zipWithManyM = V.zipWithM

zipWithManyM_ :: Monad m => (a -> b -> m c) -> Many a -> Many b -> m ()
zipWithManyM_ = V.zipWithM_

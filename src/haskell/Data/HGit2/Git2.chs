module Data.HGit2.Git2 where

import Foreign

type CPtr = Ptr ()

class CWrapper a where
  unwrap :: a -> CPtr

wrapToMNum :: (CWrapper a, Num b, Monad m, Integral c) => (CPtr -> m c) -> a
           -> m b
wrapToMNum f = (return . fromIntegral =<<) . f . unwrap

flipUSCall :: CWrapper a => (a1 -> IO c) -> (CPtr -> IO a1) -> a -> c
flipUSCall f = flip usCall (f =<<)

usCall :: CWrapper a => (CPtr -> b) -> (b -> IO c) -> a -> c
usCall f g = unsafePerformIO . g . f . unwrap

retRes :: CWrapper a => (CPtr -> a) -> CPtr -> IO (Maybe a)
retRes w = return . retRes'
  where retRes' res | res == nullPtr = Nothing
                    | otherwise      = Just $ w res

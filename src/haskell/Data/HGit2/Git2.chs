module Data.HGit2.Git2 where

import Data.HGit2.Errors
import Foreign.C.String
import Foreign.C.Types
import Foreign

type CPtr = Ptr ()

type IOEitherErr a = IO (Either GitError a)
type IOCanFail     = IO (Maybe GitError)

newtype RawData = RawData CPtr

class CWrapper a where
  unwrap :: a -> CPtr

instance CWrapper RawData where
  unwrap (RawData r) = r

wrapToMNum :: (CWrapper a, Num b, Monad m, Integral c) => (CPtr -> m c) -> a
           -> m b
wrapToMNum f = retNum . f . unwrap

flipUSCall :: CWrapper a => (b -> IO c) -> (CPtr -> IO b) -> a -> c
flipUSCall f = flip usCall (f =<<)

usCall :: CWrapper a => (CPtr -> b) -> (b -> IO c) -> a -> c
usCall f g = unsafePerformIO . g . f . unwrap

retRes :: (CPtr -> a) -> CPtr -> IO (Maybe a)
retRes w = return . retRes'
  where retRes' res | res == nullPtr = Nothing
                    | otherwise      = Just $ w res

retEither :: CInt -> IOEitherErr a -> IOEitherErr a
retEither res f | res == 0  = f
                | otherwise = return . Left . toEnum . fromIntegral $ res

retMaybe :: CInt -> IOCanFail
retMaybe = return . rm
  where rm res | res == 0  = Nothing
               | otherwise = Just . toEnum . fromIntegral $ res

eitherPeek :: Storable b => Ptr b -> (b -> a) -> CInt -> IOEitherErr a
eitherPeek ptr = eitherCon (peek ptr)

eitherPeekStr :: CString -> (String -> a) -> CInt -> IOEitherErr a
eitherPeekStr ptr = eitherCon (peekCString ptr)

eitherCon :: IO b -> (b -> a) -> CInt -> IOEitherErr a
eitherCon rght con res = retEither res $ fmap (Right . con) $ rght

retNum :: (Num b, Monad m, Integral a) => m a -> m b
retNum a = return . fromIntegral =<< a

retEnum :: (Monad m, Integral a, Enum b) => m a -> m b
retEnum a = return . toEnum . fromIntegral =<< a

callRetCons :: (CWrapper a, Monad m) => (CPtr -> m c) -> (c -> b) -> a -> m b
callRetCons call cons = (return . cons =<<) . call . unwrap

callRetNum :: (CWrapper a, Num b, Monad m, Integral c) => (CPtr -> m c) -> a
           -> m b
callRetNum call = retNum . call . unwrap

callRetMaybe :: CWrapper a => (CPtr -> IO CInt) -> a -> IOCanFail
callRetMaybe call = (retMaybe =<<) . call . unwrap

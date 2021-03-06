module Data.HGit2.Git2 where

import Data.HGit2.Errors
import Foreign.C.String
import Foreign.C.Types
import Foreign

type CPtr = ForeignPtr ()

type IOEitherErr a = IO (Either GitError a)
-- TODO: Rename this: sometimes GitError is returned just to indicate a status.
-- see createTag
type IOCanFail     = IO (Maybe GitError)

newtype RawData = RawData (Ptr ())

class CWrapper a where
  unwrap :: a -> CPtr

{- wrapToMNum :: (CWrapper a, Num b, Monad m, Integral c) => (CPtr -> m c) -> a-}
           {- -> m b-}
{- wrapToMNum f = retNum . f . unwrap-}

{- flipUSCall :: CWrapper a => (b -> IO c) -> (CPtr -> IO b) -> a -> c-}
{- flipUSCall f = flip usCall (f =<<)-}

{- usCall :: CWrapper a => (CPtr -> b) -> (b -> IO c) -> a -> c-}
{- usCall f g = unsafePerformIO . g . f . unwrap-}

retRes :: (CPtr -> a) -> CPtr -> IO (Maybe a)
retRes con fp = withForeignPtr fp $ return . ret
  where ret res | res == nullPtr = Nothing
                | otherwise      = Just $ con fp

retRes' :: (Ptr b -> a) -> IO (ForeignPtr b) -> IO (Maybe a)
retRes' con ptr = do
  fp <- ptr
  withForeignPtr fp $ return . retRes''
  where retRes'' res | res == nullPtr = Nothing
                     | otherwise      = Just $ con res

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

{- callRetCons :: (CWrapper a, Monad m) => (CPtr -> m c) -> (c -> b) -> a -> m b-}
{- callRetCons call cons = (return . cons =<<) . call . unwrap-}

{- callRetNum :: (CWrapper a, Num b, Monad m, Integral c) => (CPtr -> m c) -> a-}
           {- -> m b-}
{- callRetNum call = retNum . call . unwrap-}

{- callRetMaybe :: CWrapper a => (CPtr -> IO CInt) -> a -> IOCanFail-}
{- callRetMaybe call = (retMaybe =<<) . call . unwrap-}

wrpToCstr :: CWrapper a => (CPtr -> b) -> (Ptr () -> IO (Ptr ())) -> a -> IO b
wrpToCstr cstr fn wrp =
  withForeignPtr (unwrap wrp) $ \pt ->
  fmap cstr $ mkFPtr =<< fn pt

wrpToBool :: (CWrapper a, Integral n) => (Ptr () -> IO n) -> a -> IO Bool
wrpToBool fn wrp =
  withForeignPtr (unwrap wrp) $ \pt ->
  return . toBool =<< fn pt

wrpToStr :: CWrapper a => (Ptr () -> IO CString) -> a -> IO String
wrpToStr fn wrp =
  withForeignPtr (unwrap wrp) $ \pt ->
  peekCString =<< fn pt

wrpToInt :: (CWrapper a, Integral n) => (Ptr () -> IO n) -> a -> IO Int
wrpToInt fn wrp =
  withForeignPtr (unwrap wrp) $ \pt ->
  return . fromIntegral =<< fn pt

wrpToUnit :: CWrapper a => (Ptr () -> IO ()) -> a -> IO ()
wrpToUnit fn wrp =
  withForeignPtr (unwrap wrp) $ \pt ->
  fn pt

callPeek :: (ForeignPtr a1 -> a) -> (Ptr (Ptr a1) -> IO CInt)
         -> IO (Either GitError a)
callPeek con call = alloca $ \out -> eitherPeek' out con =<< call out

eitherPeek' :: Ptr (Ptr a1) -> (ForeignPtr a1 -> a) -> CInt -> IOEitherErr a
eitherPeek' ptr = eitherCon (newForeignPtr finalizerFree =<< peek ptr)

mkFPtr :: Ptr a -> IO (ForeignPtr a)
mkFPtr = newForeignPtr finalizerFree

unEnum :: (Enum a, Integral i) => a -> i
unEnum = fromIntegral . fromEnum


type CUString = Ptr CUChar

withCUString :: String -> (CUString -> IO a) -> IO a
withCUString = withArray0 uNUL . charsToCUChars
  where charsToCUChars :: [Char] -> [CUChar]
        charsToCUChars xs = map castCharToCUChar xs
        uNUL :: CUChar
        uNUL = 0

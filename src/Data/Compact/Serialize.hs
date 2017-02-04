{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Compact.Serialize
    ( writeFile, readFile
    , hPutCompact, hGetCompact
    ) where

import Control.Monad (replicateM)
import Data.Monoid
import Data.IORef
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import Foreign.Marshal.Utils (copyBytes)
import Type.Reflection
import System.IO (Handle, withFile, IOMode(..))
import Data.Word
import GHC.Ptr (Ptr(..), plusPtr)
import Foreign.Ptr
import Data.Compact
import Data.Compact.Serialized
import Prelude hiding (readFile, writeFile)

data CompactFile a = CompactFile (SerializedCompact a)

instance B.Binary SomeTypeRep where
    get = return $ SomeTypeRep $ typeRep @()
    put _ = return ()

instance (Typeable a) => B.Binary (CompactFile a) where
    get = do
        SomeTypeRep tyrep <- B.get
        case tyrep `eqTypeRep` typeRep @a of
          Just HRefl -> CompactFile <$> getSerializedCompact
    put (CompactFile a) = putSerializedCompact a

putPtr :: Ptr a -> B.Put
putPtr = B.put @Word64 . fromIntegral . ptrToWordPtr

getPtr :: B.Get (Ptr a)
getPtr = wordPtrToPtr . fromIntegral <$> B.get @Word64

getList :: B.Get a -> B.Get [a]
getList getElem = do
    n <- B.get @Int
    replicateM n getElem

putList :: (a -> B.Put) -> [a] -> B.Put
putList putElem xs = do
    B.put @Int (length xs)
    mapM_ putElem xs

getSerializedCompact :: B.Get (SerializedCompact a)
getSerializedCompact = SerializedCompact <$> getList getBlock <*> getPtr
  where
    getBlock :: B.Get (Ptr a, Word)
    getBlock = (,) <$> getPtr <*> B.get

putSerializedCompact :: SerializedCompact a -> B.Put
putSerializedCompact (SerializedCompact a b) = putList putBlock a <> putPtr b
  where
    putBlock :: (Ptr a, Word) -> B.Put
    putBlock (ptr, len) = putPtr ptr <> B.put len


writeFile :: Typeable a => FilePath -> Compact a -> IO ()
writeFile fname compact =
    withFile fname WriteMode $ \hdl -> hPutCompact hdl compact

hPutCompact :: Typeable a => Handle -> Compact a -> IO ()
hPutCompact hdl compact =
    withSerializedCompact compact $ \scompact -> do
        BSL.hPutStr hdl $ B.encode $ CompactFile scompact
        let putBlock (Ptr addr#, len) = do
                bs <- BSU.unsafePackAddressLen (fromIntegral len) addr#
                BS.hPutStr hdl bs
        mapM_ putBlock (serializedCompactBlockList scompact)

readFile :: Typeable a => FilePath -> IO (Either String (Compact a))
readFile fname =
    withFile fname ReadMode $ \hdl -> hGetCompact hdl

hGetCompact :: forall a. Typeable a
            => Handle -> IO (Either String (Compact a))
hGetCompact hdl = do
    mbs <- BSL.hGetContents hdl
    case B.decodeOrFail @(CompactFile a) mbs of
      Left (rest, _, err) -> return $ Left err
      Right (rest, _, CompactFile scompact) -> do
          input <- newIORef rest
          res <- importCompact scompact $ \ptr len -> do
              bs <- readIORef input
              bs' <- copyTo bs ptr (fromIntegral len)
              writeIORef input bs'
          case res of
            Nothing -> return $ Left "Failed to import compact"
            Just compact -> return $ Right compact

-- | @copyTo src dest len@ copies @len@ bytes from 'BSL.ByteString' @src@ to
-- buffer pointed to be @dest@.
copyTo :: BSL.ByteString -> Ptr a -> Int -> IO BSL.ByteString
copyTo = \src -> go (BSL.toChunks src)
  where
    go :: [BS.ByteString] -> Ptr a -> Int -> IO BSL.ByteString
    go _ _ n | n < 0 = error "copyTo: negative length"
    go srcs _ 0 = return $ BSL.fromChunks srcs
    go (src:rest) dest len | BS.null src = go rest dest len
    go (src:rest) dest len = do
        let srcLen = BS.length src
            copyLen = min srcLen len
        BSU.unsafeUseAsCStringLen src $ \(srcPtr, srcLen) -> do
            copyBytes dest (castPtr srcPtr) copyLen
        go (BS.drop copyLen src : rest) (dest `plusPtr` copyLen) (len - copyLen)
    go [] _ len = error "copyTo: Ran out of input"

{-
mmapCompact :: forall a. Typeable a => FilePath -> IO (Either String (Compact a))
mmapCompact fname = undefined

foreign import ccall unsafe "mmap.h"
-}

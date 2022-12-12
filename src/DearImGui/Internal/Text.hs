{-# LANGUAGE CPP #-}

module DearImGui.Internal.Text
  ( withCString
  , withCStringOrNull
  , withCStringLen
  , withCStringEnd
  , peekCString

  , Text
  , pack
  , unpack
  ) where

-- base
import Control.Monad.IO.Class (liftIO)
import Foreign (nullPtr, plusPtr)
import Foreign.C.String (CString)
import qualified GHC.Foreign as Foreign
import System.IO (utf8)

-- text
import Data.Text (Text, pack, unpack)
import Data.Text.Foreign (withCStringLen)

-- unliftio-core
import UnliftIO (MonadUnliftIO, UnliftIO(..), withUnliftIO)

#if MIN_VERSION_text(2,0,1)
-- XXX: just wrap the provided combinator

import qualified Data.Text.Foreign as Text

withCString :: MonadUnliftIO m => Text -> (CString -> m a) -> m a
withCString text action =
  withUnliftIO $ \(UnliftIO unlift) ->
    Text.withCString text (unlift action)

#elif MIN_VERSION_text(2,0,0)
-- XXX: the text is UTF-8, alas no withCString is available

import Data.Text.Foreign (lengthWord8, unsafeCopyToPtr)
import Data.Word (Word8)
import Foreign (allocaBytes, castPtr, pokeByteOff)

withCString :: MonadUnliftIO m => Text -> (CString -> m a) -> m a
withCString t@(Text _arr _off len) action =
  withUnliftIO $ \(UnliftIO unlift) ->
    allocaBytes (len + 1) $ \buf -> do
      unsafeCopyToPtr t buf
      pokeByteOff buf len (0 :: Word8)
      unlift $ action (castPtr buf)

#else
-- XXX: the text is UTF-16, let GHC do it

withCString :: MonadUnliftIO m => Text -> (CString -> m a) -> m a
withCString t action = do
  withUnliftIO $ \(UnliftIO unlift) ->
    liftIO $
      Foreign.withCString utf8 (unpack t) $ \textPtr ->
        unlift $ action textPtr

#endif

peekCString :: CString -> IO Text
peekCString = fmap pack . Foreign.peekCString utf8

withCStringOrNull :: Maybe Text -> (CString -> IO a) -> IO a
withCStringOrNull Nothing k  = k nullPtr
withCStringOrNull (Just s) k = withCString s k

withCStringEnd :: MonadUnliftIO m => Text -> (CString -> CString -> m a) -> m a
withCStringEnd t action =
  withUnliftIO $ \(UnliftIO unlift) ->
    withCStringLen t $ \(textPtr, size) ->
      unlift $ action textPtr (textPtr `plusPtr` size)

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module DearImGui.Raw.DragDrop
  ( -- * Source
    beginSource
  , setPayload
  , endSource
    -- * Target
  , beginTarget
  , acceptPayload
  , endTarget
    -- * Payload object
  , Payload(..)
  , getData
  , getDataSize
    -- ** Direct access
  , getPayload
  , clear
  , isDataType
  , isPreview
  , isDelivery
  )
  where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign
  ( Ptr, castPtr, nullPtr )
import Foreign.C

-- dear-imgui
import DearImGui.Raw.Context
  ( imguiContext )
import DearImGui.Enums
import DearImGui.Structs

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"

-- | Call after submitting an item which may be dragged.
--
-- When this return True, you can call 'setPayload' + 'endDragDropSource'.
beginSource :: MonadIO m => ImGuiDragDropFlags -> m Bool
beginSource flags = liftIO do
  (0 /=) <$> [C.exp| bool { BeginDragDropSource( $(ImGuiDragDropFlags flags) ) } |]

-- | Type is a user defined string of maximum 32 characters.
--
-- Strings starting with '_' are reserved for dear imgui internal types.
-- Data is copied and held by imgui.
-- Returns True when payload has been accepted.
setPayload :: MonadIO m => CString -> Ptr a -> CSize -> ImGuiCond -> m Bool
setPayload typePtr dataPtr sz cond = liftIO do
  (0 /=) <$> [C.exp| bool { SetDragDropPayload( $(char* typePtr), $(void* dataPtr'), $(size_t sz), $(ImGuiCond cond) ) } |]
  where
    dataPtr' = castPtr dataPtr

-- | Only call if 'beginSource' returns True!
endSource :: MonadIO m => m ()
endSource = liftIO do
  [C.block| void { EndDragDropSource( ); } |]

-- | Call after submitting an item that may receive a payload.
--
-- If this returns True, you can call 'acceptPayload' + 'endTarget'.
beginTarget :: MonadIO m => m Bool
beginTarget = liftIO do
  (0 /=) <$> [C.exp| bool { BeginDragDropTarget() } |]

-- | Accept contents of a given type.
--
-- If "ImGuiDragDropFlags_AcceptBeforeDelivery" is set you can peek into the payload before the mouse button is released.
acceptPayload :: MonadIO m => CString -> ImGuiDragDropFlags -> m (Maybe Payload)
acceptPayload typePtr flags = liftIO do
  ptr <- [C.exp| const ImGuiPayload* { AcceptDragDropPayload( $(char* typePtr), $(ImGuiDragDropFlags flags) ) } |]
  if ptr == nullPtr then
    pure Nothing
  else
    pure $ Just (Payload ptr)

-- | Only call if 'beginTarget' returns true!
endTarget :: MonadIO m => m ()
endTarget = liftIO do
  [C.block| void { EndDragDropTarget(); } |]

-- | Peek directly into the current payload from anywhere.
--
-- Returns NULL when drag and drop is finished or inactive.
-- Use 'isDataType' to test for the payload type.
getPayload :: MonadIO m => m (Maybe Payload)
getPayload = liftIO do
  ptr <- [C.exp| const ImGuiPayload* { GetDragDropPayload() } |]
  if ptr == nullPtr then
    pure Nothing
  else
    pure $ Just (Payload ptr)

-- | DragDrop payload data handle
--
-- Wraps @ImGuiPayload*@.
newtype Payload = Payload (Ptr ImGuiPayload)
  deriving (Eq, Show)

getData :: MonadIO m => Payload -> m (Ptr ())
getData (Payload payloadPtr) = liftIO do
  [C.exp| void* { $(ImGuiPayload* payloadPtr)->Data } |]

getDataSize :: MonadIO m => Payload -> m CInt
getDataSize (Payload payloadPtr) = liftIO do
  [C.exp| int { $(ImGuiPayload* payloadPtr)->DataSize } |]

-- | Clear the DearImGui copy of payload data.
--
-- Gets called on 'endTarget' right after delivery.
clear :: MonadIO m => Payload -> m ()
clear (Payload payloadPtr) = liftIO do
  [C.block| void { $(ImGuiPayload* payloadPtr)->Clear(); } |]

isDataType :: MonadIO m => Payload -> CString -> m Bool
isDataType (Payload payloadPtr) typePtr = liftIO do
  (0 /=) <$> [C.exp| bool { $(ImGuiPayload* payloadPtr)->IsDataType($(char* typePtr)) } |]

isPreview :: MonadIO m => Payload -> m Bool
isPreview (Payload payloadPtr) = liftIO do
  (0 /=) <$> [C.exp| bool { $(ImGuiPayload* payloadPtr)->IsPreview() } |]

isDelivery :: MonadIO m => Payload -> m Bool
isDelivery (Payload payloadPtr) = liftIO do
  (0 /=) <$> [C.exp| bool { $(ImGuiPayload* payloadPtr)->IsDelivery() } |]

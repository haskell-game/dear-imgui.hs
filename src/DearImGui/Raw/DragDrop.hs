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
  ( Payload(..)
  , beginSource
  , setPayload
  , endSource
  , beginTarget
  , acceptPayload
  , endTarget
  , getPayload
  )
  where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign
  ( Ptr, castPtr )
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

-- | Font configuration data handle
--
-- Wraps @ImGuiPayload*@.
newtype Payload = Payload (Ptr ImGuiPayload)


beginSource :: MonadIO m => ImGuiDragDropFlags -> m Bool
beginSource flags = liftIO do
  (0 /=) <$> [C.exp| bool { BeginDragDropSource( $(ImGuiDragDropFlags flags) ) } |]

setPayload :: MonadIO m => CString -> Ptr a -> CSize -> ImGuiCond -> m Bool
setPayload typePtr dataPtr sz cond = liftIO do
  (0 /=) <$> [C.exp| bool { SetDragDropPayload( $(char* typePtr), $(void* dataPtr'), $(size_t sz), $(ImGuiCond cond) ) } |]
  where
    dataPtr' = castPtr dataPtr

endSource :: MonadIO m => m ()
endSource = liftIO do
  [C.block| void { EndDragDropSource( ); } |]

beginTarget :: MonadIO m => m ()
beginTarget = liftIO do
  [C.block| void { BeginDragDropTarget(); } |]

acceptPayload :: MonadIO m => CString -> ImGuiDragDropFlags -> m Payload
acceptPayload typePtr flags = liftIO do
  Payload <$> [C.exp| const ImGuiPayload* { AcceptDragDropPayload( $(char* typePtr), $(ImGuiDragDropFlags flags) ) } |]

endTarget :: MonadIO m => m ()
endTarget = liftIO do
  [C.block| void { EndDragDropTarget(); } |]

getPayload :: MonadIO m => m Payload
getPayload = liftIO do
  Payload <$> [C.exp| const ImGuiPayload* { GetDragDropPayload() } |]

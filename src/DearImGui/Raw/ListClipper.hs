{-# OPTIONS_GHC -Wwarn #-}

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
{-|
Module: ImGuiListClipper


Main module to export ListClipper
-}

module DearImGui.Raw.ListClipper
  ( ListClipper()
    , new
    , delete
    , begin
    , displayStart
    , displayEnd
    , step
  )
    where

import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign hiding (new)
import Foreign.C

-- dear-imgui
import DearImGui.Context
  ( imguiContext )
import DearImGui.Structs

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"


-- | Wraps @ImGuiListClipper
type ListClipper = Ptr ImGuiListClipper


-- | Wraps a constructor for @ImGuiListClipper
-- 
-- Listclipper has manually by destroiyListClipper
new :: (MonadIO m) => m ListClipper
new = liftIO $ do             
--  ListClipper <$> [C.block| ImGuiListClipper* { 
  [C.exp| ImGuiListClipper* { IM_NEW(ImGuiListClipper) } |]


-- | Wraps a destructor for @ImGuiListClipper
--
-- | Deletes @ImGuiListClipper
delete :: (MonadIO m) => ListClipper -> m Int
delete clipper = liftIO do
  fromEnum <$> [Cpp.exp| void { delete $(ImGuiListClipper* clipper) } |]


-- | Wraps @ListClipper::Begin()
-- 
begin :: (MonadIO m) => ListClipper -> CInt -> m ()
begin clipper size= liftIO $ do
    [Cpp.exp| void { $(ImGuiListClipper* clipper)->Begin( $(int size) ) } |]


-- | Wraps @ListClipper::Begin()
displayStart :: (MonadIO m) => ListClipper -> m Int
displayStart clipper = liftIO do
  fromEnum <$> [Cpp.exp| int { ($(ImGuiListClipper* clipper))->DisplayStart } |] 


-- | Wraps @ListClipper::DisplayStart
displayEnd :: (MonadIO m) => ListClipper -> m Int
displayEnd clipper = liftIO do
  fromEnum <$> [Cpp.exp| int { ($(ImGuiListClipper* clipper))->DisplayEnd } |]


-- | Wraps @ListClipper::DisplayEnd
step :: (MonadIO m) => ListClipper -> m Bool
step clipper = liftIO do
  (==) (CBool 1) <$> [Cpp.exp| bool { ($(ImGuiListClipper* clipper))->Step()} |]



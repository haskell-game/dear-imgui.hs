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

{-| Helper: Manually clip large list of items.

If you are submitting lots of evenly spaced items and you have a random access to the list,
you can perform coarse clipping based on visibility to save yourself from processing those items at all.

The clipper calculates the range of visible items and advance the cursor to compensate for the non-visible items we have skipped.

Dear ImGui already clips items based on their bounds but it needs to measure text size to do so,
whereas manual coarse clipping before submission makes this cost and your own data fetching/submission cost almost null.

Usage:

@
clipper <- ListClipper.new
ListClipper.begin clipper 1000 -- We have 1000 elements, evenly spaced.
whileTrue (ListClipper.step clipper) $
  start <- ListClipper.displayStart clipper
  end <- ListClipper.displayEnd clipper
  for_ [start .. end] \ix ->
    ImGui.text $ "line number " <> show ix
@

Generally what happens is:

* Clipper lets you process the first element (DisplayStart = 0, DisplayEnd = 1) regardless of it being visible or not.
* User code submit one element.
* Clipper can measure the height of the first element
* Clipper calculate the actual range of elements to display based on the current clipping rectangle,
  position the cursor before the first visible element.
* User code submit visible elements.
-}

module DearImGui.Raw.ListClipper
  ( ListClipper
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
import System.IO.Unsafe (unsafePerformIO)

-- dear-imgui
import DearImGui.Context
  ( imguiContext )
import DearImGui.Structs
  ( ImGuiListClipper )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
Cpp.using "namespace ImGui"


-- | @ImGuiListClipper@ object handle.
type ListClipper = Ptr ImGuiListClipper


-- | Create a new 'ListClipper' instance.
new :: (MonadIO m) => m ListClipper
new = liftIO do
  [C.block|
    ImGuiListClipper* {
      return IM_NEW(ImGuiListClipper);
    }
  |]

-- | Destroy 'ListClipper' instance.
delete :: (MonadIO m) => ListClipper -> m ()
delete clipper = liftIO do
  [C.block|
    void {
      IM_DELETE($(ImGuiListClipper* clipper));
    }
  |]


-- | ListClipper setup
--
-- @items_count@: Use 'maxBound' if you don't know how many items you have
-- (in which case the cursor won't be advanced in the final step).
--
-- @items_height@: Use -1.0f to be calculated automatically on first step.
-- Otherwise pass in the distance between your items, typically
-- 'getTextLineHeightWithSpacing' or 'getFrameHeightWithSpacing'.
--
-- Wraps @ListClipper::Begin()@.
begin :: (MonadIO m) => ListClipper -> CInt -> CFloat -> m ()
begin clipper items_count items_height = liftIO do
  [C.block|
    void {
      $(ImGuiListClipper* clipper)->Begin($(int items_count), $(float items_height));
    }
  |]

-- | An accessor for @ListClipper::Begin@
displayStart :: ListClipper -> CInt
displayStart clipper = unsafePerformIO do
  [C.exp|
    int {
      $(ImGuiListClipper* clipper)->DisplayStart
    }
  |]

-- | An accessor for @ListClipper::DisplayStart@
displayEnd :: ListClipper -> CInt
displayEnd clipper = unsafePerformIO
  [C.exp|
    int {
      $(ImGuiListClipper* clipper)->DisplayEnd
    }
  |]


-- | Call until it returns 'False'.
--
-- The 'displayStart'/'displayEnd' fields will be set and you can process/draw those items.
--
-- Wraps @ListClipper::Step()@.
step :: (MonadIO m) => ListClipper -> m Bool
step clipper = liftIO do
  (0 /=) <$> [C.block|
    bool {
      return $(ImGuiListClipper* clipper)->Step();
    }
  |]

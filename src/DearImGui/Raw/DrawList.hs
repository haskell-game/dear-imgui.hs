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
Module: DearImGui

Main ImGui module, exporting the functions to create a GUI.
-}

module DearImGui.Raw.DrawList
  ( DrawList(..)
  , new
  , destroy

    -- * Primitives

  , addLine

  , addRect
  , addRectFilled
  , addRectFilledMultiColor

  , addQuad
  , addQuadFilled

  , addTriangle
  , addTriangleFilled

  , addCircle
  , addCircleFilled

  , addNgon
  , addNgonFilled

  , addText_
  , addText

  , addPolyLine
  , addConvexPolyFilled

  , addBezierCubic
  , addBezierQuadratic

    -- ** Image primitives
  , addImage
  , addImageQuad
  , addImageRounded

    -- * Stateful path API

  , pathClear
  , pathLineTo
  , pathLineToMergeDuplicate
  , pathFillConvex
  , pathStroke

  , pathArcTo
  , pathArcToFast

  , pathBezierCubicCurveTo
  , pathBezierQuadraticCurveTo

  , pathRect

    -- * Advanced

  -- , addCallback
  , addDrawCmd
  , cloneOutput

    -- * Internal state

  , pushClipRect
  , pushClipRectFullScreen
  , popClipRect
  , getClipRectMin
  , getClipRectMax

  , pushTextureID
  , popTextureID
  )
  where

import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign hiding (new)
import Foreign.C

-- dear-imgui
import DearImGui.Context
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

newtype DrawList = DrawList (Ptr ImDrawList)

new :: MonadIO m => m DrawList
new = liftIO do
  DrawList <$> [C.block|
    ImDrawList* {
      return IM_NEW(ImDrawList(GetDrawListSharedData()));
    }
  |]

destroy :: MonadIO m => DrawList -> m ()
destroy (DrawList drawList) = liftIO do
  [C.block|
    void {
      IM_DELETE($(ImDrawList* drawList));
    }
  |]


pushClipRect :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> CBool -> m ()
pushClipRect (DrawList drawList) clip_rect_min clip_rect_max intersect_with_current_clip_rect = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PushClipRect(
        *$(ImVec2* clip_rect_min),
        *$(ImVec2* clip_rect_max),
        $(bool intersect_with_current_clip_rect)
      );
    }
  |]

pushClipRectFullScreen :: MonadIO m => DrawList -> m ()
pushClipRectFullScreen (DrawList drawList) = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PushClipRectFullScreen();
    }
  |]

popClipRect :: MonadIO m => DrawList -> m ()
popClipRect (DrawList drawList) = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PopClipRect();
    }
  |]


getClipRectMin :: MonadIO m => DrawList -> m ImVec2
getClipRectMin (DrawList drawList) = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = $(ImDrawList* drawList)->GetClipRectMin();
      }
    |]

getClipRectMax :: MonadIO m => DrawList -> m ImVec2
getClipRectMax (DrawList drawList) = liftIO do
  C.withPtr_ \ptr ->
    [C.block|
      void {
        *$(ImVec2 * ptr) = $(ImDrawList* drawList)->GetClipRectMax();
      }
    |]


pushTextureID :: MonadIO m => DrawList -> Ptr () -> m ()
pushTextureID (DrawList drawList) userTextureIDPtr = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PushTextureID(
        $(void* userTextureIDPtr)
      );
    }
  |]

popTextureID :: MonadIO m => DrawList -> m ()
popTextureID (DrawList drawList) = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PopTextureID();
    }
  |]


addLine :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> CFloat -> m ()
addLine (DrawList drawList) p1 p2 col thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddLine(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        $(ImU32 col),
        $(float thickness)
      );
    }
  |]


addRect :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> CFloat -> ImDrawFlags -> CFloat -> m ()
addRect (DrawList drawList) p_min p_max col rounding flags thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddRect(
        *$(ImVec2* p_min),
        *$(ImVec2* p_max),
        $(ImU32 col),
        $(float rounding),
        $(ImDrawFlags flags),
        $(float thickness)
      );
    }
  |]

addRectFilled :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> CFloat -> ImDrawFlags -> m ()
addRectFilled (DrawList drawList) p_min p_max col rounding flags = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddRectFilled(
        *$(ImVec2* p_min),
        *$(ImVec2* p_max),
        $(ImU32 col),
        $(float rounding),
        $(ImDrawFlags flags)
      );
    }
  |]

addRectFilledMultiColor :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> ImU32 -> ImU32 -> ImU32 -> m ()
addRectFilledMultiColor (DrawList drawList) p_min p_max col_upr_left col_upr_right col_bot_right col_bot_left = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddRectFilledMultiColor(
        *$(ImVec2* p_min),
        *$(ImVec2* p_max),
        $(ImU32 col_upr_left),
        $(ImU32 col_upr_right),
        $(ImU32 col_bot_right),
        $(ImU32 col_bot_left)
      );
    }
  |]


addQuad :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> CFloat -> m ()
addQuad (DrawList drawList) p1 p2 p3 p4 col thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddQuad(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        *$(ImVec2* p4),
        $(ImU32 col),
        $(float thickness)
      );
    }
  |]

addQuadFilled :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> m ()
addQuadFilled (DrawList drawList) p1 p2 p3 p4 col = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddQuadFilled(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        *$(ImVec2* p4),
        $(ImU32 col)
      );
    }
  |]


addTriangle :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> CFloat -> m ()
addTriangle (DrawList drawList) p1 p2 p3 col thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddTriangle(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        $(ImU32 col),
        $(float thickness)
      );
    }
  |]

addTriangleFilled :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> ImU32 -> m ()
addTriangleFilled (DrawList drawList) p1 p2 p3 col = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddTriangleFilled(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        $(ImU32 col)
      );
    }
  |]


addCircle :: MonadIO m => DrawList -> Ptr ImVec2 -> CFloat -> ImU32 -> CInt -> CFloat -> m ()
addCircle (DrawList drawList) center radius col num_segments thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddCircle(
        *$(ImVec2* center),
        $(float radius),
        $(ImU32 col),
        $(int num_segments),
        $(float thickness)
      );
    }
  |]

addCircleFilled :: MonadIO m => DrawList -> Ptr ImVec2 -> CFloat -> ImU32 -> CInt -> m ()
addCircleFilled (DrawList drawList) center radius col num_segments = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddCircleFilled(
        *$(ImVec2* center),
        $(float radius),
        $(ImU32 col),
        $(int num_segments)
      );
    }
  |]


addNgon :: MonadIO m => DrawList -> Ptr ImVec2 -> CFloat -> ImU32 -> CInt -> CFloat -> m ()
addNgon (DrawList drawList) center radius col num_segments thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddNgon(
        *$(ImVec2* center),
        $(float radius),
        $(ImU32 col),
        $(int num_segments),
        $(float thickness)
      );
    }
  |]

addNgonFilled :: MonadIO m => DrawList -> Ptr ImVec2 -> CFloat -> ImU32 -> CInt -> m ()
addNgonFilled (DrawList drawList) center radius col num_segments = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddNgonFilled(
        *$(ImVec2* center),
        $(float radius),
        $(ImU32 col),
        $(int num_segments)
      );
    }
  |]


addText_ :: MonadIO m => DrawList -> Ptr ImVec2 -> ImU32 -> CString -> CString -> m ()
addText_ (DrawList drawList) pos col text_begin text_end = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddText(
        *$(ImVec2* pos),
        $(ImU32 col),
        $(char* text_begin),
        $(char* text_end)
      );
    }
  |]

addText :: MonadIO m => DrawList -> Ptr ImFont -> CFloat -> Ptr ImVec2 -> ImU32 -> CString -> CString -> CFloat -> Ptr ImVec4 -> m ()
addText (DrawList drawList) fontPtr font_size pos col text_begin text_end wrap_width cpu_fine_clip_rect = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddText(
        $(ImFont* fontPtr),
        $(float font_size),
        *$(ImVec2* pos),
        $(ImU32 col),
        $(char* text_begin),
        $(char* text_end),
        $(float wrap_width),
        $(ImVec4* cpu_fine_clip_rect)
      );
    }
  |]


addPolyLine :: MonadIO m => DrawList -> Ptr ImVec2 -> CInt -> ImU32 -> ImDrawFlags -> CFloat -> m ()
addPolyLine (DrawList drawList) points num_points col flags thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddPolyline(
        $(ImVec2* points),
        $(int num_points),
        $(ImU32 col),
        $(ImDrawFlags flags),
        $(float thickness)
      );
    }
  |]

addConvexPolyFilled :: MonadIO m => DrawList -> Ptr ImVec2 -> CInt -> ImU32 -> m ()
addConvexPolyFilled (DrawList drawList) points num_points col = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddConvexPolyFilled(
        $(ImVec2* points),
        $(int num_points),
        $(ImU32 col)
      );
    }
  |]


addBezierCubic
  :: MonadIO m
  => DrawList
  -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -- Positions (control points)
  -> ImU32
  -> CFloat
  -> CInt
  -> m ()
addBezierCubic (DrawList drawList) p1 p2 p3 p4 col thickness numSegments = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddBezierCubic(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        *$(ImVec2* p4),
        $(ImU32 col),
        $(float thickness),
        $(int numSegments)
      );
    }
  |]

addBezierQuadratic
  :: MonadIO m
  => DrawList
  -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -- Positions (control points)
  -> ImU32
  -> CFloat
  -> CInt
  -> m ()
addBezierQuadratic (DrawList drawList) p1 p2 p3 col thickness numSegments = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddBezierQuadratic(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        $(ImU32 col),
        $(float thickness),
        $(int numSegments)
      );
    }
  |]


addImage
  :: MonadIO m
  => DrawList
  -> Ptr ()
  -> Ptr ImVec2 -> Ptr ImVec2 -- Positions
  -> Ptr ImVec2 -> Ptr ImVec2 -- UVs
  -> ImU32
  -> m ()
addImage (DrawList drawList) userTextureIDPtr p_min p_max uv_min uv_max col = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddImage(
        $(void* userTextureIDPtr),
        *$(ImVec2* p_min),
        *$(ImVec2* p_max),
        *$(ImVec2* uv_min),
        *$(ImVec2* uv_max),
        $(ImU32 col)
      );
    }
  |]

addImageQuad
  :: MonadIO m
  => DrawList
  -> Ptr ()
  -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -- Positions
  -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -> Ptr ImVec2 -- UVs
  -> ImU32
  -> m ()
addImageQuad (DrawList drawList) userTextureIDPtr p1 p2 p3 p4 uv1 uv2 uv3 uv4 col = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddImageQuad(
        $(void* userTextureIDPtr),
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        *$(ImVec2* p4),
        *$(ImVec2* uv1),
        *$(ImVec2* uv2),
        *$(ImVec2* uv3),
        *$(ImVec2* uv4),
        $(ImU32 col)
      );
    }
  |]

addImageRounded
  :: MonadIO m
  => DrawList
  -> Ptr ()
  -> Ptr ImVec2 -> Ptr ImVec2 -- Positions
  -> Ptr ImVec2 -> Ptr ImVec2 -- UVs
  -> ImU32
  -> CFloat
  -> ImDrawFlags
  -> m ()
addImageRounded (DrawList drawList) userTextureIDPtr p_min p_max uv_min uv_max col rounding flags = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddImageRounded(
        $(void* userTextureIDPtr),
        *$(ImVec2* p_min),
        *$(ImVec2* p_max),
        *$(ImVec2* uv_min),
        *$(ImVec2* uv_max),
        $(ImU32 col),
        $(float rounding),
        $(ImDrawFlags flags)
      );
    }
  |]


pathClear :: MonadIO m => DrawList -> m ()
pathClear (DrawList drawList) = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathClear();
    }
  |]

pathLineTo :: MonadIO m => DrawList -> Ptr ImVec2 -> m ()
pathLineTo (DrawList drawList) pos = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathLineTo(
        *$(ImVec2* pos)
      );
    }
  |]

pathLineToMergeDuplicate :: MonadIO m => DrawList -> Ptr ImVec2 -> m ()
pathLineToMergeDuplicate (DrawList drawList) pos = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathLineToMergeDuplicate(
        *$(ImVec2* pos)
      );
    }
  |]

-- Note: Anti-aliased filling requires points to be in clockwise order.
pathFillConvex :: MonadIO m => DrawList -> ImU32 -> m ()
pathFillConvex (DrawList drawList) col = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathFillConvex(
        $(ImU32 col)
      );
    }
  |]

pathStroke :: MonadIO m => DrawList -> ImU32 -> ImDrawFlags -> CFloat -> m ()
pathStroke (DrawList drawList) col flags thickness = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathStroke(
        $(ImU32 col),
        $(ImDrawFlags flags),
        $(float thickness)
      );
    }
  |]


pathArcTo :: MonadIO m => DrawList -> Ptr ImVec2 -> CFloat -> CFloat -> CFloat -> CInt -> m ()
pathArcTo (DrawList drawList) center radius a_min a_max num_segments = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathArcTo(
        *$(ImVec2* center),
        $(float radius),
        $(float a_min),
        $(float a_max),
        $(int num_segments)
      );
    }
  |]

-- | Use precomputed angles for a 12 steps circle.
pathArcToFast :: MonadIO m => DrawList -> Ptr ImVec2 -> CFloat -> CInt -> CInt -> m ()
pathArcToFast (DrawList drawList) center radius a_min_of_12 a_max_of_12 = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathArcToFast(
        *$(ImVec2* center),
        $(float radius),
        $(int a_min_of_12),
        $(int a_max_of_12)
      );
    }
  |]


pathBezierCubicCurveTo
  :: MonadIO m
  => DrawList
  -> Ptr ImVec2
  -> Ptr ImVec2
  -> Ptr ImVec2
  -> CInt
  -> m ()
pathBezierCubicCurveTo (DrawList drawList) p1 p2 p3 num_segments = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathBezierCubicCurveTo(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        *$(ImVec2* p3),
        $(int num_segments)
      );
    }
  |]

pathBezierQuadraticCurveTo
  :: MonadIO m
  => DrawList
  -> Ptr ImVec2
  -> Ptr ImVec2
  -> CInt
  -> m ()
pathBezierQuadraticCurveTo (DrawList drawList) p1 p2 num_segments = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathBezierQuadraticCurveTo(
        *$(ImVec2* p1),
        *$(ImVec2* p2),
        $(int num_segments)
      );
    }
  |]


pathRect :: MonadIO m => DrawList -> Ptr ImVec2 -> Ptr ImVec2 -> CFloat -> ImDrawFlags -> m ()
pathRect (DrawList drawList) rect_min rect_max rounding flags = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->PathRect(
        *$(ImVec2* rect_min),
        *$(ImVec2* rect_max),
        $(float rounding),
        $(ImDrawFlags flags)
      );
    }
  |]


-- | This is useful if you need to forcefully create a new draw call (to allow for dependent rendering / blending).
-- Otherwise primitives are merged into the same draw-call as much as possible.
addDrawCmd :: MonadIO m => DrawList -> m ()
addDrawCmd (DrawList drawList) = liftIO do
  [C.block|
    void {
      $(ImDrawList* drawList)->AddDrawCmd();
    }
  |]

-- | Create a clone of the CmdBuffer/IdxBuffer/VtxBuffer.
cloneOutput :: MonadIO m => DrawList -> m DrawList
cloneOutput (DrawList drawList) = liftIO do
  DrawList <$> [C.block|
    ImDrawList* {
      return $(ImDrawList* drawList)->CloneOutput();
    }
  |]

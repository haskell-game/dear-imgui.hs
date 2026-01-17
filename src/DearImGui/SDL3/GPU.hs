{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module: DearImGUI.SDL3.GPU

Initialising the SDL3 GPU renderer backend for Dear ImGui.
-}
module DearImGui.SDL3.GPU (
    sdl3InitForSDLGPU,
    sdl3GPUInit,
    sdl3GPUShutdown,
    sdl3GPUNewFrame,
    sdl3GPUPrepareDrawData,
    sdl3GPURenderDrawData,
    sdl3GPUCreateDeviceObjects,
    sdl3GPUDestroyDeviceObjects,
    sdl3GPUCreateFontsTexture,
    sdl3GPUDestroyFontsTexture,
)
where

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

-- transformers
import Control.Monad.IO.Class (
    MonadIO,
    liftIO,
 )

-- DearImGui
import DearImGui (
    DrawData (..),
 )
import Foreign.C.Types (CInt (..))
import Foreign.Ptr
import SDL3 (
    SDLGPUCommandBuffer (SDLGPUCommandBuffer),
    SDLGPUDevice (SDLGPUDevice),
    SDLGPUGraphicsPipeline (SDLGPUGraphicsPipeline),
    SDLGPURenderPass (SDLGPURenderPass),
    SDLWindow (SDLWindow),
 )

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "imgui.h"
C.include "backends/imgui_impl_sdlgpu3.h"
C.include "backends/imgui_impl_sdl3.h"
C.include "SDL3/SDL.h"
Cpp.using "namespace ImGui"

-- | Wraps @ImGui_ImplSDL3_InitForSDLGPU@.
sdl3InitForSDLGPU :: (MonadIO m) => SDLWindow -> m Bool
sdl3InitForSDLGPU (SDLWindow windowPtr) = liftIO do
    let windowPtr' = castPtr windowPtr :: Ptr ()
    (0 /=) <$> [C.exp| bool { ImGui_ImplSDL3_InitForSDLGPU((SDL_Window*)$(void* windowPtr')) } |]

-- | Wraps @ImGui_ImplSDLGPU3_Init@.
sdl3GPUInit :: (MonadIO m) => SDLGPUDevice -> CInt -> CInt -> m Bool
sdl3GPUInit (SDLGPUDevice devicePtr) colorTargetFormat msaaSamples = liftIO do
    let devicePtr' = castPtr devicePtr :: Ptr ()
    (0 /=)
        <$> [C.block| bool {
        ImGui_ImplSDLGPU3_InitInfo info;
        info.Device = (SDL_GPUDevice*)$(void* devicePtr');
        info.ColorTargetFormat = (SDL_GPUTextureFormat)$(int colorTargetFormat);
        info.MSAASamples = (SDL_GPUSampleCount)$(int msaaSamples);
        return ImGui_ImplSDLGPU3_Init(&info);
    } |]

-- | Wraps @ImGui_ImplSDLGPU3_Shutdown@.
sdl3GPUShutdown :: (MonadIO m) => m ()
sdl3GPUShutdown = liftIO do
    [C.exp| void { ImGui_ImplSDLGPU3_Shutdown(); } |]

-- | Wraps @ImGui_ImplSDLGPU3_NewFrame@.
sdl3GPUNewFrame :: (MonadIO m) => m ()
sdl3GPUNewFrame = liftIO do
    [C.exp| void { ImGui_ImplSDLGPU3_NewFrame(); } |]

-- | Wraps @Imgui_ImplSDLGPU3_PrepareDrawData@.
sdl3GPUPrepareDrawData :: (MonadIO m) => DrawData -> SDLGPUCommandBuffer -> m ()
sdl3GPUPrepareDrawData (DrawData ptr) (SDLGPUCommandBuffer cmdBufPtr) = liftIO do
    let cmdBufPtr' = castPtr cmdBufPtr :: Ptr ()
    [C.exp| void { Imgui_ImplSDLGPU3_PrepareDrawData((ImDrawData*)$(void* ptr), (SDL_GPUCommandBuffer*)$(void* cmdBufPtr')) } |]

-- | Wraps @ImGui_ImplSDLGPU3_RenderDrawData@.
sdl3GPURenderDrawData :: (MonadIO m) => DrawData -> SDLGPUCommandBuffer -> SDLGPURenderPass -> Maybe SDLGPUGraphicsPipeline -> m ()
sdl3GPURenderDrawData (DrawData ptr) (SDLGPUCommandBuffer cmdBufPtr) (SDLGPURenderPass renderPassPtr) maybePipeline = liftIO do
    let cmdBufPtr' = castPtr cmdBufPtr :: Ptr ()
        renderPassPtr' = castPtr renderPassPtr :: Ptr ()
        pipelinePtr' = case maybePipeline of
            Just (SDLGPUGraphicsPipeline p) -> castPtr p :: Ptr ()
            Nothing -> nullPtr
    [C.exp| void { ImGui_ImplSDLGPU3_RenderDrawData((ImDrawData*)$(void* ptr), (SDL_GPUCommandBuffer*)$(void* cmdBufPtr'), (SDL_GPURenderPass*)$(void* renderPassPtr'), (SDL_GPUGraphicsPipeline*)$(void* pipelinePtr')) } |]

-- | Wraps @ImGui_ImplSDLGPU3_CreateDeviceObjects@.
sdl3GPUCreateDeviceObjects :: (MonadIO m) => m ()
sdl3GPUCreateDeviceObjects = liftIO do
    [C.exp| void { ImGui_ImplSDLGPU3_CreateDeviceObjects(); } |]

-- | Wraps @ImGui_ImplSDLGPU3_DestroyDeviceObjects@.
sdl3GPUDestroyDeviceObjects :: (MonadIO m) => m ()
sdl3GPUDestroyDeviceObjects = liftIO do
    [C.exp| void { ImGui_ImplSDLGPU3_DestroyDeviceObjects(); } |]

-- | Wraps @ImGui_ImplSDLGPU3_CreateFontsTexture@.
sdl3GPUCreateFontsTexture :: (MonadIO m) => m ()
sdl3GPUCreateFontsTexture = liftIO do
    [C.exp| void { ImGui_ImplSDLGPU3_CreateFontsTexture(); } |]

-- | Wraps @ImGui_ImplSDLGPU3_DestroyFontsTexture@.
sdl3GPUDestroyFontsTexture :: (MonadIO m) => m ()
sdl3GPUDestroyFontsTexture = liftIO do
    [C.exp| void { ImGui_ImplSDLGPU3_DestroyFontsTexture(); } |]
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Example     : GPUAnimatedQuads
Description : Drawing multiple animated and textured quads using SDL GPU API.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/AnimatedQuads C example.
Demonstrates:
- Defining a vertex structure with texture coordinates (PositionTextureVertex).
- Loading a texture from a BMP file using SDL_Surface.
- Creating a GPU sampler for texture lookup.
- Creating vertex and index buffers for a quad.
- Uploading texture and buffer data.
- Defining and using uniform data structures (FragMultiplyUniform, Matrix4x4) for transformations and color effects.
- Pushing uniform data to shaders (SDL_PushGPUVertexUniformData, SDL_PushGPUFragmentUniformData).
- Creating a graphics pipeline with alpha blending enabled.
- Animating quads by updating transformation matrices and color multipliers over time.
- Drawing multiple instances of the quad with different uniforms in a single render pass.
|
-}
module Main where

import Control.Exception (bracket, bracketOnError, finally)
import Control.Monad (unless, void, when)
import Control.Monad.Managed
import Data.Bits ()
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Word (Word16, Word32, Word64)
import DearImGui
import DearImGui.SDL3.GPU
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Alloc ()
import Foreign.Marshal.Array ()
import Foreign.Marshal.Utils ()
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, sizeOf)
import GPUCommon hiding (loadImage)

-- linear algebra library

import Control.Exception (try)
import Control.Exception.Base (IOException)
import DearImGui.SDL3 (sdl3NewFrame)
import GHC.Exception (SomeException)
import Linear
import SDL hiding (cos, sin)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

loadImage ::
    -- | Relative path within the data directory (e.g., "Content/Images/ravioli.bmp")
    FilePath ->
    -- | Returns pointer to the final surface (ABGR8888) or Nothing on failure.
    IO (Maybe (Ptr SDLSurface))
loadImage relativeImagePath = do
    sdlLog $ "Attempting to load image: " ++ relativeImagePath
    let fullPath = relativeImagePath
    -- fullPath <-
    --     getDataFileName relativeImagePath `catch` \(e :: IOException) -> do
    --         sdlLog $ "Error constructing image file path for '" ++ relativeImagePath ++ "': " ++ show e
    --         return ""

    -- Check if getDataFileName failed
    when (null fullPath) $ sdlLog "Failed to get absolute path using getDataFileName."
    if null fullPath
        then return Nothing
        else do
            -- Load the initial BMP surface
            maybeOriginalSurfPtr <- sdlLoadBMP fullPath
            case maybeOriginalSurfPtr of
                Nothing -> do
                    sdlLog $ "SDL_LoadBMP failed for: " ++ fullPath
                    sdlGetError >>= sdlLog . ("SDL Error: " ++)
                    return Nothing
                Just originalSurfPtr -> do
                    sdlLog $ "Loaded original surface: " ++ show originalSurfPtr ++ " from " ++ fullPath

                    -- Peek the original surface's format
                    originalData <- peek originalSurfPtr
                    let originalFormat = surfaceFormat originalData
                    let targetFormat = SDL_PIXELFORMAT_ABGR8888 -- Target format

                    -- Check if conversion is needed
                    if originalFormat == targetFormat
                        then do
                            sdlLog $ "Surface already in target format (" ++ show targetFormat ++ "). Returning original pointer."
                            -- No conversion needed, return the pointer we loaded. Caller now owns it.
                            return $ Just originalSurfPtr
                        else do
                            sdlLog $ "Original format (" ++ show originalFormat ++ ") differs from target (" ++ show targetFormat ++ "). Converting..."

                            -- Attempt conversion
                            eitherConverted <-
                                try (sdlConvertSurface originalSurfPtr targetFormat) ::
                                    IO (Either SomeException (Maybe (Ptr SDLSurface)))

                            -- IMPORTANT: Destroy the original surface *after* the conversion attempt,
                            -- regardless of whether it succeeded or failed. We no longer need it.
                            -- Once this is wrapped properly we could just let GC handle it.
                            sdlLog $ "Destroying original intermediate surface: " ++ show originalSurfPtr
                            sdlDestroySurface originalSurfPtr

                            -- Handle the result of the conversion attempt
                            case eitherConverted of
                                Left ex -> do
                                    sdlLog $ "!!! Exception during SDL_ConvertSurface: " ++ show ex
                                    return Nothing -- Conversion failed due to exception
                                Right Nothing -> do
                                    sdlLog "!!! SDL_ConvertSurface returned NULL (failed)."
                                    sdlGetError >>= sdlLog . ("SDL Error: " ++)
                                    return Nothing -- Conversion failed, returned null
                                Right (Just convertedSurfPtr) -> do
                                    sdlLog $ "Successfully converted to new surface: " ++ show convertedSurfPtr
                                    -- Conversion succeeded, return the new pointer. Caller now owns it.
                                    return $ Just convertedSurfPtr

{- | We can also use Geomancy for linalg
import Geomancy.Mat4               (Mat4, transpose, matrixProduct)
import Geomancy.Vulkan.Projection  (orthoOffCenter)
import Geomancy.Transform          (Transform(..), translate, rotateQ)
import Geomancy.Vec3               (vec3)
import Geomancy.Quaternion         (axisAngle)
-}

-- Vertex data (Updated coordinates for -0.5 to 0.5 range)
vertexData :: [PositionTextureVertex]
vertexData =
    [ PositionTextureVertex (-0.5) (-0.5) 0 0 0 -- Bottom-left
    , PositionTextureVertex 0.5 (-0.5) 0 1 0 -- Bottom-right
    , PositionTextureVertex 0.5 0.5 0 1 1 -- Top-right
    , PositionTextureVertex (-0.5) 0.5 0 0 1 -- Top-left
    ]

-- Index data (remains the same)
indexData :: [Word16]
indexData = [0, 1, 2, 0, 2, 3]

-- AppResources (Updated for single sampler)
data AppResources = AppResources
    { resPipeline :: SDLGPUGraphicsPipeline
    , resVertexBuffer :: SDLGPUBuffer
    , resIndexBuffer :: SDLGPUBuffer
    , resTexture :: SDLGPUTexture
    , resSampler :: SDLGPUSampler -- Changed from resSamplers
    }
    deriving (Show)

-- main
main :: IO ()
main = do
    sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
    linkedVersion <- sdlGetVersion
    sdlLog $ "Linked SDL Version: " ++ show linkedVersion

    maybeResult <- withContext "SDL3 Haskell GPU Animated Quads" [SDL_WINDOW_RESIZABLE] runAppGPU
    case maybeResult of
        Nothing -> do
            sdlLog "Application initialization failed (commonInit)."
            exitFailure
        Just _ -> do
            sdlLog "Application finished successfully."
            exitSuccess

-- runAppGPU
runAppGPU :: GPUCommon.Context -> IO ()
runAppGPU context@GPUCommon.Context{} = do
    sdlLog "Base context initialized."
    let device = contextDevice context
        window = contextWindow context

    sdlSetGPUSwapchainParameters device window SDL_GPU_SWAPCHAINCOMPOSITION_SDR SDL_GPU_PRESENTMODE_MAILBOX
    -- Init ImGui
    -- runManaged $ do
    createContext
    sdl3InitForSDLGPU (contextWindow context)
    SDLGPUTextureFormat tf <- sdlGetGPUSwapchainTextureFormat device window
    r <- sdl3GPUInit device tf 0
    unless r $ do
        sdlLog "Failed to initialize ImGui SDL3 GPU renderer."
    timeRef <- newIORef (0.0 :: Float) -- For animation time
    bracket
        (createResources context)
        (releaseResources context) -- Bracket release
        $ \case
            Nothing -> sdlLog "Failed to create resources. Exiting."
            Just resources -> do
                sdlLog "Resources created successfully."
                startTime <- sdlGetPerformanceCounter
                freq <- sdlGetPerformanceFrequency
                deltaTimeRef <- newIORef 0.0
                eventLoopGPU context resources startTime freq deltaTimeRef timeRef -- Pass timeRef

-- createResources
createResources :: GPUCommon.Context -> IO (Maybe AppResources)
createResources GPUCommon.Context{contextDevice = dev, contextWindow = win} = do
    sdlLog "--- Beginning Resource Creation ---"

    -- 1. Load Shaders (Updated filenames and uniform counts)
    sdlLog "Loading shaders..."
    -- Vertex shader expects 1 uniform buffer (slot 0, for matrix)
    let vertShaderInfo = defaultShaderCreateInfo{shaderNumUniformBuffers = 1}
    maybeVertShader <- loadShader dev "TexturedQuadWithMatrix.vert" SDL_GPU_SHADERSTAGE_VERTEX vertShaderInfo

    -- Fragment shader expects 1 sampler (slot 0), 1 uniform buffer (slot 0, for color multiplier)
    let fragShaderInfo = defaultShaderCreateInfo{shaderNumSamplers = 1, shaderNumUniformBuffers = 1}
    maybeFragShader <- loadShader dev "TexturedQuadWithMultiplyColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT fragShaderInfo

    case (maybeVertShader, maybeFragShader) of
        (Just vertShader, Just fragShader) -> do
            sdlLog "Shaders loaded successfully."

            -- 2. Load Image (using helper from GPUCommon)
            let imagePath = "./" </> "examples" </> "sdl3" </> "GPU" </> "ravioli.bmp"
            bracketOnError
                (loadImage imagePath)
                ( \maybeSurfPtr -> do
                    -- Cleanup on error after image load attempt
                    when (isJust maybeSurfPtr) $ do
                        sdlLog $ "Error during resource creation. Cleaning up loaded/converted surface: " ++ show (fromJust maybeSurfPtr)
                        sdlDestroySurface (fromJust maybeSurfPtr)
                    sdlLog "Cleaning up shaders due to error after shader load."
                    sdlReleaseGPUShader dev vertShader
                    sdlReleaseGPUShader dev fragShader
                )
                ( \maybeSurfacePtr -> do
                    -- Main block after successful image load
                    case maybeSurfacePtr of
                        Nothing -> do
                            sdlLog "Failed to load or convert image. Resource creation aborted."
                            return Nothing
                        Just surfacePtr -> do
                            sdlLog $ "Image loaded and converted successfully: " ++ show surfacePtr
                            bracket
                                (return surfacePtr) -- Manage final surface lifetime
                                sdlDestroySurface
                                $ \surfManagedPtr -> do
                                    maybeResources <- createGPUObjects dev win vertShader fragShader surfManagedPtr
                                    sdlLog "Releasing shaders..."
                                    sdlReleaseGPUShader dev vertShader
                                    sdlReleaseGPUShader dev fragShader
                                    sdlLog "Shaders released."
                                    return maybeResources
                )
        _ -> do
            -- Shader loading failed
            sdlLog "!!! Failed to load one or both shaders. Resource creation aborted."
            maybe (return ()) (sdlReleaseGPUShader dev) maybeVertShader
            maybe (return ()) (sdlReleaseGPUShader dev) maybeFragShader
            return Nothing

-- createGPUObjects (Creates single sampler, uses updated pipeline)
createGPUObjects :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> Ptr SDLSurface -> IO (Maybe AppResources)
createGPUObjects dev win vertShader fragShader surfacePtr = do
    sdlLog "Creating GPU objects (pipeline, sampler, buffers, texture)..."

    -- Peek Surface Info
    surfaceData <- peek surfacePtr :: IO SDLSurface
    let texWidth = surfaceW surfaceData
    let texHeight = surfaceH surfaceData
    when (surfacePixels surfaceData == nullPtr) $ sdlLog "!!! WARNING: Surface pixel data pointer is NULL!"

    -- Calculate data sizes
    (_, _, vertexDataSize) <- calculateBufferDataSize vertexData "Vertex"
    (_, _, indexDataSize) <- calculateBufferDataSize indexData "Index"
    let bytesPerPixel = 4 :: Int -- Assuming ABGR8888 or RGBA8888 after conversion
    let textureDataSize = fromIntegral (texWidth * texHeight * bytesPerPixel) :: Word32

    -- \*** Create ONE Sampler ***
    let samplerCI =
            SDLGPUSamplerCreateInfo
                { samplerMinFilter = SDL_GPU_FILTER_NEAREST
                , samplerMagFilter = SDL_GPU_FILTER_NEAREST
                , samplerMipmapMode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST
                , samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
                , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
                , samplerAddressModeW = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
                , samplerMipLodBias = 0.0
                , samplerMaxAnisotropy = 1.0
                , samplerCompareOp = SDL_GPU_COMPAREOP_NEVER
                , samplerMinLod = 0.0
                , samplerMaxLod = 0.0
                , samplerEnableAnisotropy = False
                , samplerEnableCompare = False
                , samplerProps = 0
                }

    -- Bracket resource creation
    bracketOnError
        (createPipeline dev win vertShader fragShader) -- Pass shaders
        (cleanupMaybe "Pipeline" (sdlReleaseGPUGraphicsPipeline dev))
        $ \maybePipeline ->
            bracketOnError
                (sdlCreateGPUSampler dev samplerCI) -- Create single sampler
                (cleanupMaybe "Sampler" (sdlReleaseGPUSampler dev))
                $ \maybeSampler ->
                    bracketOnError
                        (createGPUBuffer dev SDL_GPU_BUFFERUSAGE_VERTEX vertexDataSize "VertexBuffer")
                        (cleanupMaybe "VertexBuffer" (sdlReleaseGPUBuffer dev))
                        $ \maybeVertexBuffer ->
                            bracketOnError
                                (createGPUBuffer dev SDL_GPU_BUFFERUSAGE_INDEX indexDataSize "IndexBuffer")
                                (cleanupMaybe "IndexBuffer" (sdlReleaseGPUBuffer dev))
                                $ \maybeIndexBuffer ->
                                    bracketOnError
                                        (createGPUTexture dev texWidth texHeight)
                                        (cleanupMaybe "Texture" (sdlReleaseGPUTexture dev))
                                        $ \maybeTexture ->
                                            case (maybePipeline, maybeSampler, maybeVertexBuffer, maybeIndexBuffer, maybeTexture) of
                                                (Just pipeline, Just sampler, Just vertexBuffer, Just indexBuffer, Just texture) -> do
                                                    sdlLog "Core GPU objects created. Proceeding with data upload..."
                                                    uploadOk <-
                                                        uploadAllData
                                                            dev
                                                            surfacePtr
                                                            texWidth
                                                            texHeight
                                                            bytesPerPixel
                                                            vertexBuffer
                                                            indexBuffer
                                                            texture
                                                            vertexDataSize
                                                            indexDataSize
                                                            textureDataSize
                                                    if uploadOk
                                                        then do
                                                            sdlLog "--- Resource Creation Successful ---"
                                                            return $
                                                                Just
                                                                    AppResources
                                                                        { resPipeline = pipeline
                                                                        , resVertexBuffer = vertexBuffer
                                                                        , resIndexBuffer = indexBuffer
                                                                        , resTexture = texture
                                                                        , resSampler = sampler
                                                                        }
                                                        else do
                                                            sdlLog "!!! Data upload failed. Resource creation aborted."
                                                            return Nothing
                                                _ -> do
                                                    sdlLog "!!! Failed to create one or more core GPU objects. Resource creation aborted."
                                                    return Nothing
  where
    cleanupMaybe name release = mapM_ (\res -> sdlLog ("Error occurred, releasing partially created " ++ name ++ ": " ++ show res) >> release res)

-- createPipeline (Uses alpha blend state)
createPipeline :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe SDLGPUGraphicsPipeline)
createPipeline dev win vertShader fragShader = do
    sdlLog "Creating Graphics Pipeline..."
    swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win
    let vertexSize = sizeOf (undefined :: PositionTextureVertex)
    let texCoordOffset = sizeOf (undefined :: CFloat) * 3
    let vertexAttributes =
            [ SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 0
            , SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2 (fromIntegral texCoordOffset)
            ]
        vertexBufferDesc = [SDLGPUVertexBufferDescription 0 (fromIntegral vertexSize) SDL_GPU_VERTEXINPUTRATE_VERTEX 0]
        vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes

    -- Define Alpha Blend State
    let blendState =
            defaultColorTargetBlendState
                { enableBlend = True
                , blendOp = SDL_GPU_BLENDOP_ADD
                , alphaOp = SDL_GPU_BLENDOP_ADD
                , srcColorFactor = SDL_GPU_BLENDFACTOR_SRC_ALPHA
                , dstColorFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA
                , srcAlphaFactor = SDL_GPU_BLENDFACTOR_SRC_ALPHA -- C Ex uses SRC_ALPHA here too
                , dstAlphaFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA -- C Ex uses ONE_MINUS_SRC_ALPHA here too
                }

    let colorTargetDesc = SDLGPUColorTargetDescription swapchainFormat blendState
        targetInfo = SDLGPUGraphicsPipelineTargetInfo [colorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False
        pipelineCI =
            (defaultGraphicsPipelineCreateInfo vertShader fragShader swapchainFormat)
                { vertexInputState = vertexInputState
                , targetInfo = targetInfo
                }
    maybePipeline <- sdlCreateGPUGraphicsPipeline dev pipelineCI
    when (isNothing maybePipeline) $ sdlGetError >>= sdlLog . ("!!! Failed to create graphics pipeline: " ++)
    return maybePipeline

{- | Helper function to upload vertex, index, and texture data
Accepts Ptr SDLSurface
-}
uploadAllData :: SDLGPUDevice -> Ptr SDLSurface -> Int -> Int -> Int -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUTexture -> Word32 -> Word32 -> Word32 -> IO Bool
uploadAllData dev surfacePtr texWidth texHeight bytesPerPixel vertexBuf indexBuf texture vertexSize indexSize textureSize = do
    sdlLog "Starting data upload process..."

    let bufferDataTotalSize = vertexSize + indexSize
    let vertexOffset = 0
    let indexOffset = vertexSize

    bracket
        (createTransferBuffer dev bufferDataTotalSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "Buffer")
        (cleanupTransferBuffer dev)
        $ \maybeBufTransfer ->
            bracket
                (createTransferBuffer dev textureSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "Texture")
                (cleanupTransferBuffer dev)
                $ \maybeTexTransfer ->
                    bracket
                        (sdlAcquireGPUCommandBuffer dev)
                        cleanupCommandBuffer
                        $ \maybeCmdBuf ->
                            case (maybeBufTransfer, maybeTexTransfer, maybeCmdBuf) of
                                (Just bufTransfer, Just texTransfer, Just cmdBuf) -> do
                                    sdlLog "All necessary transfer buffers and command buffer acquired."

                                    bufMapOk <- mapAndCopyBufferData dev bufTransfer vertexData indexData vertexOffset indexOffset
                                    unless bufMapOk $ sdlLog "!!! Buffer data map/copy failed."

                                    texMapOk <-
                                        if bufMapOk
                                            then mapAndCopyTextureData dev texTransfer surfacePtr texWidth texHeight bytesPerPixel -- Uses converted surface
                                            else return False
                                    unless texMapOk $ sdlLog "!!! Texture data map/copy failed."

                                    if bufMapOk && texMapOk
                                        then do
                                            recordOk <-
                                                recordUploadCommands
                                                    dev
                                                    cmdBuf
                                                    bufTransfer
                                                    texTransfer
                                                    vertexBuf
                                                    indexBuf
                                                    texture
                                                    vertexOffset
                                                    indexOffset
                                                    texWidth
                                                    texHeight
                                            if recordOk
                                                then do
                                                    sdlLog "Submitting upload commands..."
                                                    submitted <- sdlSubmitGPUCommandBuffer cmdBuf

                                                    if submitted
                                                        then do
                                                            sdlLog "Upload command buffer submitted successfully."
                                                            sdlLog "Waiting for GPU device idle after upload submission..."
                                                            _ <- sdlWaitForGPUIdle dev `finally` sdlLog "GPU device idle wait finished (or errored)."
                                                            sdlLog "GPU device idle."
                                                            return True
                                                        else do
                                                            errMsg <- sdlGetError
                                                            sdlLog ("!!! Failed to submit upload command buffer: " ++ errMsg)
                                                            return False
                                                else do
                                                    sdlLog "!!! Failed to record upload commands. Upload cancelled."
                                                    return False
                                        else do
                                            sdlLog "!!! Mapping/copying failed. Upload cancelled."
                                            return False
                                _ -> do
                                    sdlLog "!!! Failed to acquire transfer buffers or command buffer for upload."
                                    return False

recordUploadCommands :: SDLGPUDevice -> SDLGPUCommandBuffer -> SDLGPUTransferBuffer -> SDLGPUTransferBuffer -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUTexture -> Word32 -> Word32 -> Int -> Int -> IO Bool
recordUploadCommands _ cmdBuf bufTransfer texTransfer vertexBuf indexBuf texture vOffset iOffset texWidth texHeight = do
    sdlLog "Beginning Copy Pass for uploads..."
    bracket
        (sdlBeginGPUCopyPass cmdBuf)
        (`for_` sdlEndGPUCopyPass)
        $ \case
            Nothing -> sdlLog "!!! Failed to begin copy pass." >> return False
            Just copyPass -> do
                sdlLog "Copy Pass begun. Recording uploads..."
                (_, _, vertexSizeW32) <- calculateBufferDataSize vertexData "Vertex"
                (_, _, indexSizeW32) <- calculateBufferDataSize indexData "Index"

                let vbSrc = SDLGPUTransferBufferLocation bufTransfer vOffset
                let vbDst = SDLGPUBufferRegion vertexBuf 0 vertexSizeW32
                sdlLog $ "Recording Vertex Buffer Upload: " ++ show vbSrc ++ " -> " ++ show vbDst
                sdlUploadToGPUBuffer copyPass vbSrc vbDst False

                let ibSrc = SDLGPUTransferBufferLocation bufTransfer iOffset
                let ibDst = SDLGPUBufferRegion indexBuf 0 indexSizeW32
                sdlLog $ "Recording Index Buffer Upload: " ++ show ibSrc ++ " -> " ++ show ibDst
                sdlUploadToGPUBuffer copyPass ibSrc ibDst False

                let texSrc =
                        SDLGPUTextureTransferInfo
                            { gpuTexTransferBuffer = texTransfer
                            , gpuTexTransferOffset = 0
                            , gpuTexTransferPixelsPerRow = 0
                            , gpuTexTransferRowsPerLayer = 0
                            }
                let texDst = SDLGPUTextureRegion texture 0 0 0 0 0 (fromIntegral texWidth) (fromIntegral texHeight) 1
                sdlLog $ "Recording Texture Upload (Simplified Src): " ++ show texSrc ++ " -> " ++ show texDst
                sdlUploadToGPUTexture copyPass texSrc texDst False

                sdlLog "Upload commands recorded."
                return True

-- releaseResources (Updated for single sampler)
releaseResources :: GPUCommon.Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources GPUCommon.Context{..} (Just AppResources{..}) = do
    sdlLog "--> Releasing AppResources..."
    sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
    sdlReleaseGPUBuffer contextDevice resVertexBuffer
    sdlReleaseGPUBuffer contextDevice resIndexBuffer
    sdlReleaseGPUTexture contextDevice resTexture
    sdlReleaseGPUSampler contextDevice resSampler
    sdlLog "<-- AppResources Released."

-- eventLoopGPU (Added time update)
eventLoopGPU :: GPUCommon.Context -> AppResources -> Word64 -> Word64 -> IORef Double -> IORef Float -> IO ()
eventLoopGPU context resources lastTime freq deltaTimeRef timeRef = do
    -- Calculate deltaTime
    currentTime <- sdlGetPerformanceCounter
    let frameTicks = currentTime - lastTime
    let dtSeconds = fromIntegral frameTicks / fromIntegral freq :: Double
    writeIORef deltaTimeRef dtSeconds -- Store actual seconds

    -- Update time state
    oldTime <- readIORef timeRef
    let newTime = oldTime + realToFrac dtSeconds -- Add seconds to time
    writeIORef timeRef newTime

    -- Event processing
    sdlPumpEvents
    shouldQuitRef <- newIORef False
    processEventsGPU shouldQuitRef

    shouldQuit <- readIORef shouldQuitRef
    unless shouldQuit $ do
        renderFrameGPU context resources timeRef
        eventLoopGPU context resources currentTime freq deltaTimeRef timeRef -- Recursive call

-- processEventsGPU
processEventsGPU :: IORef Bool -> IO ()
processEventsGPU shouldQuitRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return ()
        Just event -> do
            quit <- handleEventGPU event
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU shouldQuitRef

-- handleEventGPU
handleEventGPU :: SDLEvent -> IO Bool
handleEventGPU event = case event of
    SDLEventQuit _ -> sdlLog "Quit event received." >> return True
    SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
        case scancode of
            SDL_SCANCODE_Q -> return True
            _ -> return False
    _ -> return False

imguiGPUNewFrame :: IO ()
imguiGPUNewFrame = do
    sdl3GPUNewFrame
    sdl3NewFrame
    newFrame

-- renderFrameGPU
renderFrameGPU :: GPUCommon.Context -> AppResources -> IORef Float -> IO ()
renderFrameGPU GPUCommon.Context{..} AppResources{..} timeRef = do
    imguiGPUNewFrame

    showDemoWindow
    render
    drawData <- getDrawData

    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> sdlLog "Error: Failed to acquire render command buffer."
        Just cmdbuf -> do
            maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
            case maybeSwapchain of
                Nothing -> do
                    sdlLog "Error: Failed to acquire swapchain texture."
                    void (sdlSubmitGPUCommandBuffer cmdbuf `finally` return ()) -- Submit even on failure
                Just (swapchainTexture, _, _) -> do
                    -- IMGUI
                    sdl3GPUPrepareDrawData drawData cmdbuf

                    -- Ignore w, h for now
                    let clearColor = SDLFColor 0.0 0.0 0.0 1.0
                    let colorTargetInfo =
                            defaultColorTargetInfo
                                { texture = swapchainTexture
                                , clearColor = clearColor
                                , loadOp = SDL_GPU_LOADOP_CLEAR
                                , storeOp = SDL_GPU_STOREOP_STORE
                                }
                    bracket
                        (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
                        cleanupMaybeRenderPass
                        $ \case
                            Nothing -> sdlLog "Error: Failed to begin render pass."
                            Just renderPass -> do
                                -- Bind fixed resources once
                                sdlBindGPUGraphicsPipeline renderPass resPipeline
                                let vbBinding = SDLGPUBufferBinding resVertexBuffer 0
                                    ibBinding = SDLGPUBufferBinding resIndexBuffer 0
                                    texSamplerBinding = SDLGPUTextureSamplerBinding resTexture resSampler
                                 in do
                                        sdlBindGPUVertexBuffers renderPass 0 [vbBinding]
                                        sdlBindGPUIndexBuffer renderPass ibBinding SDL_GPU_INDEXELEMENTSIZE_16BIT
                                        sdlBindGPUFragmentSamplers renderPass 0 [texSamplerBinding]

                                -- Get current time
                                t <- readIORef timeRef

                                -- \| This uncommented block uses Linear
                                let proj :: M44 Float
                                    proj =
                                        ortho
                                            (-1)
                                            1
                                            (-1)
                                            1
                                            (-1)
                                            1

                                -- drawQuad helper
                                let drawQuad tx ty rot colorFunc =
                                        let axis = V3 0 0 1
                                            rotQuat = axisAngle axis rot -- Quaternion CFloat
                                            trans = V3 tx ty 0.0 -- V3 CFloat
                                            model = mkTransformation rotQuat trans -- M44 CFloat
                                            mvpRaw = proj !*! model
                                            mvp = transpose mvpRaw
                                            fragUniform = colorFunc t
                                         in do
                                                sdlPushGPUVertexUniformData cmdbuf 0 mvp
                                                sdlPushGPUFragmentUniformData cmdbuf 0 fragUniform
                                                sdlDrawGPUIndexedPrimitives renderPass 6 1 0 0 0

                                -- -- | This commented out block of code does the exact same thing but using Geomancy for lin alg ops.
                                -- -- build an orthographic proj from -1..1 in XY, with near=-1, far=1, width=2, height=2
                                -- let proj :: Mat4
                                --     proj = unTransform $ orthoOffCenter (-1)    1  2  2
                                --     --                                    ^     ^  ^  ^
                                --     --                                   near  far w  h

                                -- let drawQuad tx ty rot colorFunc = do
                                --       let
                                --           -- build your model→view transforms in Float
                                --           axis    = vec3 0 0 1
                                --           quat    = axisAngle axis rot
                                --           rotT    = rotateQ quat
                                --           transT  = translate tx ty 0
                                --           modelT  = rotT <> transT
                                --           modelM  = unTransform modelT       -- Mat4 Float

                                --           proj    = unTransform $ orthoOffCenter (-1) 1 2 2

                                --           -- sample your color func
                                --           fragU   = colorFunc t             -- FragMultiplyUniform (all Floats)

                                --       sdlPushGPUVertexUniformData   cmdbuf 0 modelM
                                --       sdlPushGPUFragmentUniformData cmdbuf 0 fragU
                                --       sdlDrawGPUIndexedPrimitives   renderPass 6 1 0 0 0

                                -- Top-left
                                drawQuad (-0.5) 0.5 t $ \tm ->
                                    FragMultiplyUniform 1.0 (0.5 + sin tm * 0.5) 1.0 1.0
                                -- Top-right
                                drawQuad 0.5 0.5 (2.0 * pi - t) $ \tm ->
                                    FragMultiplyUniform 1.0 (0.5 + cos tm * 0.5) 1.0 1.0
                                -- Bottom-left
                                drawQuad (-0.5) (-0.5) t $ \tm ->
                                    FragMultiplyUniform 1.0 (0.5 + sin tm * 0.2) 1.0 1.0
                                -- Bottom-right
                                drawQuad 0.5 (-0.5) t $ \tm ->
                                    FragMultiplyUniform 1.0 (0.5 + cos tm * 1.0) 1.0 1.0

                                sdl3GPURenderDrawData drawData cmdbuf renderPass Nothing

                    -- Submit after bracket ensures render pass is ended
                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ sdlLog "Error: Failed to submit render command buffer."
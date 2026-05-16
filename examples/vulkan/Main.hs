{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified DearImGui as ImGui
import qualified DearImGui.SDL as ImGui.SDL
import qualified DearImGui.SDL.Vulkan as ImGui.SDL.Vulkan
import Debug.Trace (traceM)
import Foreign.Ptr (castPtr)
import Render (render)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan.Core10
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12 (pattern API_VERSION_1_2)
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Utils.Frame (frameDeviceRequirements, frameInstanceRequirements)
import qualified Vulkan.Utils.Init.SDL2 as Init
import Vulkan.Utils.Init.SDL2.Window (createWindow, drawableSize, showWindow, withSDL)
import Vulkan.Utils.Queues (withDevice)
import Vulkan.Utils.Swapchain (Swapchain, SwapchainConfig (..), allocSwapchain, defaultSwapchainConfig)
import Vulkan.Utils.VulkanContext (VulkanContext, mkVulkanContext)
import Vulkan.Zero (zero)

main :: IO ()
main = runResourceT $ do
    withSDL
    window <- createWindow appName windowWidth windowHeight
    showWindow window
    (vc, initialSC) <- withWindowedVk window
    _ <- allocate ImGui.createContext ImGui.destroyContext
    _ <- allocate_ (ImGui.SDL.Vulkan.sdl2InitForVulkan window) ImGui.SDL.sdl2Shutdown
    render vc initialSC (drawableSize window) pump
  where
    pump =
        any (\SDL.Event{SDL.eventPayload = e} -> e == SDL.QuitEvent) <$> ImGui.SDL.pollEventsWithImGui

appName :: Text
appName = "Haskell Vulkan triangle example"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

{- | Open a Vulkan instance + device + initial swapchain bound to the given
window. Logs the chosen device's name to stderr.
-}
withWindowedVk ::
    SDL.Window ->
    ResourceT IO (VulkanContext, Swapchain)
withWindowedVk window = do
    inst <-
        Init.withInstance
            window
            ( Just
                zero
                    { Vk.applicationName = Just (Text.encodeUtf8 appName)
                    , Vk.apiVersion = API_VERSION_1_2
                    }
            )
            frameInstanceRequirements
            []
    (_, surf) <-
        allocate
            (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr (instanceHandle inst)))
            (\s -> destroySurfaceKHR inst s Nothing)

    (phys, dev, qs) <- Vulkan.Utils.Queues.withDevice inst (Just surf) frameDeviceRequirements
    props <- Vk.getPhysicalDeviceProperties phys
    traceM $ "Using device: " <> show (Vk.deviceName props)
    vc <- liftIO $ mkVulkanContext inst phys dev qs

    initialSize <- drawableSize window
    initialSC <- allocSwapchain phys dev swapchainConfig Vk.NULL_HANDLE initialSize surf
    pure (vc, initialSC)
  where
    swapchainConfig =
        defaultSwapchainConfig
            { scSurfaceFormatPreferences =
                [ prefer FORMAT_B8G8R8A8_UNORM
                , prefer FORMAT_R8G8B8A8_UNORM
                ]
            }
    prefer fmt SurfaceFormatKHR{format, colorSpace} =
        format == fmt && colorSpace == COLOR_SPACE_SRGB_NONLINEAR_KHR

module Main where

-- Used for rendererName

import Control.Exception (bracket)
import Control.Exception.Base (bracket_)
import Control.Monad (unless, when)
import Control.Monad.Managed
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import DearImGui
import DearImGui.SDL3
import DearImGui.SDL3.Renderer
import SDL
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

-- Key state IORefs type alias for clarity
type KeyStates = (IORef Bool, IORef Bool, IORef Bool, IORef Bool) -- Up, Down, Left, Right

main :: IO ()
main = do
    -- Check compiled version
    sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
    when (sdlVersionAtLeast 3 3 0) $ sdlLog "Compiled with at least SDL 3.3.0"

    -- Get linked version
    linkedVersion <- sdlGetVersion
    sdlLog $ "Linked SDL Version: " ++ show linkedVersion

    -- Initialize SDL (Events are implicitly initialized by Video, but explicit is fine)
    initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]
    unless initSuccess $ do
        sdlLog "Failed to initialize SDL!"
        exitFailure

    -- Check initialized subsystems
    initializedSystems <- sdlWasInit []
    sdlLog "Initialized subsystems:"
    mapM_ printSubsystem initializedSystems

    -- Create a window
    runManaged $ do
        window <- do
            let createAction = sdlCreateWindow "SDL3 Haskell Render Example" 800 600 [SDL_WINDOW_RESIZABLE]
            managed $ bracket createAction (maybe (error "Bad window") sdlDestroyWindow)
        case window of
            Nothing -> liftIO $ do
                sdlLog "Failed to create window!"
                sdlQuit
                exitFailure
            Just win -> do
                liftIO $ sdlLog "Window created successfully!"

                -- Create a Renderer
                renderer <- managed $ bracket (sdlCreateRenderer win Nothing) (error "no renderer" sdlDestroyRenderer) -- Let SDL choose
                case renderer of
                    Nothing -> liftIO $ do
                        sdlLog "Failed to create default renderer!"
                        err <- sdlGetError
                        sdlLog $ "SDL Error: " ++ err
                        sdlDestroyWindow win
                        sdlQuit
                        exitFailure
                    Just ren -> do
                        _ <- managed $ bracket createContext destroyContext
                        _ <- managed_ $ do
                            bracket_ (sdl3InitForSDLRenderer win ren) sdl3Shutdown
                        _ <- managed_ $ bracket_ (sdl3RendererInit ren) sdl3RendererShutdown
                        liftIO $ do
                            mRendererName <- sdlGetRendererName ren
                            sdlLog $ "Created renderer: " ++ fromMaybe "Unknown" mRendererName

                            -- Run the application logic
                            runApp win ren

                            -- Cleanup
                            sdlLog "Destroying renderer..."
                            sdlDestroyRenderer ren
                            sdlLog "Renderer destroyed."
        pure ()

-- case window of
--     Nothing -> do
--         sdlLog "Failed to create window!"
--         sdlQuit
--         exitFailure
--     Just win -> do
--         sdlLog "Window created successfully!"

--         -- Create a Renderer
--         renderer <- sdlCreateRenderer win Nothing -- Let SDL choose
--         case renderer of
--             Nothing -> do
--                 sdlLog "Failed to create default renderer!"
--                 err <- sdlGetError
--                 sdlLog $ "SDL Error: " ++ err
--                 sdlDestroyWindow win
--                 sdlQuit
--                 exitFailure
--             Just ren -> do
--                 mRendererName <- sdlGetRendererName ren
--                 sdlLog $ "Created renderer: " ++ fromMaybe "Unknown" mRendererName
--                 runApp win ren -- Pass window and renderer to runApp
-- sdlLog "Shutting down SDL..."
-- sdlQuit
-- sdlLog "Application terminated successfully"
-- exitSuccess

-- | Encapsulate the application logic with window and renderer
runApp :: SDLWindow -> SDLRenderer -> IO ()
runApp win renderer = do
    startTime <- sdlGetPerformanceCounter
    freq <- sdlGetPerformanceFrequency
    deltaTimeRef <- newIORef 0.0 -- Will store delta time in seconds
    rectPosRef <- newIORef (SDLFPoint 100 100)
    shouldQuitRef <- newIORef False

    -- Create IORefs for key states
    upPressedRef <- newIORef False
    downPressedRef <- newIORef False
    leftPressedRef <- newIORef False
    rightPressedRef <- newIORef False
    let keyStates = (upPressedRef, downPressedRef, leftPressedRef, rightPressedRef)

    eventLoop win renderer startTime freq deltaTimeRef rectPosRef shouldQuitRef keyStates

    -- Cleanup (happens after eventLoop finishes)
    sdlLog "Destroying renderer..."
    sdlDestroyRenderer renderer
    sdlLog "Renderer destroyed."
    sdlLog "Destroying window..."
    sdlDestroyWindow win
    sdlLog "Window destroyed."

-- | Main event loop
eventLoop :: SDLWindow -> SDLRenderer -> Word64 -> Word64 -> IORef Double -> IORef SDLFPoint -> IORef Bool -> KeyStates -> IO ()
eventLoop window renderer lastTime freq deltaTimeRef rectPosRef shouldQuitRef keyStates = do
    currentTime <- sdlGetPerformanceCounter
    let deltaTimeInSeconds = fromIntegral (currentTime - lastTime) / fromIntegral freq
    writeIORef deltaTimeRef deltaTimeInSeconds -- Store delta time in seconds

    -- Event handling: Process all pending events for this frame
    sdlPumpEvents
    processEvents shouldQuitRef keyStates -- This will handle multiple events
    shouldQuit <- readIORef shouldQuitRef
    unless shouldQuit $ do
        -- Update game logic based on current key states and delta time
        updateGameLogic rectPosRef deltaTimeRef keyStates

        -- Render the scene
        renderFrame renderer rectPosRef

        -- Continue loop
        eventLoop window renderer currentTime freq deltaTimeRef rectPosRef shouldQuitRef keyStates

-- | Process all pending events from the queue for the current frame
processEvents :: IORef Bool -> KeyStates -> IO ()
processEvents shouldQuitRef keyStates = do
    maybeEvent <- pollEventWithImGui
    case maybeEvent of
        Nothing -> return () -- No more events in queue for this frame
        Just event -> do
            -- Handle the current event
            quitSignalFromEvent <- handleSingleEvent event keyStates -- Renamed from handleEvent to avoid clash
            when quitSignalFromEvent $ writeIORef shouldQuitRef True

            -- Check if we should continue processing events (e.g., if quit wasn't signaled)
            currentQuitState <- readIORef shouldQuitRef
            unless currentQuitState $
                processEvents shouldQuitRef keyStates -- Recursively process next event

-- | Handle a single SDL event, updating key states. Returns True if this event signals a quit.
handleSingleEvent :: SDLEvent -> KeyStates -> IO Bool
handleSingleEvent event (upRef, downRef, leftRef, rightRef) = case event of
    SDLEventQuit _ -> do
        sdlLog "Quit event received."
        return True
    SDLEventKeyboard ke -> do
        let scancode = sdlKeyboardScancode ke
        let isKeyDown = sdlKeyboardDown ke
        let eventType = sdlKeyboardType ke
        let isRepeat = sdlKeyboardRepeat ke

        sdlLog $
            printf
                "Keyboard Event: Type: %s, Scancode: %s, isKeyDown: %s, Repeat: %s"
                (show eventType)
                (show scancode)
                (show isKeyDown)
                (show isRepeat)

        -- Update IORefs based on key state
        case scancode of
            SDL_SCANCODE_Q ->
                if isKeyDown
                    then do
                        -- Quit only on Q press
                        sdlLog "Q pressed, signaling quit."
                        return True
                    else
                        return False
            SDL_SCANCODE_UP -> writeIORef upRef isKeyDown >> return False
            SDL_SCANCODE_DOWN -> writeIORef downRef isKeyDown >> return False
            SDL_SCANCODE_LEFT -> writeIORef leftRef isKeyDown >> return False
            SDL_SCANCODE_RIGHT -> writeIORef rightRef isKeyDown >> return False
            _ -> return False -- Other scancodes don't signal quit by default
    _ -> return False -- Other event types don't signal quit by default

-- | Update game state (like rectangle position) based on current input states and delta time
updateGameLogic :: IORef SDLFPoint -> IORef Double -> KeyStates -> IO ()
updateGameLogic rectPosRef deltaTimeRef (upRef, downRef, leftRef, rightRef) = do
    dtSec <- readIORef deltaTimeRef -- Delta time of the frame in seconds
    let moveSpeed = 200.0 -- Pixels per second
    let moveAmount = realToFrac (moveSpeed * dtSec)

    -- Read current key states
    up <- readIORef upRef
    down <- readIORef downRef
    left <- readIORef leftRef
    right <- readIORef rightRef

    -- Optional: Log states if debugging movement
    -- sdlLog $ printf "updateGameLogic: up:%s, down:%s, left:%s, right:%s, dt:%.4fs, move:%.3f"
    --                 (show up) (show down) (show left) (show right) dtSec moveAmount

    SDLFPoint currentX currentY <- readIORef rectPosRef
    let newX | left = currentX - moveAmount | right = currentX + moveAmount | otherwise = currentX
    let newY | up = currentY - moveAmount | down = currentY + moveAmount | otherwise = currentY

    when (newX /= currentX || newY /= currentY) $
        writeIORef rectPosRef (SDLFPoint newX newY)

-- | Render a single frame
renderFrame :: SDLRenderer -> IORef SDLFPoint -> IO ()
renderFrame renderer rectPosRef = do
    sdl3RendererNewFrame
    sdl3NewFrame
    newFrame

    showDemoWindow

    -- 1. Set draw color to clear color (e.g., dark blue) and clear
    _ <- sdlSetRenderDrawColor renderer 32 32 64 255
    clearSuccess <- sdlRenderClear renderer
    unless clearSuccess $ sdlLog "Warning: Failed to clear renderer"

    -- 2. Set draw color for rectangle (e.g., yellow)
    _ <- sdlSetRenderDrawColor renderer 255 255 0 255

    -- 3. Get current rectangle position
    (SDLFPoint x y) <- readIORef rectPosRef

    -- 4. Define rectangle geometry
    let rect = SDLFRect x y 50 50 -- x, y, width, height

    -- 5. Draw the filled rectangle
    fillRectSuccess <- sdlRenderFillRect renderer (Just rect)
    unless fillRectSuccess $ sdlLog "Warning: Failed to draw filled rect"

    render
    sdl3RendererRenderDrawData renderer =<< getDrawData

    -- 6. Present the rendered frame
    presentSuccess <- sdlRenderPresent renderer
    unless presentSuccess $ do
        err <- sdlGetError
        sdlLog $ "Warning: Failed to present renderer: " ++ err

-- Helper function to print subsystem names
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag =
    sdlLog $
        "  - " ++ case flag of
            SDL_INIT_AUDIO -> "Audio"
            SDL_INIT_VIDEO -> "Video"
            SDL_INIT_JOYSTICK -> "Joystick"
            SDL_INIT_HAPTIC -> "Haptic"
            SDL_INIT_GAMEPAD -> "Gamepad"
            SDL_INIT_EVENTS -> "Events"
            SDL_INIT_SENSOR -> "Sensor"
            SDL_INIT_CAMERA -> "Camera"
            _ -> "Unknown subsystem"

data Refs = Refs
    { refsShowDemoWindow :: IORef Bool
    , refsShowAnotherWindow :: IORef Bool
    , refsFloat :: IORef Float
    , refsCounter :: IORef Int
    }

newRefs :: IO Refs
newRefs =
    Refs
        <$> newIORef True
        <*> newIORef False
        <*> newIORef 0
        <*> newIORef 0

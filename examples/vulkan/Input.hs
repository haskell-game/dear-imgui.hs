module Input where

-- base
import Data.Int
  ( Int32 )

-- sdl2
import qualified SDL

--------------------------------------------------------------------------------

data Input = Input
  { keysDown    :: [ SDL.Scancode ]
  , keysPressed :: [ SDL.Scancode ]
  , mousePos    :: ( Int32, Int32 )
  , mouseRel    :: ( Int32, Int32 )
  , quitAction  :: Bool
  }

nullInput :: Input
nullInput
  = Input
    { keysDown    = []
    , keysPressed = []
    , mousePos    = ( 0, 0 )
    , mouseRel    = ( 0, 0 )
    , quitAction  = False
    }

onSDLInput :: Input -> SDL.EventPayload -> Input
onSDLInput input SDL.QuitEvent
  = input { quitAction = True }
onSDLInput input (SDL.WindowClosedEvent _)
  = input { quitAction = True }
onSDLInput input ( SDL.KeyboardEvent ev )
  = let keyCode = SDL.keysymScancode ( SDL.keyboardEventKeysym ev )
    in case SDL.keyboardEventKeyMotion ev of
         SDL.Pressed  -> input { keysDown    = keyCode : filter ( /= keyCode ) ( keysDown    input )
                               , keysPressed = keyCode : filter ( /= keyCode ) ( keysPressed input )
                               }
         SDL.Released -> input { keysDown    =           filter ( /= keyCode ) ( keysDown    input ) }
onSDLInput input ( SDL.MouseMotionEvent ev )
  = input { mousePos = (px, py)
          , mouseRel = (rx, ry)
          }
    where
      SDL.P ( SDL.V2 px py ) = SDL.mouseMotionEventPos       ev
      SDL.V2         rx ry   = SDL.mouseMotionEventRelMotion ev
onSDLInput input _ = input

onSDLInputs :: Input -> [ SDL.EventPayload ] -> Input
onSDLInputs prevInput events = escapeQuits $ foldl onSDLInput zeroedInput events
  where
    zeroedInput :: Input
    zeroedInput = prevInput { keysPressed = [], mouseRel = ( 0, 0 ) }
    escapeQuits :: Input -> Input
    escapeQuits input
      | SDL.ScancodeEscape `elem` keysPressed input
      = input { quitAction = True }
      | otherwise
      = input

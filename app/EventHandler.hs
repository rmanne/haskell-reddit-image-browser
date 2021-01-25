module EventHandler
  ( ViewAction(..)
  , processEvent
  ) where

import Control.Concurrent.Chan (Chan, writeChan)
import Data.IORef (IORef, atomicModifyIORef, writeIORef)
import Foreign.C.Types (CInt)
import qualified SDL
import Types

data ViewAction
  = ResizeWindow CInt CInt
  | Quit

processEvent :: Chan Command -> SDL.Event -> IO (Maybe ViewAction)
processEvent chan event =
  processEventPayload chan (SDL.eventPayload event)

processEventPayload ::
     Chan Command -> SDL.EventPayload -> IO (Maybe ViewAction)
processEventPayload _ (SDL.WindowSizeChangedEvent SDL.WindowSizeChangedEventData {SDL.windowSizeChangedEventSize = SDL.V2 width height}) =
  return $ Just $ ResizeWindow (fromIntegral width) (fromIntegral height)
processEventPayload _ SDL.QuitEvent =
  return $ Just Quit
processEventPayload controllerChannel (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = keysym
                                                                                             , SDL.keyboardEventKeyMotion = SDL.Released
                                                                                             }) =
  case SDL.keysymModifier keysym of
    SDL.KeyModifier { SDL.keyModifierLeftShift = False
                    , SDL.keyModifierRightShift = False
                    , SDL.keyModifierLeftCtrl = False
                    , SDL.keyModifierRightCtrl = False
                    , SDL.keyModifierLeftAlt = False
                    , SDL.keyModifierRightAlt = False
                    , SDL.keyModifierLeftGUI = False
                    , SDL.keyModifierRightGUI = False
                    , SDL.keyModifierNumLock = False
                    , SDL.keyModifierCapsLock = False
                    , SDL.keyModifierAltGr = False
                    } ->
      case SDL.keysymKeycode keysym of
        SDL.KeycodeQ -> return $ Just Quit
        SDL.KeycodeJ -> send Next
        SDL.KeycodeK -> send Prev
        SDL.KeycodeS -> send Toggle
        SDL.KeycodeD -> send Remove
        SDL.KeycodeW -> send Save
        SDL.KeycodeV -> send ToggleDeleted
        SDL.KeycodeR -> send Refresh
        SDL.KeycodeG -> send Front
        SDL.KeycodeC -> send Commit
        SDL.KeycodeF -> send FindFailed
        SDL.KeycodeA -> send Status
        SDL.KeycodeL -> send NextImage
        SDL.KeycodeH -> send PrevImage
        SDL.Keycode0 -> send (Multi 0)
        SDL.Keycode1 -> send (Multi 1)
        SDL.Keycode2 -> send (Multi 2)
        SDL.Keycode3 -> send (Multi 3)
        SDL.Keycode4 -> send (Multi 4)
        SDL.Keycode5 -> send (Multi 5)
        SDL.Keycode6 -> send (Multi 6)
        SDL.Keycode7 -> send (Multi 7)
        SDL.Keycode8 -> send (Multi 8)
        SDL.Keycode9 -> send (Multi 9)
        _ -> return Nothing
    SDL.KeyModifier { SDL.keyModifierLeftCtrl = False
                    , SDL.keyModifierRightCtrl = False
                    , SDL.keyModifierLeftAlt = False
                    , SDL.keyModifierRightAlt = False
                    , SDL.keyModifierLeftGUI = False
                    , SDL.keyModifierRightGUI = False
                    , SDL.keyModifierNumLock = False
                    , SDL.keyModifierCapsLock = False
                    , SDL.keyModifierAltGr = False
                    } ->
      case SDL.keysymKeycode keysym of
        SDL.KeycodeG -> send Back
        _ -> return Nothing
    _ -> return Nothing
  where
    send command = writeChan controllerChannel command >> return Nothing
processEventPayload _ _ = return Nothing

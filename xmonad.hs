import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import System.IO
import qualified Data.Map as M

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ defaultConfig 
    { manageHook = manageDocks <+> manageHook defaultConfig
    -- , layoutHook = avoidStruts  $ layoutHook defaultConfig
    , layoutHook = avoidStruts $ spacing 2$ smartBorders $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "grey" "" . shorten 50
      }
    , borderWidth = 1
    , normalBorderColor = "#cccccc"
    , focusedBorderColor = "#ccbb00"
    , modMask = mod1Mask
    } `additionalKeys`
    [ ((mod1Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod1Mask .|. shiftMask, xK_e), spawn "emacsclient -c")
    ]

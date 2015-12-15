import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
  where h = 0.1
        w = 0.1
        t = 0.1 
        l = 0.1

myManageHook = manageDocks
  <+> manageScratchpad
  <+> manageHook defaultConfig

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ defaultConfig 
    { manageHook = myManageHook
    , layoutHook = avoidStruts $ spacing 2 $ smartBorders $ layoutHook defaultConfig
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
    , ((mod1Mask .|. shiftMask, xK_s), scratchpadSpawnActionTerminal "xclock")
    ]

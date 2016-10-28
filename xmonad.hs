import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
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

myUrgencyHook = NoUrgencyHook

myManageHook = manageDocks
  <+> (className =? "X64" --> doFloat)
  <+> manageScratchpad
  <+> manageHook defaultConfig

myTerminal = "urxvt -bg black"
myFont = "Inconsolata-16"
mySpacing = 8

myGSConfig = defaultGSConfig { gs_cellwidth = 250 }

myPPLayout l = case l of
                 spacingTall   -> "[|]"
                 spacingMirror -> "[-]"
                 spacingTabbed -> "[_]"
                 spacingFull   -> "[ ]"
                 _             -> l
  where mySpacingString = show mySpacing
        spacingTall     = ("Spacing " ++ mySpacingString ++ " Tall")
        spacingMirror   = ("Spacing " ++ mySpacingString ++ " Mirror Tall")
        spacingTabbed   = ("Spacing " ++ mySpacingString ++ " Mirror Tabbed")
        spacingFull     = ("Spacing " ++ mySpacingString ++ " Full")

myLayouts = (Tall 1 (3/100) (80/100))
  ||| Mirror (Tall 1 (3/100) (60/100))
  ||| Full

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ withUrgencyHook myUrgencyHook $ defaultConfig 
    { manageHook = myManageHook
    , layoutHook = avoidStruts $ spacing mySpacing $ smartBorders $ toggleLayouts Full myLayouts -- layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "grey" "" . shorten 25
      , ppUrgent = xmobarColor "red" ""
      , ppSort = DO.getSortByOrder
      , ppLayout = myPPLayout
      }
    , terminal = myTerminal
    , borderWidth = 1
    , normalBorderColor = "#cccccc"
    , focusedBorderColor = "yellow"
    -- , focusedBorderColor = "#ff0000"
    , modMask = mod4Mask
    }
    `additionalKeysP`
      ( [ ("M-S-z", spawn "xscreensaver-command -lock")
        , ("M-S-e", spawn "emacsclient -c")
        , ("M-S-s", spawn "scrot -e 'mv $f ~/Pictures/screenshots/'")
        , ("M-S-w", spawn "scrot -s -e 'mv $f ~/Pictures/screenshots/'")
        , ("M-S-t", spawn "/usr/bin/curl --user tiro:JF4NabjScClXkgx 192.168.20.9/tiro.py")
        , ("M-p",   spawn "dmenu_run -fn 'Droid Sans Mono-16'")
        , ("M-<R>", DO.moveTo Next HiddenNonEmptyWS)
        , ("M-<L>", DO.moveTo Prev HiddenNonEmptyWS)

        -- GridSelect
        , ("M-g",   goToSelected myGSConfig)
        -- WindowBringer
        , ("M-b",     gotoMenuArgs ["-fn", myFont])
        , ("M-S-b",   bringMenuArgs ["-fn", myFont])
        , ("M-C-<R>", DO.swapWith Next NonEmptyWS)
        , ("M-C-<L>", DO.swapWith Prev NonEmptyWS)
        ] 
      ++ 
        map (\ (key, event) -> (key, spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ event))
        [ ("<XF86AudioPlay>", "PlayPause")
        , ("<XF86AudioStop>", "Stop")
        , ("<XF86AudioPrev>", "Previous")
        , ("<XF86AudioNext>", "Next") ] )

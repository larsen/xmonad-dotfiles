import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
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

myUrgencyHook = NoUrgencyHook

myManageHook = manageDocks
  <+> manageScratchpad
  <+> manageHook defaultConfig

myTerminal = "urxvt -bg black"
-- myTerminal = "mate-terminal"
myFont = "Inconsolata-16"

myGSConfig = defaultGSConfig { gs_cellwidth = 250 }

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ withUrgencyHook myUrgencyHook $ defaultConfig 
    { manageHook = myManageHook
    , layoutHook = avoidStruts $ spacing 2 $ smartBorders $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "grey" "" . shorten 25
      , ppUrgent = xmobarColor "red" ""
      , ppSort = DO.getSortByOrder
      , ppLayout = (\layout -> case layout of
                                 "Spacing 2 Tall"        -> "[|]"
                                 "Spacing 2 Mirror Tall" -> "[-]"
                                 "Spacing 2 Tabbed"      -> "[_]"
                                 "Spacing 2 Full"        -> "[ ]"
                                 _                       -> layout
                                 )           
      }
    , terminal = myTerminal
    , borderWidth = 1
    , normalBorderColor = "#cccccc"
    , focusedBorderColor = "cadetblue"
    -- , focusedBorderColor = "#ff0000"
    , modMask = mod4Mask
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_e), spawn "emacsclient -c")
    , ((mod4Mask .|. shiftMask, xK_s), spawn "scrot -e 'mv $f ~/Pictures/screenshots/'")
    , ((mod4Mask,               xK_p), spawn "dmenu_run -fn 'Droid Sans Mono-16'")
      -- 
    , ((mod4Mask,               xK_Right), DO.moveTo Next HiddenNonEmptyWS)
    , ((mod4Mask,               xK_Left),  DO.moveTo Prev HiddenNonEmptyWS)
      -- GridSelect
    , ((mod4Mask,               xK_g ), goToSelected myGSConfig)
      -- WindowBringer
    , ((mod4Mask,               xK_b ), gotoMenuArgs ["-fn", myFont])
    , ((mod4Mask .|. shiftMask, xK_b ), bringMenuArgs ["-fn", myFont])      

    , ((mod4Mask .|. controlMask,  xK_Right), DO.swapWith Next NonEmptyWS)
    , ((mod4Mask .|. controlMask,  xK_Left), DO.swapWith Prev NonEmptyWS)

    -- , ("M-S-<R>",   DO.shiftTo Next HiddenNonEmptyWS)
    -- , ("M-S-<L>",   DO.shiftTo Prev HiddenNonEmptyWS)
    -- , ("M-<R>",     DO.moveTo Next HiddenNonEmptyWS)
    -- , ("M-<L>",     DO.moveTo Prev HiddenNonEmptyWS)

    -- , ((mod4Mask .|. shiftMask, xK_s), scratchpadSpawnActionTerminal "")
    ]
    `additionalKeysP`
      map (\ (key, event) -> (key, spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ event))
        [ ("<XF86AudioPlay>", "PlayPause")
        , ("<XF86AudioStop>", "Stop")
        , ("<XF86AudioPrev>", "Previous")
        , ("<XF86AudioNext>", "Next") ]

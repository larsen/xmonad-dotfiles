import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
-- import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.PerScreen
import XMonad.Layout.Gaps
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import XMonad.Util.NamedScratchpad
import System.IO
import Data.List
import qualified Data.Map as M
import qualified XMonad.StackSet as W

scratchpads = [
  -- run htop in xterm, find it by title, use default floating window placement
  NS "htop" "xterm -e htop" (title =? "htop")
    (customFloating $ W.RationalRect l t w h),
  NS "glances" "urxvt -e glances" (title =? "glances")
    (customFloating $ W.RationalRect l t w h),
  NS "term" (myTerminal ++ " -name scratchpadTerm") (title =? "scratchpadTerm")
    (customFloating $ W.RationalRect l t w h)
  ]
  where role = stringProperty "WM_WINDOW_ROLE"
        h = 9/10
        w = 7/10
        t = 5/100
        l = 15/100

myUrgencyHook = NoUrgencyHook

myManageHook = manageDocks
  <+> (className =? "Xfce4-notifyd" --> doIgnore)
  <+> (className =? "X64" --> doFloat)
  <+> (className =? "vlc" --> doFloat)
  <+> (className =? "gimp-2.8" --> doFloat)
  <+> (className =? "XClock" --> doIgnore)
  <+> (title =? "popup-frame" --> doFloat <+> doF W.focusDown)
  <+> (title =? "CEPL" --> doFloat <+> doF W.focusDown)
  <+> (title =? "CLIM" --> doFloat <+> doF W.focusDown)
  <+> (title =? "Listener" --> doFloat <+> doF W.focusDown)
  <+> (title =? "Stella-App" --> doFloat <+> doF W.focusDown)
  <+> namedScratchpadManageHook scratchpads
  <+> manageHook def

-- myTerminal = "urxvt -bg black"
myTerminal = "~/.local/kitty.app/bin/kitty"
myFont = "Inconsolata-16"
mySpacing = 10 
myGaps = gaps [(U,18)]

myPPLayout l
  | isSuffixOf "Mirror Tall" l = "[-]" 
  | isSuffixOf "Tall" l = "[|]" 
  | isSuffixOf "smartTall" l = "[s]" 
  | isSuffixOf "Full" l = "[ ]" 
  | isSuffixOf "ThreeCol" l = "[‖]"
  | isSuffixOf "Tabbed Tall" l = "[T]"
  | isSuffixOf "NoFrillsDecoration" l = "[ ]"
  | otherwise = (take 10 l)

myLayouts = windowNavigation $ subTabbed $
  smartTall
  ||| bottomSide
  ||| Full
  ||| ThreeColMid 1 (3/100) (2/3)
  ||| Grid
  where rightSide = (Tall 1 (3/100) (80/100))
        bottomSide = Mirror (Tall 1 (3/100) (60/100))
        smartTall = ifWider 1080 rightSide bottomSide

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green       = "#859900"

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

topbar      = 10

topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

addTopBar = noFrillsDeco shrinkText topBarTheme

main = do
  xmproc <- spawnPipe "/home/stefano/.local/bin/xmobar"

  xmonad $ withUrgencyHook myUrgencyHook $ ewmh def
    { manageHook = myManageHook
    , layoutHook = avoidStruts
                   $ addTopBar
                   $ myGaps
                   $ spacing mySpacing
                   $ smartBorders
                   $ addTabs shrinkText myTabTheme
                   $ toggleLayouts Full myLayouts -- layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "grey" "" . shorten 25
      , ppCurrent = xmobarColor "yellow" ""
      , ppVisible = xmobarColor "lightblue" ""
      , ppUrgent = xmobarColor "red" ""
      , ppSort = DO.getSortByOrder
      , ppLayout = myPPLayout
      , ppOrder = \(ws:_:t:_) -> [ws,t]
      , ppSep = "‧"
      }
    , terminal = myTerminal
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , borderWidth = 0
    , modMask = mod4Mask
    }
    `additionalKeysP`
      ( [ ("M-S-z", spawn "xscreensaver-command -lock")
        , ("M-S-e", spawn "emacsclient -c")
        , ("M-S-t", spawn "~/bin/capture.sh")
        , ("M-S-a", spawn "~/bin/capture-anki.sh")
        , ("M-S-p", spawn "~/bin/passmenu")
        , ("M-v",   spawn "clip2capture")
        , ("M-S-s", spawn "scrot -e 'mv $f ~/Pictures/screenshots/'")
        , ("M-S-w", spawn "scrot -s -e 'mv $f ~/Pictures/screenshots/'")
        , ("M-p",   spawn "rofi -show run")

        , ("M-C-t", namedScratchpadAction scratchpads "glances")
        , ("M-o",   namedScratchpadAction scratchpads "term")
        
        , ("M-<R>", DO.moveTo Next HiddenNonEmptyWS)
        , ("M-<L>", DO.moveTo Prev HiddenNonEmptyWS)

        , ("M-C-h", sendMessage $ pullGroup L) -- h
        , ("M-C-l", sendMessage $ pullGroup R) -- l
        , ("M-C-k", sendMessage $ pullGroup U) -- k
        , ("M-C-j", sendMessage $ pullGroup D) -- j
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-,", onGroup W.focusUp')
        , ("M-C-.", onGroup W.focusDown')

        -- GridSelect
        , ("M-g",   spawn "rofi -show window")
        -- WindowBringer
        , ("M-b",     gotoMenuArgs ["-fn", myFont])
        , ("M-S-b",   bringMenuArgs ["-fn", myFont])
        , ("M-C-<R>", DO.swapWith Next NonEmptyWS)
        , ("M-C-<L>", DO.swapWith Prev NonEmptyWS)
        
        , ("<XF86AudioRaiseVolume>", spawn $ "pactl set-sink-volume 0 +10%")
        , ("<XF86AudioLowerVolume>", spawn $ "pactl set-sink-volume 0 -10%")        
        , ("<XF86AudioMute>", spawn $ "pactl set-sink-mute 0 toggle")        
        , ("<XF86MonBrightnessUp>", spawn $ "xbacklight +10")
        , ("<XF86MonBrightnessDown>", spawn $ "xbacklight -10")
        ] 
      ++ 
        map (\ (key, event) -> (key, spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ event))
        [ ("<XF86AudioPlay>", "PlayPause")
        , ("<XF86AudioStop>", "Stop")
        , ("<XF86AudioPrev>", "Previous")
        , ("<XF86AudioNext>", "Next") ] )

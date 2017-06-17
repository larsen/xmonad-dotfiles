import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import XMonad.Util.NamedScratchpad
import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W

scratchpads = [
  -- run htop in xterm, find it by title, use default floating window placement
  NS "htop" "xterm -e htop" (title =? "htop") (customFloating $ W.RationalRect l t w h),

  NS "term" (myTerminal ++ " -name scratchpadTerm") (title =? "scratchpadTerm") (customFloating $ W.RationalRect l t w h)
  ]
  where role = stringProperty "WM_WINDOW_ROLE"
        h = 0.9
        w = 0.7
        t = 0.05
        l = 0.15

myUrgencyHook = NoUrgencyHook

myManageHook = manageDocks
  <+> (className =? "X64" --> doFloat)
  <+> (className =? "XClock" --> doFloat)
  <+> (title =? "popup-frame" --> doFloat <+> doF W.focusDown)
  <+> namedScratchpadManageHook scratchpads
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

myLayouts = windowNavigation $ subTabbed $
  (Tall 1 (3/100) (80/100))
  ||| Mirror (Tall 1 (3/100) (60/100))
  ||| Full
  ||| ThreeColMid 1 (3/100) (2/3)

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

addTopBar = noFrillsDeco shrinkText topBarTheme

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ withUrgencyHook myUrgencyHook $ ewmh defaultConfig 
    { manageHook = myManageHook
    , layoutHook = avoidStruts
                   $ spacing mySpacing
                   $ smartBorders
                   $ addTopBar
                   $ toggleLayouts Full myLayouts -- layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "grey" "" . shorten 25
      , ppUrgent = xmobarColor "red" ""
      , ppSort = DO.getSortByOrder
      , ppLayout = myPPLayout
      }
    , terminal = myTerminal
    , borderWidth = 0
    , modMask = mod4Mask
    }
    `additionalKeysP`
      ( [ ("M-S-z", spawn "xscreensaver-command -lock")
        , ("M-S-e", spawn "emacsclient -c")
        , ("M-S-f", spawn "bash -c \"source ~/bin/functions.sh && toggle_touchpad\"")
        , ("M-S-s", spawn "scrot -e 'mv $f ~/Pictures/screenshots/'")
        , ("M-S-w", spawn "scrot -s -e 'mv $f ~/Pictures/screenshots/'")
        , ("M-S-t", spawn "/usr/bin/curl --user tiro:JF4NabjScClXkgx 192.168.20.9/tiro.py")
        , ("M-p",   spawn "dmenu_run -fn 'Droid Sans Mono-16'")

        , ("M-C-t", namedScratchpadAction scratchpads "htop")
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

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
import XMonad.Layout.NoBorders ( smartBorders, lessBorders, SetsAmbiguous ( hiddens ) )
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
-- Order screens by physical location
import XMonad.Actions.PhysicalScreens
import XMonad.Layout.PerScreen
import XMonad.Layout.Gaps
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import XMonad.Util.NamedScratchpad
-- import qualified XMonad.Util.Hacks as Hacks
import System.IO
import Data.List
import Data.Default
import qualified Data.Map as M
import qualified XMonad.StackSet as W

scratchpads = [
  -- run htop in xterm, find it by title, use default floating window placement
  NS "htop" "xterm -e htop" (title =? "htop")
    (customFloating $ W.RationalRect l t w h),
  NS "pulsemixer" "urxvt -e pulsemixer" (title =? "pulsemixer")
    (customFloating $ W.RationalRect l t w h),
  NS "glances" "urxvt -e glances" (title =? "glances")
    (customFloating $ W.RationalRect l t w h),
  -- NS "spotify" "spotify" (className =? "spotify")
  --   (customFloating $ W.RationalRect l t w h),
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
  <+> (title =? "Ediff" --> doFloat)
  <+> (className =? "gimp-2.8" --> doFloat)
  <+> (className =? "XClock" --> doIgnore)
  <+> (className =? "Xephyr" --> doIgnore)
  <+> (title =? "Synacor Debugger" --> doFloat)
  <+> (title =? "Melody" --> doFloat)
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
mySpacing = 5
myGaps = gaps [(U,8)]

myPPLayout l
  | "Mirror Tall" `isSuffixOf` l = "[-]"
  | "Tall" `isSuffixOf` l = "[|]"
  | "smartTall" `isSuffixOf` l = "[s]"
  | "Full" `isSuffixOf` l = "[ ]"
  | "ThreeCol" `isSuffixOf` l = "[‖]"
  | "Tabbed Tall" `isSuffixOf` l = "[T]"
  | "NoFrillsDecoration" `isSuffixOf` l = "[ ]"
  | otherwise = take 10 l

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myLayouts = windowNavigation $ subTabbed $
  smartTall
  ||| bottomSide
  ||| tabbed shrinkText myTabTheme
  ||| ThreeColMid 1 (3/100) (2/3)
  ||| Grid
  where rightSide = Tall 1 (3 / 100) (80 / 100)
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

addTopBar = noFrillsDeco shrinkText topBarTheme

main = xmonad $ docks $ withUrgencyHook myUrgencyHook $ ewmh def
    { manageHook = myManageHook
    , layoutHook = avoidStruts
                   $ addTopBar
                   $ spacing mySpacing
                   $ smartBorders
                   $ addTabs shrinkText myTabTheme
                   $ toggleLayouts Full myLayouts -- layoutHook defaultConfig
    , terminal = myTerminal
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , borderWidth = 1
    , focusedBorderColor = blue
    , normalBorderColor = "#000000"
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
        , ("M-C-m", namedScratchpadAction scratchpads "pulsemixer")

        , ("M-<R>", DO.moveTo Next HiddenNonEmptyWS)
        , ("M-<L>", DO.moveTo Prev HiddenNonEmptyWS)

        , ("M-C-h", sendMessage $ pullGroup L) -- h
        , ("M-C-l", sendMessage $ pullGroup R) -- l
        , ("M-C-k", sendMessage $ pullGroup U) -- k
        , ("M-C-j", sendMessage $ pullGroup D) -- j
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-,", onGroup W.focusUp')
        , ("M-C-.", onGroup W.focusDown')

        , ("M-<F1>", spawn "sxiv -q -t -r ~/Pictures/wallpapers")
        , ("M-<F2>", spawn "urxvt -e ~/bin/db")

        -- GridSelect
        , ("M-g",   spawn "rofi -show window")
        -- WindowBringer
        , ("M-b",     gotoMenuArgs ["-fn", myFont])
        , ("M-S-b",   bringMenuArgs ["-fn", myFont])
        , ("M-C-<R>", DO.swapWith Next NonEmptyWS)
        , ("M-C-<L>", DO.swapWith Prev NonEmptyWS)

        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86MonBrightnessUp>", spawn "xbacklight +10")
        , ("<XF86MonBrightnessDown>", spawn "xbacklight -10")
        ]
      ++
      -- https://xiangji.me/2018/11/19/my-xmonad-configuration/#xmonadactionsphysicalscreens
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      --   [((modm .|. mask, key), f sc)
      --  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      --  , (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]
        [ ("M-w", viewScreen def 0)
        , ("M-e", viewScreen def 1)
        , ("M-r", viewScreen def 2)
        ]
      ++ 
        map (\ (key, event) -> (key, spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ event))
        [ ("<XF86AudioPlay>", "PlayPause")
        , ("<XF86AudioStop>", "Stop")
        , ("<XF86AudioPrev>", "Previous")
        , ("<XF86AudioNext>", "Next") ] )

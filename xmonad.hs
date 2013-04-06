import XMonad
-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle.Instances hiding (FULL, NBFULL, NOBORDERS, SMARTBORDERS)

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Actions.GridSelect
-- WINDOW RULES
import XMonad.ManageHook
-- KEYBOARD & MOUSE CONFIG
import XMonad.Util.EZConfig
import XMonad.Actions.FloatKeys
import Graphics.X11.ExtraTypes.XF86
-- STATUS BAR
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Dmenu
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import System.IO (hPutStrLn)
--import XMonad.Operations
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS           -- nextWS, prevWS
import Data.List                        -- clickable workspaces

-- MULTIMONITOR
import XMonad.Actions.PhysicalScreens

defaultLayouts =          onWorkspace (myWorkspaces !! 0) (avoidStruts (Circle ||| tiled) ||| fullTile)
                        $ onWorkspace (myWorkspaces !! 1) (avoidStruts (Circle ||| noBorders (fullTile)) ||| fullScreen)
                        $ onWorkspace (myWorkspaces !! 2) (avoidStruts simplestFloat)
                        $ onWorkspace (myWorkspaces !! 4) (avoidStruts fullScreen)
                        $ avoidStruts ( mkToggle (single REFLECTX) $ mkToggle (single MIRROR) ( tiledSpace  ||| tiled ||| goldenSpiral ||| Circle ||| mosaic )) ||| fullScreen
        where
                tiled            = spacing 5 $ ResizableTall nmaster delta ratio []
                tiledSpace       = spacing 60 $ ResizableTall nmaster delta ratio []
                fullScreen       = noBorders(fullscreenFull Full)
                fullTile         = ResizableTall nmaster delta ratio []
                borderlessTile   = noBorders(fullTile)
                mosaic           = spacing 5 $ MosaicAlt M.empty
                fullGoldenSpiral = spiral ratio
                goldenSpiral     = spacing 5 $ spiral ratio
                -- Default number of windows in master pane
                nmaster = 1
                -- Percent of the screen to increment when resizing
                delta   = 5/100
                -- Default proportion of the screen taken up by main pane
                ratio   = toRational (2/(1 + sqrt 5 :: Double))

-- Give some workspaces no borders
nobordersLayout = noBorders $ Full
myLayout = defaultLayouts

-- Declare workspaces and rules for applications

myWorkspaces = clickable $
                ["^i(/home/genesis/.xmonad/icons/mail.xbm) alpha"
                ,"^i(/home/genesis/.xmonad/icons/globe.xbm) beta"
                ,"^i(/home/genesis/.xmonad/icons/shell.xbm) gamma"
                ,"^i(/home/genesis/.xmonad/icons/monitor.xbm) delta"
                ,"^i(/home/genesis/.xmonad/icons/calc.xbm) epsilon"
                ,"^i(/home/genesis/.xmonad/icons/text.xbm) stigma"
                ,"^i(/home/genesis/.xmonad/icons/picture.xbm) zeta"
                ,"^i(/home/genesis/.xmonad/icons/movie.xbm) eta"
                ,"^i(/home/genesis/.xmonad/icons/doc.xbm) theta"]
        where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                                    (i,ws) <- zip [1..] l,
                                    let n = i ]

myManageHook = composeAll       [ resource =? "dmenu"    --> doFloat
                                , resource =? "skype"    --> doFloat
                                , resource =? "feh"      --> doFloat
                                , resource =? "MATLAB"   --> doShift (myWorkspaces !! 4)
                                , (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
                                , resource =? "chromium" --> doShift (myWorkspaces !! 1)
                                , resource =? "zathura"  --> doShift (myWorkspaces !! 8)
--                                , isFullscreen --> (doF W.focusDown <+> doFullFloat)
                                ]
                        where role = stringProperty "WM_WINDOW_ROLE"

newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks

myLogHook h = dynamicLogWithPP ( defaultPP
        {
                  ppCurrent             = dzenColor fg_red background . pad
                , ppVisible             = dzenColor fg_blue background . pad
                , ppHidden              = dzenColor white0 background . pad
                , ppHiddenNoWindows     = dzenColor black0 background . pad
                , ppWsSep               = ""
                , ppSep                 = "    "
                , ppLayout              = wrap "^ca(1,xdotool key super+space)" "^ca()" . dzenColor white1 background .
                                (\x -> case x of
                                        "Full"                           ->      "^i(/home/genesis/.xmonad/icons/monitor.xbm)"
                                        "Spacing 5 ResizableTall"        ->      "^i(/home/genesis/.xmonad/icons/layout.xbm)"
                                        "ResizableTall"                  ->      "^i(/home/genesis/.xmonad/icons/layout_tall.xbm)"
                                        "SimplestFloat"                  ->      "^i(/home/genesis/.xmonad/icons/layers.xbm)"
                                        "Spacing 5 Spiral"               ->      "^i(/home/genesis/.xmonad/icons/spiral.xbm)"
                                        "Circle"                         ->      "^i(/home/genesis/.xmonad/icons/circle.xbm)"
                                        _                                ->      "^i(/home/genesis/.xmonad/icons/grid3x3.xbm)"
                                )
                , ppOrder       =  \(ws:l:t:_) -> [ws,l]
                , ppOutput      =   hPutStrLn h
        } )

myXmonadBar = "dzen2 -x '1920' -y '0' -h '25' -w '700' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -qc /home/genesis/.xmonad/.conky_dzen | dzen2 -xs 3 -x '700' -y '0' -w '1220' -h '25' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -fn "++myFont
--myConky = "conky -c /home/genesis/conkyrc"

main = do
        dzenLeftBar     <- spawnPipe myXmonadBar
        dzenRightBar    <- spawnPipe myStatusBar
--      conky           <- spawn myConky
        xmonad $ ewmh defaultConfig
                { terminal              = myTerminal
                , borderWidth           = 1
                , normalBorderColor     = black0
                , focusedBorderColor    = fg_magenta
                , modMask               = mod4Mask
                , layoutHook            = smartBorders(myLayout)
--              , layoutHook            = avoidStruts  $  layoutHook defaultConfig
                , workspaces            = myWorkspaces
                , manageHook            = newManageHook
--              , manageHook            = manageDocks <+> manageHook defaultConfig
                , handleEventHook       = fullscreenEventHook <+> docksEventHook
                , startupHook           = ewmhDesktopsStartup >> setWMName "LG3D"
                , logHook               = myLogHook dzenLeftBar >> setWMName "LG3D" -- >> fadeInactiveLogHook 0xdddddddd
                }
                `additionalKeys`
                [((mod4Mask .|. shiftMask       , xK_x), kill)
                ,((mod4Mask .|. shiftMask       , xK_r), spawn "dmenu_run -h '25' -nb '#000000' -nf '#8ebdde' -sb '#7788af' -sf '#ffffff' -fn 'PragmataPro-12'")
                ,((mod4Mask                     , xK_q), spawn "killall dzen2; killall conky; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
                -- Window and Program sittings for Dvorak Layout
                ,((mod4Mask                     , xK_apostrophe), windows W.focusMaster)
                ,((mod4Mask                     , xK_f), toggleFloat)
                ,((mod4Mask                     , xK_m), sendMessage $ Toggle REFLECTX)
                ,((mod4Mask .|. shiftMask       , xK_m), sendMessage $ Toggle MIRROR)
                ,((mod4Mask                     , xK_n), windows W.focusDown)
                ,((mod4Mask .|. shiftMask       , xK_n), windows W.swapDown)
                ,((mod4Mask                     , xK_t), windows W.focusUp)
                ,((mod4Mask .|. shiftMask       , xK_t), windows W.swapUp)
                ,((mod4Mask                     , xK_s), sendMessage Shrink)
                ,((mod4Mask .|. shiftMask       , xK_h), sendMessage MirrorShrink)
                ,((mod4Mask                     , xK_h), sendMessage Expand)
                ,((mod4Mask .|. shiftMask       , xK_s), sendMessage MirrorExpand)
                -- Xinerama settings for Dvorak Layout
                ,((mod4Mask                     , xK_w), viewScreen 0)
                ,((mod4Mask                     , xK_v), viewScreen 1)
                ,((mod4Mask                     , xK_z), viewScreen 2)
                ,((mod4Mask .|. shiftMask       , xK_w), sendToScreen 0)
                ,((mod4Mask .|. shiftMask       , xK_v), sendToScreen 1)
                ,((mod4Mask .|. shiftMask       , xK_z), sendToScreen 2)
--                [((modMask .|. mask, key), f sc)
--                       | (key, sc) <- zip [xK_w, xK_v, xK_z] [0..]
--                       , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
                -- Dzen scripts
                ,((mod4Mask                     , xK_F1), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_music.sh")
                ,((mod4Mask                     , xK_F2), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_vol.sh")
                ,((mod4Mask                     , xK_F3), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_network.sh")
                ,((mod4Mask                     , xK_F4), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_battery.sh")
                ,((mod4Mask                     , xK_F5), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_hardware.sh")
                ,((mod4Mask                     , xK_F6), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_pacman.sh")
                ,((mod4Mask                     , xK_F7), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_date.sh")
                ,((mod4Mask                     , xK_F8), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_log.sh")
                --Special keys
                ,((0                            , xK_Print), spawn "scrot & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
                ,((mod4Mask                     , xK_Print), spawn "scrot -s & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
                ,((0                            , xF86XK_AudioLowerVolume), spawn "amixer set Master 2- & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
                ,((0                            , xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+ & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
                ,((0                            , xF86XK_AudioMute), spawn "amixer set Master toggle")
                ,((0                            , xF86XK_Sleep), spawn "sudo suspend")
                ,((0                            , xF86XK_AudioPlay), spawn "ncmpcpp toggle")
                ,((0                            , xF86XK_AudioNext), spawn "ncmpcpp next")
                ,((0                            , xF86XK_AudioPrev), spawn "ncmpcpp prev")
                -- Application shortcuts
                ,((mod4Mask .|. shiftMask       , xK_b), spawn "~/.scripts/wpchanger")
--                ,((mod4Mask .|. shiftMask       , xK_b), spawn "chromium")
--                ,((mod4Mask .|. shiftMask       , xK_t), spawn "urxvt -e tmux")
                ]
                `additionalMouseBindings`
                [((mod4Mask                     , 6), (\_ -> moveTo Next NonEmptyWS))
                ,((mod4Mask                     , 7), (\_ -> moveTo Prev NonEmptyWS))
                ,((mod4Mask                     , 5), (\_ -> moveTo Prev NonEmptyWS))
                ,((mod4Mask                     , 4), (\_ -> moveTo Next NonEmptyWS))
                ]
                `removeKeys`
                [(mod4Mask .|. shiftMask, xK_c)]
        where toggleFloat = withFocused (\windowId -> do
                                           { floats <- gets (W.floating . windowset);
                                             if windowId `M.member` floats
                                             then withFocused $ windows . W.sink
                                             else float windowId })

myTerminal      = "urxvt"
myBitmapsDir    = "~/.xmonad/icons/"
myFont          = "xft:PragmataPro:style=Regular:pixelsize=14"


-- Old Colours. (Check their usage and clean them out if not needed)
background    = "#000000"
foreground    = "#ffffff"
black0        = "#343638"
black1        = "#404040"
red0          = "#2f468e"
red1          = "#7791e0"
green0        = "#424242"
green1        = "#828a8c"
yellow0       = "#6b8ba3"
yellow1       = "#8ebdde"
blue0         = "#1c4582"
blue1         = "#5365a6"
magenta0      = "#74636d"
magenta1      = "#927d9e"
cyan0         = "#556c85"
cyan1         = "#6e98b8"
white0        =  "#b2b2b2"
white1        = "#bdbdbd"

-- New Colours. May not be 100% to my liking yet..
fg_normal     = "#aaaaaa"
fg_focus      = "#7788af"
fg_urgent     = "#94738c"
fg_minimize   = "#444444"
fg_red        = "#ce5666"
fg_green      = "#80a673"
fg_yellow     = "#ffaf5f"
fg_blue       = "#7788af"
fg_magenta    = "#94738c"
fg_cyan       = "#778baf"
fg_white      = "#aaaaaa"
fg_blu        = "#8ebdde"

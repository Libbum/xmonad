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
import XMonad.Layout.MultiToggle.Instances hiding (FULL, NBFULL, NOBORDERS, SMARTBORDERS)
import XMonad.Layout.Reflect

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Actions.GridSelect
-- WINDOW RULES
import XMonad.ManageHook
-- KEYBOARD & MOUSE CONFIG
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86
-- STATUS BAR
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Dmenu
import XMonad.Util.Run (spawnPipe)
import Data.List                        -- clickable workspaces

-- MULTIMONITOR
import XMonad.Actions.PhysicalScreens

defaultLayouts =          onWorkspace (myWorkspaces !! 8) (avoidStruts fullScreen)
                        $ avoidStruts ( mkToggle (single REFLECTX) $ mkToggle (single MIRROR) ( tiledSpace  ||| tiled ||| goldenSpiral ||| Circle ||| mosaic )) ||| fullScreen
        where
                tiled            = spacing 5 $ ResizableTall nmaster delta ratio []
                tiledSpace       = spacing 60 $ ResizableTall nmaster delta ratio []
                fullScreen       = noBorders(fullscreenFull Full)
                mosaic           = spacing 5 $ MosaicAlt M.empty
                goldenSpiral     = spacing 5 $ spiral ratio
                -- Default number of windows in master pane
                nmaster = 1
                -- Percent of the screen to increment when resizing
                delta   = 5/100
                -- Default proportion of the screen taken up by main pane
                ratio   = toRational (2/(1 + sqrt 5 :: Double))

-- Declare workspaces and rules for applications

myWorkspaces = clickable $
                ["^i(/home/genesis/.xmonad/icons/alpha.xpm) alpha"
                ,"^i(/home/genesis/.xmonad/icons/beta.xpm) beta"
                ,"^i(/home/genesis/.xmonad/icons/gamma.xpm) gamma"
                ,"^i(/home/genesis/.xmonad/icons/delta.xpm) delta"
                ,"^i(/home/genesis/.xmonad/icons/epsilon.xpm) epsilon"
                ,"^i(/home/genesis/.xmonad/icons/stigma.xpm) stigma"
                ,"^i(/home/genesis/.xmonad/icons/zeta.xpm) zeta"
                ,"^i(/home/genesis/.xmonad/icons/eta.xpm) eta"
                ,"^i(/home/genesis/.xmonad/icons/theta.xpm) theta"]
        where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                                    (i,ws) <- zip [1..] l,
                                    let n = i ]

myManageHook = composeAll
                [ isFullscreen --> (doF W.focusDown <+> doFullFloat)
                ]

newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks

myXmonadBar = "dzen2 -x '1920' -y '0' -h '20' -w '700' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -qc /home/genesis/.xmonad/.conky_dzen | dzen2 -xs 3 -x '700' -y '0' -h '20' -w '1220' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -fn "++myFont
--myConky = "conky -c /home/genesis/conkyrc"

main = do
        dzenLeftBar     <- spawnPipe myXmonadBar
        dzenRightBar    <- spawnPipe myStatusBar
--      conky           <- spawn myConky
        xmonad $ defaultConfig
                { terminal              = myTerminal
                , borderWidth           = 1
                , normalBorderColor     = "#373B41"
                , focusedBorderColor    = "#DE935F"
                , modMask               = mod4Mask
                , layoutHook            = smartBorders(defaultLayouts)
                , workspaces            = myWorkspaces
                , manageHook            = newManageHook
                , handleEventHook       = fullscreenEventHook <+> docksEventHook
                }
                `additionalKeys`
                [((mod4Mask .|. shiftMask       , xK_x), kill)
                ,((mod4Mask .|. shiftMask       , xK_r), spawn "dmenu_run -h '20' -nb '#000000' -nf '#81A2BE' -sb '#282A2E' -sf '#DE935F' -fn 'PragmataPro-10'")
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
                `removeKeys`
                [(mod4Mask .|. shiftMask, xK_c)]
        where toggleFloat = withFocused (\windowId -> do
                                           { floats <- gets (W.floating . windowset);
                                             if windowId `M.member` floats
                                             then withFocused $ windows . W.sink
                                             else float windowId })

myTerminal      = "urxvt"
myBitmapsDir    = "~/.xmonad/icons/"
myFont          = "xft:PragmataPro:style=Regular:pixelsize=12"

background    = "#000000"
foreground    = "#ffffff"

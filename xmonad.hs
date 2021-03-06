import XMonad
import Control.Monad (liftM)
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
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Dmenu
import System.IO (hPutStrLn)
import XMonad.Util.Run (spawnPipe)
import Data.List
-- MULTIMONITOR
import XMonad.Actions.PhysicalScreens
import Data.Maybe (fromMaybe)
-- UTILITIES
import XMonad.Prompt
import XMonad.Prompt.AppendFile

--- LAYOUTS ---

defaultLayouts = onWorkspace (myWorkspaces !! 8) ((avoidStruts fullScreen) ||| fullScreen)
               $ avoidStruts (mkToggle (single REFLECTX) $ mkToggle (single MIRROR) ( tiledSpace  ||| tiled ||| goldenSpiral ||| Circle ||| mosaic )) ||| fullScreen
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

--- WORKSPACES ---

myWorkspaces = clickable $
                ["^i("++myIconsDir++"alpha.xpm) alpha"
                ,"^i("++myIconsDir++"beta.xpm) beta"
                ,"^i("++myIconsDir++"gamma.xpm) gamma"
                ,"^i("++myIconsDir++"delta.xpm) delta"
                ,"^i("++myIconsDir++"epsilon.xpm) epsilon"
                ,"^i("++myIconsDir++"stigma.xpm) stigma"
                ,"^i("++myIconsDir++"zeta.xpm) zeta"
                ,"^i("++myIconsDir++"eta.xpm) eta"
                ,"^i("++myIconsDir++"theta.xpm) theta"]
        where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                                    (i,ws) <- zip [1..] l,
                                    let n = i ]

--- HOOKS ---

myManageHook = composeAll
                [ isFullscreen                                              --> (doF W.focusDown   <+> doFullFloat)
                , isDialog                                                  --> (doF W.shiftMaster <+> doFloat)
                , resource =? "dmenu"                                       --> doFloat
                , resource =? "galculator"                                  --> doFloat
                , resource =? "skype"                                       --> doFloat
                , resource =? "feh"                                         --> doFloat
                , resource =? "MATLAB R2015b - academic use"                --> (doFloat <+> doShift (myWorkspaces !! 8))
                , (liftM (take 6) title) =? "Figure" <&&>
                               resource =? "MATLAB R2015b - academic use"   --> (doFloat <+> doShift (myWorkspaces !! 8))
                , resource =? "trayer"                                      --> doShift (myWorkspaces !! 3)
                , (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
                ]
        where role = stringProperty "WM_WINDOW_ROLE"

newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks

myLogHook h = dynamicLogWithPP ( defaultPP
        {
                  ppCurrent             = dzenColor "#D53E4F" background . pad -- \i -> wsCurrent i (wrap "|" "|" i) ++ "^p(6;)"
                , ppVisible             = dzenColor "#3288BD" background . pad
                , ppHidden              = dzenColor "#AAAAAA" background . pad
                , ppHiddenNoWindows     = dzenColor "#2B2B2B" background . pad
                , ppWsSep               = ""
                , ppSep                 = "    "
                , ppLayout              = wrap "^ca(1,xdotool key super+space)" "^ca()" . dzenColor "#D4D4D4" background .
                                (\x -> case x of
                                        "Full"                           ->      "^i("++myIconsDir++"monitor.xbm)"
                                        "Spacing 60 ResizableTall"       ->      "^i("++myIconsDir++"layout.xbm)"
                                        "Spacing 5 ResizableTall"        ->      "^i("++myIconsDir++"layout_tall.xbm)"
                                        "SimplestFloat"                  ->      "^i("++myIconsDir++"layers.xbm)"
                                        "Spacing 5 Spiral"               ->      "^i("++myIconsDir++"spiral.xbm)"
                                        "Circle"                         ->      "^i("++myIconsDir++"circle.xbm)"
                                        _                                ->      "^i("++myIconsDir++"grid3x3.xbm)"
                                )
                , ppOrder       =  \(ws:l:t:_) -> [ws,l]
                , ppOutput      =   hPutStrLn h
        } )
       where
          fg c        = dzenColor c ""
          --wsIcon = ("^i("++myIconsDir) . wrap "ws-" ".xbm)"
          wsCurrent i = fg . fromMaybe "#CC6666" . lookup i . zip [1..9] $ concatMap (replicate 3) ["#8C9440", "#8C9440", "#8C9440"]

--- GRID SELECT CUSTOMISATION ---

myGSNavigation :: TwoD a (Maybe a)
myGSNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
        where navKeyMap = M.fromList [((0,xK_Escape), cancel)
                                     ,((0,xK_Return), select)
                                     ,((0,xK_slash) , substringSearch myGSNavigation)
                                     ,((0,xK_Left)  , move (-1,0)  >> myGSNavigation)
                                     ,((0,xK_d)     , move (-1,0)  >> myGSNavigation)
                                     ,((0,xK_Right) , move (1,0)   >> myGSNavigation)
                                     ,((0,xK_n)     , move (1,0)   >> myGSNavigation)
                                     ,((0,xK_Down)  , move (0,1)   >> myGSNavigation)
                                     ,((0,xK_h)     , move (0,1)   >> myGSNavigation)
                                     ,((0,xK_Up)    , move (0,-1)  >> myGSNavigation)
                                     ,((0,xK_t)     , move (0,-1)  >> myGSNavigation)
                                     ,((0,xK_space) , setPos (0,0) >> myGSNavigation)
                                     ]
-- The navigation handler ignores unknown key symbols
navDefaultHandler = const myGSNavigation

hybridColorizer = colorRangeFromClassName
                      (0x00,0x00,0x00) -- lowest inactive bg
                      (0x2B,0x2B,0x2B) -- highest inactive bg
                      (0x32,0x88,0xBD) -- active bg
                      (0x32,0x88,0xBD) -- inactive fg
                      (0xFE,0xE0,0x8B) -- active fg

gsconfig2 colorizer = (buildDefaultGSConfig colorizer)  { gs_cellwidth  = 400
                                                        , gs_cellheight = 100
                                                        , gs_font       = myFont16
                                                        , gs_navigate   = myGSNavigation
                                                        , gs_colorizer  = hybridColorizer
                                                        }

--- PROMPT CUSTOMISATION ---

myPromptConfig = defaultXPConfig { font        = myFont16
                                 , height      = 30
                                 , bgColor     = "#000000"
                                 , fgColor     = "#ABDDA4"
                                 , borderColor = "#808080"
                                 , historySize = 0
                                 }

--- STATUS BARS ---

myXmonadBar = "dzen2 -dock -x '0' -y '0' -h '20' -w '565' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont12
myStatusBar = "conky -qc /home/tim/.xmonad/.conky_dzen | dzen2 -x '565' -y '0' -h '20' -w '1355' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -fn "++myFont12

-- CONTROL CONFIG --
screenlock :: MonadIO m => m ()
screenlock = spawn "sflock -f '-*-droid sans mono-medium-r-*-*-50-120-200-*-*-*-iso8859-1' -xshift -950"

showmenu :: MonadIO m => m ()
showmenu = spawn "dmenu_run -h '20' -nb '#000000' -nf '#3288BD' -sb '#2B2B2B' -sf '#F46D43' -fn 'PragmataPro'"

recomp :: MonadIO m => m ()
recomp = spawn "killall dzen2; killall conky; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart"

calculator :: MonadIO m => m ()
calculator = spawn "galculator"

melbtime :: MonadIO m => m ()
melbtime = spawn "~/bin/spooler" --Spool process for 5 seconds to toggle conky time zone check.

changewp :: MonadIO m => m ()
changewp = spawn "~/.scripts/wpchanger"

--- MAIN CONFIG ---

main = do
        dzenLeftBar     <- spawnPipe myXmonadBar
        dzenRightBar    <- spawnPipe myStatusBar
        xmonad $ defaultConfig
                { terminal              = myTerminal
                , borderWidth           = 1
                , normalBorderColor     = "#555555"
                , focusedBorderColor    = "#FDAE61"
                , modMask               = mod4Mask
                , layoutHook            = smartBorders(defaultLayouts)
                , workspaces            = myWorkspaces
                , manageHook            = newManageHook
                , handleEventHook       = fullscreenEventHook <+> docksEventHook
                , logHook               = myLogHook dzenLeftBar >> setWMName "LG3D"
                }
                `additionalKeys`
                [((mod4Mask .|. shiftMask       , xK_x), kill)
                ,((mod4Mask .|. shiftMask       , xK_r), showmenu)
                ,((mod4Mask                     , xK_q), recomp)
                ,((mod4Mask                     , xK_l), screenlock)
                ,((mod4Mask                     , xK_c), calculator)
                ,((mod4Mask                     , xK_o), melbtime)
                -- Window and Program settings for Dvorak Layout
                ,((mod4Mask                     , xK_g), goToSelected $ gsconfig2 hybridColorizer)
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
 -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
 -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--                [((modMask .|. mask, key), f sc)
--                       | (key, sc) <- zip [xK_w, xK_v, xK_z] [0..]
--                       , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
                --Special keys
                ,((mod4Mask                     , xK_Print), spawn "sleep 0.2; scrot -s")
                ,((0                            , xK_Print), spawn "scrot")
                ,((mod4Mask .|. controlMask     , xK_Print), spawn "sleep 0.2; scrot -s ~/tmp.png && ~/.scripts/clip.py ~/tmp.png && rm ~/tmp.png") --clipboard copy
                ,((0                            , xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
                ,((0                            , xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
                ,((0                            , xF86XK_AudioMute), spawn "amixer set Master toggle")
                ,((0                            , xF86XK_Sleep), spawn "systemctl suspend")
                ,((0                            , xF86XK_AudioPlay), spawn "ncmpcpp toggle")
                ,((0                            , xF86XK_AudioNext), spawn "ncmpcpp next")
                ,((0                            , xF86XK_AudioPrev), spawn "ncmpcpp prev")
                -- Application shortcuts
                ,((mod4Mask .|. shiftMask       , xK_b), changewp)
                ,((mod4Mask                     , xK_a), do
                                                          spawn ("echo -e '\n'$(date +\"%T %F\")'\n'===================>>"++"/home/tim/notes.md")
                                                          appendFilePrompt myPromptConfig "/home/tim/notes.md")
                ]
                `additionalMouseBindings`
                [((mod4Mask                     , 2), (const $ spawn "ncmpcpp stop"))
                ,((mod4Mask                     , 4), (const $ spawn "ncmpcpp prev"))
                ,((mod4Mask                     , 5), (const $ spawn "ncmpcpp next"))
                ]
                `removeKeys`
                [(mod4Mask .|. shiftMask, xK_c)]
        where toggleFloat = withFocused (\windowId -> do
                                           { floats <- gets (W.floating . windowset);
                                             if windowId `M.member` floats
                                             then withFocused $ windows . W.sink
                                             else float windowId })

--- ADDITIONAL VARIABLES ---

myTerminal      = "urxvt"
myIconsDir      = "/home/tim/.xmonad/icons/"
myFont12        = "xft:PragmataPro:style=Regular:pixelsize=12"
myFont16        = "xft:PragmataPro:style=Regular:pixelsize=16"

background    = "#000000"
foreground    = "#ffffff"

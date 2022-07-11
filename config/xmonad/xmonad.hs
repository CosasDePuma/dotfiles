-- TODO closed imports(), myLogHook, help, up/down keys, move window using keys.

-- 📦 imports

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad                    as X
import XMonad.Layout.Spacing(spacingRaw,Border(..))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks(avoidStruts,docks)
import XMonad.Util.Run(hPutStrLn,spawnPipe)
import XMonad.Util.SpawnOnce(spawnOnce)
import Graphics.X11.ExtraTypes.XF86
import Data.Function
import System.Exit(exitWith,ExitCode(ExitSuccess))

-- 🚪 entrypoint

main = do
    xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"  -- start xmobar
    getDirectories >>= (launch $ docks $ myConfig xmproc)   -- start xmonad

-- 🧰 configuration

myConfig myProc = def {
    -- simple stuff
    terminal           = "xterm",
    borderWidth        = 3,
    clickJustFocuses   = False,
    focusFollowsMouse  = True,
    modMask            = mod4Mask,
    workspaces         = map show [1..9],
    normalBorderColor  = "#0d0e0c",
    focusedBorderColor = "#c15a45",
    -- complex stuff
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook myProc,
    startupHook        = myStartupHook
}

-- 🔑 keybindings

myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Escape              ), io (exitWith ExitSuccess))                                      -- close xmonad
--  , ((modm              , xK_Escape              ), restart "xmonad" True)                                          -- reload xmonad
    , ((modm              , xK_Return              ), spawn $ X.terminal conf)                                        -- terminal
    , ((modm              , xK_r                   ), spawn "rofi -modi drun -show drun")                             -- launcher
    , ((modm              , xK_q                   ), kill)                                                           -- close window
    , ((modm              , xK_space               ), sendMessage NextLayout)                                         -- change layout
    , ((modm .|. shiftMask, xK_space               ), setLayout $ X.layoutHook conf)                                  -- reset layout
    , ((modm              , xK_Tab                 ), windows W.focusDown)                                            -- change focus (down)
    , ((modm .|. shiftMask, xK_Tab                 ), windows W.focusUp)                                              -- change focus (up)
    , ((modm              , xK_Right               ), windows W.focusDown)                                            -- focus next
    , ((modm              , xK_Left                ), windows W.focusUp)                                              -- focus previous
    , ((modm              , xK_m                   ), windows W.focusMaster)                                          -- focus master
    , ((modm .|. shiftMask, xK_m                   ), windows W.swapMaster)                                           -- change master
    , ((modm              , xK_j                   ), sendMessage Shrink)                                             -- shrink window
    , ((modm              , xK_k                   ), sendMessage Expand)                                             -- expand window
    , ((modm              , xK_t                   ), withFocused $ windows . W.sink)                                 -- tiling window
    , ((0                 , xF86XK_AudioLowerVolume), spawn "amixer set Master 10%-")                                 -- volume down
    , ((0                 , xF86XK_AudioRaiseVolume), spawn "amixer set Master 10%+")                                 -- volume up
    , ((0                 , xF86XK_AudioMute       ), spawn "amixer set Master toggle")                               -- volume mute
    , ((modm              , xK_period              ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))           -- show help
    ] ++ [
      ((m .|. modm, k), windows $ f i) | (i, k) <- zip (X.workspaces conf) [xK_1 .. xK_9]                             -- change workspace
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]                                                           -- move to workspace
    ]

-- 🐭 mousebindings

myMouseBindings (XConfig {X.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))         -- floating/move window
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))       -- floating/resize window
    ]

-- 🍱 layouts

myLayout = avoidStruts $ spacingRaw False
    (Border 10 10 10 10) True               -- screen gaps
    (Border 10 10 10 10) True               -- window gaps
    $ ( tiled ||| Full )                    -- layouts
  where
    tiled   = Tall nmaster delta ratio      -- custom layout: master and secondary windows
    nmaster = 1                             -- how many master windows
    ratio   = 1/2                           -- master window space
    delta   = 3/100                         -- resize percentage

-- 🏠 windows
  -- check window names with 'xprop | grep WM_CLASS'

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , resource  =? "kdesktop"       --> doIgnore ]

-- 🔔 events

myEventHook = mempty

-- 📜 logger

myLogHook xmproc = dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc }


-- 🏁 startup

myStartupHook = do
    spawnOnce "pulseaudio --start"                                       -- audio
    spawnOnce "feh --no-fehbg --bg-fill ~/.config/wallpapers/wallpaper"  -- wallpaper

-- 🆘 help message

help :: String
help = unlines [
    "",
    " Default keybindings:",
    "",
    " -- Programs --",
    " [Win + Enter]         Launch terminal",
    " [Win + R]             Run the launcher",
    "",
    " -- Windows --",
    " [Win + Q]             Close the focused window",
    " [Win + Tab]           Focus next window",
    " [Win + Shift + Tab]   Focus previous window",
    " [Win + Right]         Focus next window",
    " [Win + Left]          Focus previous window",
    "",
    " -- Workspaces --",
    " [Win + 1..9]          Switch to workspace (1..9)",
    " [Win + Shift + 1..9]  Move focused window to workspace (1..9)",
    "",
    " -- XMonad --",
--  " [Win + Esc]             Reload the window manager",
    " [Win + Shift + Esc]     Exit the window manager"]

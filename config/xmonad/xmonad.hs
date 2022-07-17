-- 📦 imports

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad                    as X
import XMonad.Layout.Spacing(spacingRaw,Border(..))
import XMonad.Layout.MultiToggle(mkToggle,single)
import XMonad.Layout.MultiToggle.Instances(StdTransformers(NBFULL))
import qualified XMonad.Layout.MultiToggle as MT(Toggle(..))
import XMonad.Hooks.DynamicLog(dynamicLogWithPP,xmobarPP,PP(..) , xmobarColor,shorten,wrap)
import XMonad.Hooks.EwmhDesktops(ewmh,ewmhFullscreen)
import XMonad.Hooks.ManageDocks(avoidStruts,docks,ToggleStruts(..))
import XMonad.Hooks.ManageHelpers(doRectFloat)
import XMonad.StackSet(RationalRect(..))
import XMonad.Util.Run(hPutStrLn,spawnPipe)
import XMonad.Util.SpawnOnce(spawnOnce)
import Graphics.X11.ExtraTypes.XF86
import Data.Function((&))
import Data.Ratio((%))
import System.Exit(exitWith,ExitCode(ExitSuccess))

-- 🚪 entrypoint

main = do
    xmproc <- spawnPipe "xmobar /etc/xmobar/xmobarrc"                              -- start xmobar
    getDirectories >>= (launch $ docks . ewmhFullscreen . ewmh $ myConfig xmproc)  -- start xmonad

-- 🔨 configuration

myConfig myProc = def {
    -- simple stuff
    terminal           = "alacritty",
    borderWidth        = 3,
    clickJustFocuses   = False,
    focusFollowsMouse  = True,
    modMask            = mod4Mask,
    workspaces         = map show [1..9],
    normalBorderColor  = "#474646",
    focusedBorderColor = "#83a598",
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

launcher = "rofi -theme /etc/rofi/rofi.rasi -show "
myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Escape              ), io (exitWith ExitSuccess))                                      -- close xmonad
    , ((modm              , xK_Return              ), spawn $ X.terminal conf)                                        -- terminal
    , ((modm .|. shiftMask, xK_Return              ), spawn "cool-retro-term --fullscreen")                           -- retro terminal
    , ((modm              , xK_r                   ), spawn $ launcher ++ "drun")                                     -- launcher (apps)
    , ((modm              , xK_e                   ), spawn $ launcher ++ "emoji")                                    -- launcher (emojis)
    , ((modm              , xK_c                   ), spawn $ launcher ++ "calc")                                     -- calculator
    , ((modm              , xK_f                   ), spawn "pcmanfm")                                                -- file manager
    , ((modm              , xK_q                   ), kill)                                                           -- close window
    , ((modm              , xK_space               ), sendMessage NextLayout)                                         -- change layout
    , ((modm .|. shiftMask, xK_space               ), setLayout $ X.layoutHook conf)                                  -- reset layout
    , ((modm              , xK_Tab                 ), windows W.focusDown)                                            -- change focus (down)
    , ((modm .|. shiftMask, xK_Tab                 ), windows W.focusUp)                                              -- change focus (up)
    , ((modm              , xK_Right               ), windows W.focusDown)                                            -- focus next
    , ((modm              , xK_Down                ), windows W.focusDown)                                            -- focus next
    , ((modm              , xK_Left                ), windows W.focusUp)                                              -- focus previous
    , ((modm              , xK_Up                  ), windows W.focusUp)                                              -- focus previous
    , ((modm .|. shiftMask, xK_Right               ), windows W.swapDown)                                             -- focus next
    , ((modm .|. shiftMask, xK_Down                ), windows W.swapDown)                                             -- focus next
    , ((modm .|. shiftMask, xK_Left                ), windows W.swapUp)                                               -- focus previous
    , ((modm .|. shiftMask, xK_Up                  ), windows W.swapUp)                                               -- focus previous
    , ((modm              , xK_m                   ), windows W.swapMaster)                                           -- change master
    , ((modm .|. shiftMask, xK_m                   ), windows W.focusMaster)                                          -- focus master
    , ((modm              , xK_j                   ), sendMessage Shrink)                                             -- shrink window
    , ((modm              , xK_k                   ), sendMessage Expand)                                             -- expand window
    , ((modm              , xK_F11                 ), sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)     -- toggle fullscreen
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

myLayout = avoidStruts $ mkToggle (single NBFULL) $ spacingRaw False
    (Border gap gap gap gap) True           -- screen gaps
    (Border gap gap gap gap) True           -- window gaps
    $ ( tiled ||| Full )                    -- layouts
  where
    tiled   = Tall nmaster delta ratio      -- custom layout: master and secondary windows
    nmaster = 1                             -- how many master windows
    ratio   = 1/2                           -- master window space
    delta   = 3/100                         -- resize percentage
    gap     = 5                             -- gaps size

-- 🏠 windows
  -- check window names with 'xprop | grep WM_CLASS'

myManageHook = composeAll
    [ stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doRectFloat (RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    ]

-- 🔔 events

myEventHook = mempty

-- 📜 logger

myLogHook xmproc = dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppCurrent = xmobarColor "#83a598" "" . wrap "[" "]"   -- #9BC1B2 #69DFFA
    , ppTitle = xmobarColor "#d3869b" "" . shorten 50       -- #9BC1B2 #69DFFA
    , ppOrder = \(ws:l:t:_) -> [ws]
    }

-- 🏁 startup

myStartupHook = do
    spawnOnce "dunst"                                        -- notification
    spawnOnce "picom"                                        -- compositor
    spawnOnce "feh --no-fehbg --bg-fill /etc/feh/wallpaper"  -- wallpaper

-- 🆘 help message

help :: String
help = unlines [
    "",
    " Default keybindings:",
    "",
    " -- Programs --",
    " [Win + Enter]                Launch terminal",
    " [Win + Shift + Enter]        Launch retro terminal",
    " [Win + R]                    Run the launcher (applications)",
    " [Win + E]                    Run the launcher (emojis)",
    " [Win + C]                    Run the launcher (calculator)",
    "",
    " -- Windows --",
    " [Win + Q]                    Close the focused window",
    " [Win + M]                    Define the master window",
    " [Win + Shift + M]            Focus master window",
    " [Win + Tab]                  Focus next window",
    " [Win + Shift + Tab]          Focus previous window",
    " [Win + Down/Right]           Focus next window",
    " [Win + Up/Left]              Focus previous window",
    " [Win + Shift + Down/Right]   Swap window with next",
    " [Win + Shift + Up/Left]      Swap window with previous",
    "",
    " -- Layouts --",
    " [Win + F]                    Enter fullscreen mode",
    " [Win + Space]                Change the layout",
    " [Win + Shift + Space]        Change to primary layout",
    "",
    " -- Workspaces --",
    " [Win + 1..9]                 Switch to workspace (1..9)",
    " [Win + Shift + 1..9]         Move focused window to workspace (1..9)",
    "",
    " -- XMonad --",
    " [Win + Esc]                  Launch lockscreen",
    " [Win + Shift + Esc]          Exit the window manager"]

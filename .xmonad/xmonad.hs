import Data.Map (Map,member)
import Data.Ratio ((%))
import System.Exit (exitSuccess)
import System.IO (Handle,hPutStrLn)
import XMonad
import XMonad.Config (def)
import XMonad.Hooks.DynamicLog (shorten,xmobarColor,wrap)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhFullscreen)
import XMonad.Hooks.ManageDocks (AvoidStruts,avoidStruts,docks)
import XMonad.Hooks.StatusBar (statusBarProp,withSB)
import XMonad.Hooks.StatusBar.PP (PP,PP(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder,smartBorders)
import XMonad.Layout.Spacing (Border(Border),Spacing,spacingRaw)
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.SpawnOnce (spawnOnce) 
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import Theme -- Custom

-- -----------------
--  Entry Point
-- -----------------

-- main :: IO ()
main = do
  xmonad . myWrappers $ myConfig

-- -----------------
--  Wrappers
-- -----------------

--myWrappers :: XConfig a0 -> XConfig a0
myWrappers =
  withSB myStatusBar .    -- status bar
  ewmhFullscreen . ewmh . -- fullscreen support
  docks                   -- taskbar and docks support

-- -----------------
--  Configuration
-- -----------------

myConfig :: XConfig Layouts
myConfig = def {
  -- | General behavior
  modMask            = mod4Mask,
  focusFollowsMouse  = False,
  clickJustFocuses   = False,
  -- | Appearance
  borderWidth        = 3,
  normalBorderColor  = Theme.normal,
  focusedBorderColor = Theme.focused,
  -- | Hooks
  keys               = myKeys,
  layoutHook         = myLayouts,
  startupHook        = myStartup,
  workspaces         = myWorkspaces
}
       
-- -----------------
--  Startup
-- -----------------

myStartup :: X ()
myStartup = do
  spawnOnce "picom -b"         -- compositor
  spawnOnce "flameshot"        -- screenshot
  spawnOnce "~/.local/bin/feh" -- wallpaper

-- -----------------
--  Workspaces
-- -----------------

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts x = [x]
myWorkspaces :: [String]
myWorkspaces = clickableSymbols [1..9]
  where
    clickableSymbols l = ["<action=xdotool set_desktop " ++ show (i) ++ "> <fn=1>\61713</fn> </action>" | i <- l] 

-- -----------------
--  Keybindings
-- -----------------

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys = \conf -> mkKeymap conf $ [
    -- | XMonad
    ("M-S-<Esc>",     io exitSuccess),                                                                       -- quit xmonad
    ("M-S-r",         spawn "sh -c 'xmonad --recompile && xmonad --restart'"),                               -- reload xmonad
    -- | Windows: Control
    ("M-q",           kill),                                                                                 -- close the focused window
    ("M-t",           withFocused toggleFloat),                                                              -- toggle the focused window between tiled and float
    ("M-`",           withFocused toggleFloat),                                                              -- toggle the focused window between tiled and float
    -- | Windows: Focus
    ("M-m",           windows W.focusMaster),                                                                -- focus master window
    ("M1-<Tab>",      windows W.focusDown),                                                                  -- focus next window
    ("M1-S-<Tab>",    windows W.focusUp),                                                                    -- focus previous window
    ("M-<Right>",     windows W.focusDown),                                                                  -- focus next window
    ("M-<Down>",      windows W.focusDown),                                                                  -- focus next window
    ("M-<Left>",      windows W.focusUp),                                                                    -- focus previous window
    ("M-<Up>",        windows W.focusUp),                                                                    -- focus previous window
    -- | Windows: Order
    ("M-S-m",         windows W.swapMaster),                                                                 -- swap the focus and the master
    ("M-S-<Left>",    windows W.swapUp),                                                                     -- swap the focus with the previous window
    ("M-S-<Up>",      windows W.swapUp),                                                                     -- swap the focus with the previous window
    ("M-S-<Right>",   windows W.swapDown),                                                                   -- swap the focus with the next window
    ("M-S-<Down>",    windows W.swapDown),                                                                   -- swap the focus with the next window
    -- | Layouts
    ("M-<Backspace>", sendMessage NextLayout),                                                               -- rotate through layout algorithms
    ("M-.",           sendMessage Expand),                                                                   -- expand the master area
    ("M-,",           sendMessage Shrink),                                                                   -- shrink the master area
    -- | Programs
    ("M-b",           spawn "firefox"),                                                                      -- browser
    ("M-S-b",         spawn "firefox --private-window"),                                                     -- browser (private)
    ("M-e",           spawn "thunar"),                                                                       -- file manager
    ("M-<Return>",    spawn "kitty"),                                                                        -- terminal
    ("M-S-<Space>",   spawn "rofi -show drun"),                                                              -- launcher
    ("M-C-<Tab>",     spawn "rofi -show window"),                                                            -- launcher (windows)
    -- | Screen
    ("<Print>",       spawn "flameshot gui")                                                                 -- screenshot
  ]
  -- | Workspaces
  ++ [ ("M-"   ++ [n], windows $ W.greedyView w)             | (n,w) <- zip ['1'..wl] (workspaces conf) ]   -- switch workspaces with numbers
  ++ [ ("M-C-" ++ [n], windows $ W.shift w)                  | (n,w) <- zip ['1'..wl] (workspaces conf) ]   -- send focused window to workspace
  ++ [ ("M-S-" ++ [n], windows $ W.greedyView w . W.shift w) | (n,w) <- zip ['1'..wl] (workspaces conf) ]   -- send focused window to workspace and switch workspace
    where
      wl = head . show $ length myWorkspaces
      toggleFloat window = windows $ \s ->
        if member window (W.floating s)
          then W.sink window s
          else W.float window (W.RationalRect (1%4) (1%4) (1%2) (1%2)) s

-- -----------------
--  Layouts
-- -----------------

type Tiled   = ModifiedLayout Spacing Tall
type Layouts =
  ModifiedLayout SmartBorder
    (ModifiedLayout AvoidStruts
      (Choose Tiled (Mirror Tiled)))

myGaps :: Integer -> Tall a0 -> Tiled a0
myGaps i = spacingRaw True (Border 0 i 0 i) True (Border i 0 i 0) True 

myLayouts :: Layouts Window
myLayouts = smartBorders . avoidStruts $ layouts
  where
    layouts = tiled ||| Mirror tiled
    tiled   = myGaps 5 $ Tall nmaster delta ratio
    nmaster = 1     -- default number of windows in the master pane
    ratio   = 1/2   -- default proportion of screen occupied by master
    delta   = 3/100 -- percent of screen to increment by when resizing

-- -----------------
--  Status Bar
-- -----------------

myStatusBar = statusBarProp statusBar (pure myXmobarPP)
  where
    statusBar = "xmobar ~/.config/xmobar/xmobarrc"

myXmobarPP :: PP
myXmobarPP = def {
  ppSep             = "<fn=1> </fn>",
  ppCurrent         = xmobarColor Theme.focused "" . \s -> " <fn=1>\61713</fn> ",
  ppVisible         = xmobarColor Theme.normal "",
  ppHidden          = xmobarColor Theme.subtext "",
  ppHiddenNoWindows = xmobarColor Theme.normal "",
  ppUrgent          = xmobarColor Theme.urgent "" . wrap "*" "",
  ppOrder           = \(ws:_) -> [ws],
  ppExtras          = []
}

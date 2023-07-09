import Data.Map (Map,member)
import Data.Ratio ((%))
import System.Exit (exitSuccess)
import XMonad
import XMonad.Config (def)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhFullscreen)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder,smartBorders)
import XMonad.Layout.Spacing (Border(Border),Spacing,spacingRaw)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (mkKeymap)
import qualified XMonad.StackSet as W
-- Custom
import Theme

-- -----------------
--  Entry Point
-- -----------------

main :: IO ()
main = xmonad . myWrappers $ myConfig

-- -----------------
--  Wrappers
-- -----------------

myWrappers :: XConfig a0 -> XConfig a0
myWrappers =
  ewmhFullscreen . ewmh -- fullscreen support

-- -----------------
--  Variables
-- -----------------

myConfig :: XConfig Layouts
myConfig = def {
    modMask            = mod4Mask,
    borderWidth        = 3,
    normalBorderColor  = Theme.normal,
    focusedBorderColor = Theme.focused,
    focusFollowsMouse  = False,
    clickJustFocuses   = False,
    
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
    spawnOnce "picom -b"          -- compositor
    spawnOnce "flameshot"         -- screenshot
    spawnOnce "~/.local/bin/feh"  -- wallpaper

-- -----------------
--  Workspaces
-- -----------------

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9]

-- -----------------
--  Keybindings
-- -----------------

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys = \conf -> mkKeymap conf $ [
    -- XMonad
    ("M-S-<Esc>",     io exitSuccess),                                                                       -- quit xmonad
    ("M-S-r",         spawn "sh -c 'xmonad --recompile && xmonad --restart'"),                               -- reload xmonad
    -- Windows: Control
    ("M-q",           kill),                                                                                 -- close the focused window
    ("M-t",           withFocused toggleFloat),                                                              -- toggle the focused window between tiled and float
    ("M-`",           withFocused toggleFloat),                                                              -- toggle the focused window between tiled and float
    -- Windows: Focus
    ("M-m",           windows W.focusMaster),                                                                -- focus master window
    ("M1-<Tab>",      windows W.focusDown),                                                                  -- focus next window
    ("M1-S-<Tab>",    windows W.focusUp),                                                                    -- focus previous window
    ("M-<Right>",     windows W.focusDown),                                                                  -- focus next window
    ("M-<Down>",      windows W.focusDown),                                                                  -- focus next window
    ("M-<Left>",      windows W.focusUp),                                                                    -- focus previous window
    ("M-<Up>",        windows W.focusUp),                                                                    -- focus previous window
    -- Windows: Order
    ("M-S-m",         windows W.swapMaster),                                                                 -- swap the focus and the master
    ("M-S-<Left>",    windows W.swapUp),                                                                     -- swap the focus with the previous window
    ("M-S-<Up>",      windows W.swapUp),                                                                     -- swap the focus with the previous window
    ("M-S-<Right>",   windows W.swapDown),                                                                   -- swap the focus with the next window
    ("M-S-<Down>",    windows W.swapDown),                                                                   -- swap the focus with the next window
    -- Layouts
    ("M-<Backspace>", sendMessage NextLayout),                                                               -- rotate through layout algorithms
    ("M-.",           sendMessage Expand),                                                                   -- expand the master area
    ("M-,",           sendMessage Shrink),                                                                   -- shrink the master area
    -- Programs
    ("M-b",           spawn "firefox"),                                                                      -- browser
    ("M-S-b",         spawn "firefox --private-window"),                                                     -- browser (private)
    ("M-e",           spawn "thunar"),                                                                       -- file manager
    ("M-<Return>",    spawn "kitty"),                                                                        -- terminal
    ("M-S-<Space>",   spawn "rofi -show drun"),                                                              -- launcher
    ("M-C-<Tab>",     spawn "rofi -show window"),                                                            -- launcher (windows)
    -- Screen
    ("<Print>",       spawn "flameshot gui")                                                                 -- screenshot
  ]
  -- Workspaces
  ++ [ ("M-"   ++ [n], windows $ W.greedyView w)             | (n,w) <- zip ['1'..'9'] (workspaces conf) ]   -- switch workspaces with numbers
  ++ [ ("M-S-" ++ [n], windows $ W.greedyView w . W.shift w) | (n,w) <- zip ['1'..'9'] (workspaces conf) ]   -- switch focused window to workspace
    where
      toggleFloat window = windows $ \s ->
        if member window (W.floating s)
          then W.sink window s
          else (W.float window (W.RationalRect (1%4) (1%4) (1%2) (1%2)) s)

-- -----------------
--  Layouts
-- -----------------

type Layouts = ModifiedLayout SmartBorder (Choose Tiled (Mirror Tiled))
type Tiled   = ModifiedLayout Spacing Tall

myGaps i = spacingRaw True (Border 0 i 0 i) True (Border i 0 i 0) True 

myLayouts :: Layouts Window
myLayouts = smartBorders $ layouts
  where
    layouts = tiled ||| Mirror tiled
    tiled   = myGaps 5 $ Tall nmaster delta ratio

    nmaster = 1     -- default number of windows in the master pane
    ratio   = 1/2   -- default proportion of screen occupied by master
    delta   = 3/100 -- percent of screen to increment by when resizing

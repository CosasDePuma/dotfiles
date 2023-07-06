import XMonad
import XMonad.Config (def)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhFullscreen)

-- -----------------
--  Entry Point
-- -----------------

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh $ myConfig

-- -----------------
--  Variables
-- -----------------

myConfig :: XConfig a
myConfig = def {
    terminal = "kitty",
    modMask = mod4Mask,
    layoutHook = myLayouts
  } `additionalKeysP` [

    -- -----------------
    --  Keybindings
    -- -----------------

    -- XMonad
    ("M-S-q",       spawn "xmonad --recompile && xmonad --restart"),
    -- Programs
    ("M-b",         spawn "firefox"),
    ("M-S-b",       spawn "firefox --private-window"),
    ("M-e",         spawn "thunar"),
    ("M-<Return>",  spawn "kitty"),
    ("M-S-<Space>", spawn "rofi -show drun"),
    ("M-S-<Tab>",   spawn "rofi -show window"),
    -- Screen
    ("<Print>",     spawn "flameshot gui")
  ]

-- -----------------
--  Layouts
-- -----------------
myLayouts = tiled ||| Mirror tiled
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1    -- number of windows in the master pane
    ratio = 1/2    -- proportion of screen occupied by master pane
    delta = 3/100  -- percent of screen to increment by when resizing panes
import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.SpawnOn

-- Layout stuff
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders




main :: IO ()
main = xmonad $ ewmhFullscreen . ewmh $ myConfig

myConfig = def 
    { modMask = mod4Mask
    , terminal = "alacritty"
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    }
    `additionalKeysP` 
    [ ("M-b", spawn "vivaldi")
    , ("M-x", kill)
    , ("M-w", spawn "walset")
    , ("M-<Space>", spawn "dmenu_run")
    , ("M-f", sendMessage NextLayout)
    ]

myManageHook :: ManageHook
myManageHook = manageSpawn <> composeAll
    [ isDialog			    --> doFloat
    , className =? "discord"	    --> doShift "chat"
    , className =? "Spotify"	    --> doShift "music"
    ]

myLayoutHook = smartSpacingWithEdge 10 $ noBorders $ myLayouts
myLayouts = tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myWorkspaces = ["term", "web", "3", "4", "5", "6", "7", "music", "chat"]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom"
  spawnOnce "setxkbmap gb extd"
  spawnOnce "walset"
  spawnOnOnce "term" "alacritty"
  spawnOnOnce "web" "vivaldi"
  spawnOnce "discord"
  spawnOnce "spotify"

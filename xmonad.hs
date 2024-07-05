import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad $ ewmhFullscreen . ewmh $ myConfig

myConfig = desktopConfig
    { modMask = mod4Mask
    , terminal = "alacritty"
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    , focusFollowsMouse = False
    }
    `additionalKeysP`
    [ ("M-b", spawn "vivaldi")
    , ("M-w", kill)
    , ("M-<Space>", spawn "dmenu_run")
    , ("M-f", sendMessage NextLayout)
    , ("M-s", spawn "dunstify Time \"$(date)\"")
    , ("M-a", toggleSystray)
    ]

myManageHook :: ManageHook
myManageHook = manageSpawn <> composeAll
    [ isDialog --> doFloat
    , className =? "discord" --> doShift "chat"
    , className =? "Spotify" --> doShift "entertainment"
    , className =? "element-desktop" --> doShift "chat"
    , className =? "Vivaldi-stable" --> doShift "browser"
    , className =? "lxqt-config" --> doShift "settings"
    ]

myLayoutHook = smartSpacingWithEdge 7 $ noBorders $ myLayouts
  where
    myLayouts = tiled ||| Full
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myWorkspaces = ["term", "browser", "3", "4", "5", "6", "entertainment", "chat", "settings"]

toggleSystray = do
    spawn "killall stalonetray || stalonetray"

myStartupHook :: X ()
myStartupHook = do
  spawnOnOnce "term" "tmux attach -t 0 || tmux new -s 0"
  spawnOnce "walset"
  spawnOnce "vivaldi"
  spawnOnce "discord"
  spawnOnce "spotify"
  spawnOnce "lxqt-config"
  spawn "killall dunst; notify-send 'Reloaded configuration'"


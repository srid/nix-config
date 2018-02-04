import           Data.Default               (def)
import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders    (smartBorders)
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig

main = do
  xmonad $ def
    { terminal    = "konsole"
    , modMask     = mod4Mask
    , startupHook = startupCommands
    , borderWidth = 2
    , layoutHook = ThreeColMid 1 (3/100) (1/2) ||| Full
    , manageHook  = manageHook desktopConfig <+> manageDocks
    , normalBorderColor = "#10b060"
    , focusedBorderColor = "#30d080"
    }
    `additionalKeysP`
    [ ("M1-C-l", lockScreen) -- Lock screen using Ctrl+Alt+L
    , ("<Print>", takeScreenshot) -- Take screenshot
    ]

takeScreenshot =
  spawn "maim -s | xclip -selection clipboard -t image/png"

lockScreen =
  spawn "i3lock -i ~/mynixos/files/Atom-HD-Wallpaper.png"

startupCommands :: X ()
startupCommands = do
  -- Wallpaper
  spawn "feh --bg-fill ~/mynixos/files/Elephant-Mammoth-Dark.jpg"

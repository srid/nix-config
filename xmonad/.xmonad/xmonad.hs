import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ThreeColumns

main = do
  xmonad $ defaultConfig
    { terminal    = "konsole"
    , modMask     = mod4Mask
    , borderWidth = 2
    , layoutHook = ThreeColMid 1 (3/100) (1/2) ||| Full
    , manageHook  = manageHook desktopConfig <+> manageDocks
    , normalBorderColor = "#10b060"
    , focusedBorderColor = "#30d080"
    }
    `additionalKeysP`
    [ ("M1-C-l", spawn "slock") -- Lock screen using Ctrl+Alt+L
    , ("<Print>", spawn "maim -s ~/Pictures/maim-screenshot.png") -- Take screenshot
    ]

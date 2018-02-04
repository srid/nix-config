import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Actions.SimpleDate

main = xmonad $ defaultConfig
    { terminal    = "konsole"
    , modMask     = mod4Mask
    , borderWidth = 2
    -- Status bar space setup
    , layoutHook  = avoidStruts $ layoutHook desktopConfig
    -- , layoutHook = smartSpacing 2 $ Tall 1 (3/100) (1/2)
    , manageHook  = manageHook desktopConfig <+> manageDocks
    , normalBorderColor = "#10b060"
    , focusedBorderColor = "#30d080"
    }
    `additionalKeysP`
    [ ("M1-C-l", spawn "slock") -- Lock screen using Ctrl+Alt+L
    ]

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar desktopConfig
    { terminal    = "alacritty"
    , modMask     = mod4Mask
    , borderWidth = 2
    -- Status bar space setup
    , layoutHook  = avoidStruts $ layoutHook desktopConfig
    , manageHook  = manageHook desktopConfig <+> manageDocks
    , normalBorderColor = "#10b060"
    , focusedBorderColor = "#30d080"
    }

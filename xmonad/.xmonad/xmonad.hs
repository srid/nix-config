import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar desktopConfig
    { terminal    = "alacritty"
    , modMask     = mod4Mask
    , layoutHook  = avoidStruts $ layoutHook desktopConfig
    , manageHook  = manageHook desktopConfig <+> manageDocks
    }

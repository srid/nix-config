import XMonad
import XMonad.Config.Desktop

main = xmonad desktopConfig
    { terminal    = "alacritty"
    , modMask     = mod4Mask
    }

import XMonad
import XMonad.Layout.MultiColumns

main =
    xmonad
    defaultConfig
        { modMask = mod4Mask, -- Use Super instead of Alt
          terminal = "myst"
        -- more changes
        }
import XMonad

-- import XMonad.Layout.MultiColumns

main :: IO ()
main =
  xmonad
    def
      { modMask = mod4Mask, -- Use Super instead of Alt
        terminal = "myst"
      }
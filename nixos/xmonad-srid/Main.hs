import XMonad
import XMonad.Layout.ThreeColumns (ThreeCol (..))

-- import XMonad.Layout.MultiColumns

main :: IO ()
main =
  xmonad
    def
      { modMask = mod4Mask, -- Use Super instead of Alt
        terminal = "myst",
        layoutHook = layoutHook def ||| ThreeColMid 1 (3 / 100) (1 / 2)
      }
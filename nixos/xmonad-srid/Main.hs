import XMonad
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Layout.ThreeColumns (ThreeCol (..))

main :: IO ()
main =
  xmonad =<< xmobar cfg
  where
    cfg =
      def
        { modMask = mod4Mask, -- Use Super instead of Alt
          terminal = "myst",
          layoutHook = layoutHook def ||| ThreeColMid 1 (3 / 100) (1 / 2)
        }
import qualified Data.Map.Strict as M
import XMonad
import XMonad.Hooks.DynamicLog (dzen)
import XMonad.Layout.ThreeColumns (ThreeCol (..))

main :: IO ()
main =
  xmonad =<< dzen cfg
  where
    cfg =
      def
        { modMask = mod4Mask, -- Use Super instead of Alt
          terminal = "myst",
          layoutHook = layoutHook def ||| ThreeColMid 1 (3 / 100) (1 / 2),
          keys = myKeys
        }
    myKeys baseConfig@XConfig {modMask = modKey} =
      keys def baseConfig
        <> M.fromList
          [ ((modKey, xK_q), restart "/run/current-system/sw/bin/xmonad" True),
            ((modKey, xK_f), spawn "screenshot")
            -- ...
          ]

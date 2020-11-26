import qualified Data.Map.Strict as M
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (dzenWithFlags)
import XMonad.Layout.ThreeColumns (ThreeCol (..))

main :: IO ()
main =
  xmonad =<< statusBar cfg
  where
    cfg =
      desktopConfig
        { modMask = mod4Mask, -- Use Super instead of Alt
          terminal = "myst",
          layoutHook = ThreeColMid 1 (3 / 100) (1 / 2) ||| layoutHook def,
          keys = myKeys
        }
    myKeys baseConfig@XConfig {modMask = modKey} =
      keys def baseConfig
        <> M.fromList
          [ ((modKey, xK_q), restart "/run/current-system/sw/bin/xmonad" True),
            ((modKey, xK_f), spawn "screenshot")
            -- ...
          ]

statusBar =
  -- -dock is necessary for https://github.com/xmonad/xmonad/issues/21
  dzenWithFlags "-dock -fn CascadiaCode:pixelsize=26"
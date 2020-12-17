{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Map.Strict as M
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks)
import XMonad.Layout.ThreeColumns (ThreeCol (..))

main :: IO ()
main = do
  xmonad $ docks $ ewmh cfg
  where
    cfg =
      def
        { modMask = mod4Mask, -- Use Super instead of Alt
          terminal = "myst",
          layoutHook = avoidStruts $ ThreeColMid 1 (3 / 100) (1 / 2) ||| layoutHook def,
          keys = myKeys
        }
    myKeys baseConfig@XConfig {modMask = modKey} =
      keys def baseConfig
        <> M.fromList
          [ ((modKey, xK_q), restart "/run/current-system/sw/bin/xmonad" True),
            ((modKey, xK_f), spawn "screenshot"),
            ((modKey, xK_b), sendMessage ToggleStruts)
            -- ...
          ]

{- old status bar; remove after configuring taffybar

myStatusBar =
  statusBar dzenCli pp toggleStrutsKey
  where
    -- -dock is necessary for https://github.com/xmonad/xmonad/issues/21
    -- https://github.com/xmonad/xmonad-contrib/pull/203
    dzenCli = "dzen2 -dock -fn CascadiaCode:pixelsize=26"
    pp =
      dzenPP
        { -- ppSep = "🔥", Neither unicode, nor emoji work with dzen2
          ppTitleSanitize =
            shorten 15 . dzenEscape,
          ppExtras =
            [ padL $ pure $ Just "|",
              battery,
              padL $ pure $ Just "|",
              moment
            ]
        }
    toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)

moment :: Logger
moment = do
  now <- liftIO getZonedTime
  pure $ do
    pure $ formatTime defaultTimeLocale "%d/%a %R" now

battery :: Logger
battery = do
  s <-
    fmap trim . liftIO . readFile $
      "/sys/class/power_supply/BAT0/capacity"
  pure $ do
    pct <- readMaybe @Int s
    let fmt
          | pct < 33 = dzenColor "white" "red"
          | pct < 66 = dzenColor "white" "orange"
          | otherwise = id
    pure $ fmt $ show pct <> "%"
  where
    trim = T.unpack . T.strip . T.pack

-}

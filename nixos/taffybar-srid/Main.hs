{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Foldable (traverse_)
import GI.Gtk.Objects.Widget (Widget)
import System.Log.Logger
  ( Priority (DEBUG),
    getLogger,
    saveGlobalLogger,
    setLevel,
  )
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.CPU (cpuLoad)
import System.Taffybar.SimpleConfig (SimpleTaffyConfig (endWidgets), barHeight, defaultSimpleTaffyConfig, startWidgets, toTaffyConfig)
import System.Taffybar.Widget (defaultClockConfig, defaultWorkspacesConfig, textClockNewWith, workspacesNew)
import System.Taffybar.Widget.Generic.Graph (GraphConfig (graphDataColors), defaultGraphConfig, graphLabel)
import System.Taffybar.Widget.Generic.PollingGraph (pollingGraphNew)

main :: IO ()
main =
  startTaffybar $ toTaffyConfig cfg

cfg :: SimpleTaffyConfig
cfg =
  defaultSimpleTaffyConfig
    { startWidgets =
        [ workspacesW
        ],
      endWidgets =
        [ clockW,
          cpuW
        ],
      barHeight = 50
    }

workspacesW :: TaffyIO Widget
workspacesW = workspacesNew defaultWorkspacesConfig

clockW :: TaffyIO Widget
clockW = textClockNewWith defaultClockConfig

cpuW :: TaffyIO Widget
cpuW = pollingGraphNew cpuCfg 0.5 cpuCallback
  where
    cpuCfg =
      defaultGraphConfig
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
          graphLabel = Just "cpu"
        }

    cpuCallback :: IO [Double]
    cpuCallback = do
      (_, systemLoad, totalLoad) <- cpuLoad
      return [totalLoad, systemLoad]

_enableDebugLogging :: IO ()
_enableDebugLogging = do
  traverse_ (saveGlobalLogger . setLevel DEBUG)
    =<< sequence
      [ getLogger "",
        getLogger "System.Taffybar",
        getLogger "StatusNotifier.Tray"
      ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import System.Taffybar (startTaffybar)
import System.Taffybar.Information.CPU (cpuLoad)
import System.Taffybar.SimpleConfig (SimpleTaffyConfig (endWidgets), defaultSimpleTaffyConfig, startWidgets, toTaffyConfig)
import System.Taffybar.Widget (defaultWorkspacesConfig, sniTrayNew, workspacesNew)
import System.Taffybar.Widget.Generic.Graph (GraphConfig (graphDataColors), defaultGraphConfig, graphLabel)
import System.Taffybar.Widget.Generic.PollingGraph (pollingGraphNew)

main :: IO ()
main = do
  let cpuCfg =
        defaultGraphConfig
          { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
            graphLabel = Just "cpu"
          }
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      workspaces = workspacesNew defaultWorkspacesConfig
      simpleConfig =
        defaultSimpleTaffyConfig
          { startWidgets = [workspaces],
            endWidgets = [sniTrayNew, cpu]
          }
  startTaffybar $ toTaffyConfig simpleConfig

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

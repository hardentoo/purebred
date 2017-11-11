{-# LANGUAGE DataKinds #-}
module UI.GatherHeaders.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Types
import UI.Actions

interactiveGatherHeadersKeybindings :: [Keybinding 'GatherHeaders (T.Next AppState)]
interactiveGatherHeadersKeybindings =
  [Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)]

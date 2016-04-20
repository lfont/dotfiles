module XMonad.Prompt.FuzzyWindowPrompt (
                                        fuzzyWindowPromptGoto,
                                        fuzzyWindowPromptBring
                                       ) where

import XMonad
import XMonad.Prompt
import XMonad.Actions.WindowBringer
import qualified XMonad.StackSet as W

import qualified Data.Map as M

import Text.Fuzzy

-- Fuzzy window prompt
data FuzzyWindowPrompt = Goto | Bring
instance XPrompt FuzzyWindowPrompt where
    showXPrompt Goto      = "Go to window: "
    showXPrompt Bring     = "Bring window: "
    commandToComplete _ c = c
    nextCompletion      _ = getNextCompletion

fuzzyWindowPromptGoto, fuzzyWindowPromptBring :: XPConfig -> X ()
fuzzyWindowPromptGoto  = doPrompt Goto
fuzzyWindowPromptBring = doPrompt Bring

-- | Pops open a prompt with window titles. Choose one, and you will be
-- taken to the corresponding workspace.
doPrompt :: FuzzyWindowPrompt -> XPConfig -> X ()
doPrompt t c = do
  a <- case t of
         Goto  -> fmap gotoAction  windowMap
         Bring -> fmap bringAction windowMap
  wm <- windowMap
  mkXPrompt t c (compList wm) a

    where
      winAction a m = flip whenJust (windows . a) . flip M.lookup m
      gotoAction    = winAction W.focusWindow
      bringAction   = winAction bringWindow

      compList m = return . flip simpleFilter (map fst $ M.toList m)

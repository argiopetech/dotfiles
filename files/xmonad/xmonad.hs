{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Main where
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.Map as M
import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.Plane
import XMonad.Actions.TopicSpace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.AutoMaster
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

myTerm :: String
myTerm = "urxvtc"

spawnShell :: X ()
spawnShell = spawn myTerm

-- | Applies Mirror to flip a layout 90°
data Flip = NoFlip
          | Flip
    deriving (Typeable, Show, Read)

instance Message Flip

data Mirrorable layout a = Mirrorable Flip (layout a) deriving (Show, Read)

-- a nice pure layout, lots of properties for the layout, and its messages, in Properties.hs
instance (LayoutClass layout a) => LayoutClass (Mirrorable layout) a where
    runLayout (W.Workspace i (Mirrorable NoFlip l) ms) r = second (fmap (Mirrorable NoFlip))
                                                             `fmap` runLayout (W.Workspace i l ms) r
    runLayout (W.Workspace i (Mirrorable Flip   l) ms) r = second (fmap (\(Mirror layout) -> Mirrorable Flip layout))
                                                             `fmap` runLayout (W.Workspace i (Mirror l) ms) r

--    pureLayout (Mirrorable NoFlip t) r s = pureLayout t r s
--    pureLayout (Mirrorable Flip   t) r s = (second mirrorRect) `fmap` pureLayout t (mirrorRect r) s

    pureMessage (Mirrorable f t) m =
            msum [ fmap (flip f)   (fromMessage m)
                 , fmap (Mirrorable f) (pureMessage t m)]

      where flip Flip   Flip   = Mirrorable NoFlip t
            flip Flip   NoFlip = Mirrorable NoFlip t
            flip NoFlip Flip   = Mirrorable Flip   t
            flip NoFlip NoFlip = Mirrorable NoFlip t

    description (Mirrorable NoFlip t) = description t
    description (Mirrorable Flip   t) = description (Mirror t)

mirrorable :: layout a -> Mirrorable layout a
mirrorable = Mirrorable NoFlip

myTall :: Mirrorable Tall a
myTall = mirrorable tiled


-- default tiling algorithm partitions the screen into two panes
tiled :: Tall a
tiled   = Tall nmaster delta ratio
    where
     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

topics :: [Topic]
topics = [ "Web-1" , "Web-2"
         , "Mail"  , "Attachments"
         , "BASE-1", "BASE-2"
         , "CHOP-1", "CHOP-2"
         , "Misc-1", "Misc-2"
         , "Music" , "Video"]

topicConfig :: TopicConfig
topicConfig = defaultTopicConfig
    { topicDirs = M.fromList $
                [ ("conf", "w/conf")
                , ("dashboard", "Desktop")
                , ("yi", "w/dev-haskell/yi")
                , ("darcs", "w/dev-haskell/darcs")
                , ("haskell", "w/dev-haskell")
                , ("xmonad", "w/dev-haskell/xmonad")
                , ("tools", "w/tools")
                , ("movie", "Movies")
                , ("talk", "w/talks")
                , ("music", "Music")
                , ("documents", "w/documents")
                , ("pdf", "w/documents")
                ]
    , defaultTopicAction = const $ spawnShell >*> 3
    , defaultTopic = "dashboard"
    , topicActions = M.fromList $
                     [ ("conf",       spawnShell)
                     , ("darcs",      spawnShell >*> 3)
                     , ("yi",         spawnShell >*> 3)
                     , ("haskell",    spawnShell >*> 2)
                     , ("dashboard",  spawnShell)
                     , ("twitter",    spawnShell)
                     , ("movie",      spawnShell)
                     , ("documents",  spawnShell >*> 2)
                     ]
    }

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
--    _ <- spawn "liferea"
    xmonad $ ewmh defaultConfig
     { modMask = mod1Mask
     , workspaces = topics
     , terminal = myTerm
     , manageHook = manageDocks <+> manageHook defaultConfig
     , layoutHook = avoidStruts $ smartBorders $
                    myTall
                ||| Full
                ||| renamed [Replace "CenterGrid"] (centerMaster Grid)
                ||| renamed [Replace "AutoMaster"] (autoMaster 1 (1/100) Grid)
     , focusedBorderColor = "#ffffff"
     , normalBorderColor = "#bbbbbb"
     , logHook = dynamicLogWithPP xmobarPP
                     { ppOutput = hPutStrLn xmproc
--                     , ppTitle = xmobarColor "green" "" . shorten 100
                     , ppTitle = xmobarColor "#2ecc40" "" . shorten 100
                     , ppCurrent = xmobarColor "#ffcc00" "" . wrap "[" "]"
                     }
     }
     `removeKeysP`
     ["M-S-/"]
     `additionalKeysP`
     [ -- Moving workspaces
--       ("M-<Left>",     prevWS )
--     , ("M-<Right>",    nextWS )
       ("M-<Left>",     planeMove (Lines 6) Circular ToLeft )
     , ("M-<Right>",    planeMove (Lines 6) Circular ToRight )
     , ("M-<Up>",       planeMove (Lines 6) Circular ToUp )
     , ("M-<Down>",     planeMove (Lines 6) Circular ToDown )
     , ("M-S-<Left>",     planeShift (Lines 6) Circular ToLeft )
     , ("M-S-<Right>",    planeShift (Lines 6) Circular ToRight )
     , ("M-S-<Up>",       planeShift (Lines 6) Circular ToUp )
     , ("M-S-<Down>",     planeShift (Lines 6) Circular ToDown )
       -- Moving and focusing windows
     , ("M-j", windows W.focusUp)
     , ("M-k", windows W.focusDown)
     , ("M-S-j", windows W.swapUp)
     , ("M-S-k", windows W.swapDown)
       -- Override M-p launcher
     , ("M-p",          spawn "dmenu_run -b -f -i -nb black -nf white -sb red -sf white -p '>' -fn 'xft:DejaVuSansMono:size=14'")
     , ("M4-l",         spawn "xtrlock -b" >> (liftIO $ threadDelay 750000) >> spawn "xset dpms force standby")
     , ("M4-b",         (liftIO $ threadDelay 750000) >> spawn "xset dpms force standby")
     , ("M-b",          sendMessage ToggleStruts)
     , ("M-<Print>",    spawn "xrandr --output DFP1 --right-of LVDS")
     , ("M-S-<Print>",  spawn "xrandr --output DFP1 --same-as LVDS")
     , ("M-<F12>",      spawn "urxvtc -e mpv '/home/elliot/Music/Hardcore Mix 180 bpm (part2)-poZPLmc1tgg.mp4'")
     , ("M-<F11>",      spawn "urxvtc -e mpv --start=1 '/home/elliot/Music/Dramatic.mp4'")
     , ("M-<F10>",      spawn "urxvtc -e mpv '/home/elliot/Music/Knowing_is_half_the_battle.mp4'")
     , ("M-<F9>",       spawn "urxvtc -e mpv --start 12 '/home/elliot/Videos/family_guy_men_dont_know.mp4'")
     , ("<F9>",         spawn "pkill -f chrome --signal STOP")
     , ("<F10>",        spawn "pkill -f chrome --signal CONT")
     , ("M-g",          gridselectWorkspace (defaultGSConfig { gs_navigate = navNSearch }) W.view)
     , ("M-m",          sendMessage Flip)
     ]

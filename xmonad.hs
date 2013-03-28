{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad
import XMonad.Core
import XMonad.Config.Kde
import XMonad.Layout
import XMonad.Config.Desktop
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.FixedColumn
import XMonad.Layout.Decoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.MouseResizableTile
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Layout.Renamed
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.DraggableWindows (makeDraggable, makeDraggableWindowSwitcher)
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
import qualified XMonad.StackSet as S
import Control.Monad
import Foreign.C.Types(CInt)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
--import XMonad.Actions.UpdatePointer

import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M

-- Define Terminal
myTerminal      = "urxvt"

myModMask = mod4Mask

main = xmonad kde4Config
    { modMask = myModMask,-- use the Windows button as mod
      keys = myKeys <+> keys kde4Config 
    , manageHook = manageHook kde4Config <+> myManageHook
    , layoutHook = myLayoutHook                   
    , logHook = takeTopFocus <+> logHook kde4Config
    }
 
myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
    ]
  where myFloats      = ["MPlayer", "Gimp"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2
        ircApps       = ["Ksirc"]                -- open on desktop 3
        
conditionalCycleUp = do	
    theLayout <- gets $ description . W.layout . W.workspace . W.current . windowset
    --spawn ("kdialog -msgbox " ++ theLayout)
    case theLayout of
      "twoPaneLayout" -> rotFocusedDown
      _ -> rotAllDown	
          
conditionalCycleDown =  do 
    theLayout <- gets $ description . W.layout . W.workspace . W.current . windowset
    --spawn ("kdialog -msgbox " ++ theLayout)
    case theLayout of
	 "twoPaneLayout" -> rotAllDown
	 _ -> rotAllDown

myKeys (XConfig {modMask = modm}) = M.fromList $ 
    [ ((mod4Mask, xK_space), sendMessage NextLayout),
      ((mod1Mask, xK_Tab), conditionalCycleUp),
      ((mod1Mask .|. shiftMask, xK_Tab), (conditionalCycleDown)),
      ((controlMask , xK_Right), nextWS),
      ((controlMask , xK_Left), prevWS),
      ((controlMask .|. mod1Mask, xK_Right), shiftToNext),
      ((controlMask .|. mod1Mask, xK_Left), shiftToPrev),
      ((controlMask .|. mod1Mask .|. shiftMask, xK_Right), shiftToNext >> nextWS),
      ((controlMask .|. mod1Mask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    ]   
    
myDeco=windowSwitcherDecorationWithImageButtons shrinkText defaultThemeWithButtons
    
mySplitLayout = named "mySplitLayout" $ desktopLayoutModifiers $ myDeco $ draggingVisualizer $ magnifiercz' 1.4 $ Tall nmaster delta ratio
    where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 60/100
    
-- twoPaneLayout = named "twoPaneLayout" $ draggingVisualizer $ myDeco $ limitWindows 2 $ mouseResizableTile { draggerType = BordersDragger}
twoPaneLayout =  named "twoPaneLayout" $ limitWindows 2 $ desktopLayoutModifiers  $ myDeco $ draggingVisualizer $ mouseResizableTile{ nmaster = 1
                                                                                                         , masterFrac = ratio
                                                                                                         , slaveFrac = delta
                                                                                                         , draggerType = BordersDragger} 
  where
                                                                                                           
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 60/100
        
myFull = desktopLayoutModifiers $ Full

myGrid = named "myGrid" $ desktopLayoutModifiers $ myDeco $ Grid

myLayoutHook = smartBorders $ myFull ||| twoPaneLayout ||| mySplitLayout ||| myGrid

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [

    ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))
  
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
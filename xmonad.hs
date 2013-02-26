import XMonad
import XMonad.Config.Kde
import XMonad.Layout
import XMonad.Config.Desktop
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.FixedColumn
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.MouseResizableTile
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Layout.Renamed
--import XMonad.Actions.UpdatePointer

import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M

main = xmonad kde4Config
    { modMask = mod4Mask,-- use the Windows button as mod
      keys = myKeys <+> keys kde4Config 
    , manageHook = manageHook kde4Config <+> myManageHook
    , layoutHook = myLayoutHook
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
	 "twoPaneLayout" -> rotFocusedDown
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
    
mySplitLayout = renamed [Replace "mySplitLayout"] $ deco shrinkText defaultTheme $ magnifiercz' 1.4 $ Tall nmaster delta ratio
    where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 60/100
  
twoPaneLayout = renamed [Replace "twoPaneLayout"] $ deco shrinkText defaultTheme $ limitWindows 2 $ mouseResizableTile { draggerType = BordersDragger}

deco = noFrillsDeco

myLayoutHook = desktopLayoutModifiers $ smartBorders $ Full ||| mySplitLayout ||| twoPaneLayout ||| Grid  

        
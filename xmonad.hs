import XMonad
import XMonad.Config.Kde
import XMonad.Layout
import XMonad.Config.Desktop
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.FixedColumn
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoFrillsDecoration
import qualified XMonad.StackSet as W -- to shift and float windows
 
main = xmonad kde4Config
    { modMask = mod4Mask -- use the Windows button as mod
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
        
twoPaneLayout = magnifiercz' 1.4 $ Tall nmaster delta ratio
    where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 60/100
        
fullWithMagLayout = limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 80 10

codeLayout = limitWindows 3 $ FixedColumn 1 20 80 10

deco = noFrillsDeco

myLayoutHook = desktopLayoutModifiers $ deco shrinkText defaultTheme $ smartBorders $ Full ||| fullWithMagLayout ||| twoPaneLayout ||| codeLayout ||| Grid  

        
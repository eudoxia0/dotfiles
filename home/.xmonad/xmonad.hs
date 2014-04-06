import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import System.Exit

myMod = mod4Mask
myTerm = "urxvt"
myBrowser = "firefox"

main = do
    xmonad $ defaultConfig
        { modMask     = myMod
        , borderWidth = 0
        , terminal    = myTerm
        , workspaces  = ["α","β","γ","δ","ε","ζ","η"]
        } `additionalKeys`
        [ ((myMod, xK_c),
           spawn myTerm)
        , ((myMod, xK_r),
           spawn "gmrun")
        , ((myMod .|. controlMask, xK_w),
           spawn myBrowser)
        , ((myMod .|. controlMask, xK_f),
           spawn "pcmanfm")
        , ((myMod .|. controlMask, xK_e),
           spawn "emacs")
        , ((myMod .|. controlMask, xK_o),
           spawn "tor-browser-en")
        , ((myMod .|. controlMask, xK_t),
           spawn "ntorrent")
        , ((myMod .|. controlMask, xK_b),
           spawn "calibre")
        , ((myMod .|. controlMask, xK_i),
           spawn "pidgin")
        , ((myMod, xK_x),
           spawn "xscreensaver-command -lock")
        , ((myMod, xK_q),
           kill)
        , ((myMod .|. shiftMask, xK_q),
           io (exitWith ExitSuccess))
        , ((myMod .|. shiftMask, xK_r),
           restart "xmonad" True)
        ]

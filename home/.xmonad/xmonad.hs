import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = 0
        , startupHook = do
            spawn "xcompmgr -c -t-5 -l-5 -r4.2 -o.55 &"
        } `additionalKeys`
        [ ((mod4Mask .|. controlMask, xK_c), spawn "chromium-browser")
        , ((mod4Mask .|. controlMask, xK_x), spawn "xchat")
        , ((mod4Mask .|. controlMask, xK_i), spawn "pidgin")
        , ((mod4Mask .|. controlMask, xK_e), spawn "emacs")
        , ((mod4Mask .|. controlMask, xK_f), spawn "pcmanfm")
        , ((mod4Mask, xK_Return), spawn "rxvt-unicode")
        , ((mod4Mask .|. shiftMask, xK_c), kill)
        , ((mod4Mask, xK_r), spawn "gmrun")
        , ((mod4Mask, xK_x), spawn "xscreensaver-command -lock")
        ]

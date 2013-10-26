import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog

ws = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

customKeys =   [
         -- workspaces are distinct by screen
          ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
               | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
               , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]
         ++
         [
         -- swap screen order
         ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust
          (windows . f))
               | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
               , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
         ++
         -- Applications
         [ ((mod4Mask .|. controlMask, xK_c), spawn "chromium-browser")
        , ((mod4Mask .|. controlMask, xK_x), spawn "xchat")
        , ((mod4Mask .|. controlMask, xK_i), spawn "pidgin")
        , ((mod4Mask .|. controlMask, xK_e), spawn "emacs")
        , ((mod4Mask .|. controlMask, xK_f), spawn "pcmanfm")
        , ((mod4Mask .|. controlMask, xK_a), spawn "audacious")
        , ((mod4Mask .|. controlMask, xK_o), spawn "xosview")
        , ((mod4Mask, xK_Return), spawn "rxvt-unicode")
        , ((mod4Mask .|. shiftMask, xK_c), kill)
        , ((mod4Mask, xK_r), spawn "gmrun")
        , ((mod4Mask, xK_x), spawn "xscreensaver-command -lock")
        ]

conf = defaultConfig {
        workspaces = ws
        , terminal = "urxvt"
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = 0
        , startupHook = do
            spawn "bash .xinitrc"
  } `additionalKeys` customKeys

main = xmonad conf

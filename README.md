# praecones

One day I got tired of not being able to have both my XMonad state and a
second-accuracy system clock print out to dzen2 without blocking on
the next [DynamicLog](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-DynamicLog.html)
tick, so I wrote this utility.

## Example

Build and invoke from your `xmonad.hs` main function:

```hs
main :: IO ()
main = xmonad =<< statusBar "praecones" def (const (mod4Mask,xK_b)) myConfig
```

## About

My xmonad config - quite early in development, plenty to do before it's useful or warrants a screenshot. I have little to no knowledge of Haskell at the moment, so if you can see some improvements to my coding I'm all ears.

## Design concepts

I run a tripple monitor setup and Dvorak keyboard layout (and am considering moving to the workman layout). Because of this, keybindings are probably going to be quite different to the norm. I'm also going to be quite focused on Xinerama based movements too, so this layout won't be for the everyday user.

## Key Bindings

![Alt text](https://raw.github.com/Libbum/xmonad/master/keymap.png "Current Keymap")

## Todo

- [X] Sort out Gimp layout (changed to single window and sunk default floating behaviour)
- [ ] Colour each visible workspace separately
- [X] Set up decent layouts 
- [ ] Individualise layouts; ie. overwrite default workspace layouts based on Xinerama screen - specifically layout flip (master on left for rhs monitor, master on right for lhs monitor) [Partially done - added M-m & M-M commands, want to ultimately hook this though...]
- [ ] fix dzen2 bar to my liking
- [X] Organise keymap better
- [ ] Maybe look into Floatkeys. Discussion on [StackOverflow](http://stackoverflow.com/questions/9157349/xmonad-when-floating-a-window-move-or-resize-it) (only useful if I use floats a lot - doesn't seem to be the case atm)
- [ ] Fix flash fullscreen - Think I've got too many hooks fighting for dominance...

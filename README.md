## About

My xmonad config - still quite actively changing. Screen shots will be updated regularly until I'm happy with everything. I have little to no knowledge of Haskell at the moment, so if you can see some improvements to my coding I'm all ears.

## Design concepts

I run a triple monitor setup and Dvorak keyboard layout (and am considering moving to the workman layout). Because of this, keybindings are probably going to be quite different to the norm. I'm also going to be quite focused on Xinerama based movements too, so this layout won't be for the everyday user.

## Key Bindings

![Alt text](https://raw.github.com/Libbum/xmonad/master/keymap.png "Current Keymap")

## Todo

- [X] Sort out Gimp layout (changed to single window and sunk default floating behaviour)
- [ ] Colour each visible workspace separately [Not sure if this is possible]
- [X] Set up decent layouts 
- [X] Individualise layouts; ie. overwrite default workspace layouts based on Xinerama screen - specifically layout flip (master on left for rhs monitor, master on right for lhs monitor) added M-m & M-M commands]
- [X] fix dzen2 bar to my liking [Colours may be a little overkill at the moment though...]
- [X] Possibly rethink this hybrid colour layout. Not sure if I'm happy with it at. [Now using [αποκλίνων](https://github.com/Libbum/vim-apoklinon)]
- [X] Organise keymap better
- [X] Fix flash fullscreen - Think I've got too many hooks fighting for dominance... [Removed ewmh]
- [X] fullsreen on top of floats [Removed ewmh]
- [ ] Look into Actions.OnSrceen to see if it's of any use

These are my personal dotfiles for my Arch Linux setup. They are messy AF but they work fine for me.

**Please do not blindly clone this repo and install the dotfiles on your system. They have very specific settings that won't work for everyone. Only use this repo as reference for writing your own configs.**

If you still want to use some of them, here's how:

```cd ~
git clone https://github.com/bandithedoge/dotfiles.git
cd dotfiles
stow <whatever you want to use>
```

If you want to replace your existing configs: `stow <whatever you want to use> --adopt`

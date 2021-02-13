# dotfiles
Personal dotfiles.

# Configuration
Files can be set-up using the `dotfiles.py` command line tool:
```sh
λ ./dotfiles.py (stow|unstow|restow) <folder> (<packages>*) [-t|--to <target>]
λ
λ # Stow an entire folder
λ ./dotfiles.py stow .config
λ
λ # Stow one or more packages
λ ./dotfiles.py stow .config bspwm dunst
λ
λ # Stow on a manually-specified target folder
λ ./dotfiles.py stow .config polybar -t $HOME/.config-alt
λ
λ # Stow on an automatically-determined target folder
λ # This makes use of the `TO_OVERRIDES` constant within
λ # `dotfiles.py` in order to point `.home` to `$HOME`.
λ ./dotfiles.py stow .home
```

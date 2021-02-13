# dotfiles
Personal dotfiles.

# Configuration
Files can be set-up using the `dotfiles.py` command line tool:
```sh
λ ./dotfiles.py (stow|unstow|restow) <folder> [<package>] [-t|--to <target>]
λ
λ # Stow an entire folder
λ ./dotfiles.py stow .config
λ
λ # Stow a specific package
λ ./dotfiles.py stow .config bspwm
λ
λ # Stow on a manually-specified target folder
λ ./dotfiles.py stow .config polybar -t $HOME/.config-alt
λ
λ # Stow on an automatically-determined target folder
λ # This makes use of the `TO_OVERRIDES` constant within
λ # `dotfiles.py` in order to point `.home` to `$HOME`.
λ ./dotfiles.py stow .home
```

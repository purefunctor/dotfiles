# Configuration File for User Paths

export EDITOR="/usr/bin/nvim"

# ghcup
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

# pyenv
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"

# poetry
source "$HOME/.poetry/env"

# rustup
source "$HOME/.cargo/env"

# nvm
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# nix
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi

# Remove Duplicates
export PATH=$(python -c "import os;print(':'.join(dict.fromkeys(os.getenv('PATH').split(':')).keys()))")

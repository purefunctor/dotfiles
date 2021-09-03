# Configuration File for User Paths

export EDITOR="/usr/bin/nvim"

# ghcup
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

# poetry
source "$HOME/.poetry/env"

# rustup
# source "$HOME/.cargo/env"

# nvm
# export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

export PNPM_HOME="/home/pure/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"

# erl
source "$HOME/.kerl/installations/24.0.5/activate"
export PATH="$HOME/.cache/rebar3/bin:$PATH"

# elixir
source "$HOME/.kiex/scripts/kiex"
source "$HOME/.kiex/elixirs/elixir-1.12.2.env"

# nix
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi

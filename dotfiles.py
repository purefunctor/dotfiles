#!/usr/bin/python
from argparse import ArgumentParser
from pathlib import Path
from subprocess import run
from typing import Callable


HERE = Path.cwd()
HOME = Path.home()

TO_OVERRIDES = {
    Path(".home"): HOME,
}

CLI_PARSER = ArgumentParser("dotfiles")
CLI_PARSER.add_argument("action", choices=["stow", "unstow", "restow"])
CLI_PARSER.add_argument("folder")
CLI_PARSER.add_argument("packages", nargs="*")
CLI_PARSER.add_argument("-t", "--to", default=None)


def _stow(command: str, package: str, source: Path, target: Path) -> None:
    run(["stow", command, package, "-d", source, "-t", target])


def stow(package: str, source: Path, target: Path) -> None:
    _stow("-S", package, source, target)


def unstow(package: str, source: Path, target: Path) -> None:
    _stow("-D", package, source, target)


def restow(package: str, source: Path, target: Path) -> None:
    _stow("-R", package, source, target)


if __name__ == "__main__":
    actions = {"stow": stow, "unstow": unstow, "restow": restow}
    args = CLI_PARSER.parse_args()
    action = actions[args.action]

    if len(args.packages) == 0:
        folder = Path(args.folder)
        for package in folder.iterdir():
            if args.to is None:
                to = HOME / args.folder / package.name
            else:
                to = Path(args.to)

            if not to.exists():
                to.mkdir(parents=True)

            to = TO_OVERRIDES.get(folder, to)

            action(package.name, args.folder, to)

    else:
        for package in args.packages:
            action(
                package, args.folder, HOME / args.folder / package
            )

#!/usr/bin/python
from argparse import ArgumentParser
from itertools import repeat
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
CLI_PARSER.add_argument("-v", action="count")


def _stow(
    command: str, package: str, source: Path, target: Path, verbosity: str
) -> None:
    run(["stow", command, package, "-d", source, "-t", target, verbosity])


if __name__ == "__main__":
    actions = {"stow": "-S", "unstow": "-D", "restow": "-R"}
    args = CLI_PARSER.parse_args()
    action = actions[args.action]

    if args.v is None:
        verbosity = ""
    else:
        verbosity = "-" + "".join(repeat('v', args.v))

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

            _stow(
                action,
                package.name,
                args.folder,
                to,
                verbosity
            )

    else:
        for package_name in args.packages:
            _stow(
                action,
                package_name,
                args.folder,
                HOME / args.folder / package_name,
                verbosity,
            )

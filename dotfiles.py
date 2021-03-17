#!/usr/bin/python
from argparse import ArgumentParser
from pathlib import Path
from subprocess import run

HOME = Path.home()

RECURSE_INTO = { ".config" }

CLI_PARSER = ArgumentParser("dotfiles")
CLI_PARSER.add_argument("sources", nargs="*")


def _link(source: Path, target: Path) -> None:
    arguments = [
        "ln", "-sf", str(Path("dotfiles") / source.absolute()), "-t", str(target), "-v"
    ]

    run(arguments)


if __name__ == "__main__":
    args = CLI_PARSER.parse_args()

    sources = [
        Path(source) for source in args.sources
    ] or Path("dotfiles").iterdir()

    for source in sources:
        if source.name in RECURSE_INTO:
            for inner_source in source.iterdir():
                _link(inner_source, HOME / source.name)
        else:
            _link(source, HOME)

#!/usr/bin/python
from subprocess import run
import sys

step = "5%"

current = int(run(["brightnessctl", "g"], capture_output=True).stdout)
maximum = int(run(["brightnessctl", "m"], capture_output=True).stdout)
percent = current * 100 // maximum

icons = [
    (90, ""),
    (75, ""),
    (50, ""),
    (40, ""),
    (25, ""),
    (10, ""),
    ( 0, ""),
]

def get_text():
    expr = ((icon, abs(percent - bound)) for bound, icon in icons)
    icon, _ = min(expr, key=lambda x: x[1])
    print(f"{icon} {percent}%")


def increase_brightness():
    run(["brightnessctl", "s", f"{step}+"])


def decrease_brightness():
    run(["brightnessctl", "s", f"{step}-"])


if __name__ == "__main__":
    if "increase" in sys.argv:
        increase_brightness()
    elif "decrease" in sys.argv:
        decrease_brightness()
    else:
        get_text()

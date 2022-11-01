import os
from subprocess import run

import pykeepass

from rofi import Rofi

r = Rofi()

path = os.path.expanduser("~/keepass/pass.kdbx")


def format_entry(entry: pykeepass.entry.Entry) -> str:
    return f"{entry.title}: {entry.username}"


def type(string: str) -> None:
    run(["xdotool", "type", string])


def main() -> None:
    kp = None

    while True:
        password = r.text_entry("password", rofi_args=["-password"])
        if password is None:
            exit(1)
        else:
            try:
                r.status("Opening database...")
                kp = pykeepass.PyKeePass(path, password)
                r.close()
            except pykeepass.exceptions.CredentialsError:
                continue
        break

    while True:
        index, _ = r.select("entries", map(format_entry, kp.entries), rofi_args=["-i"])
        if index == -1:
            exit(1)

        selected_entry = kp.entries[index]

        index, _ = r.select(
            "field",
            ["Username", "Password", "OTP"],
        )

        match index:
            case -1:
                continue
            # username
            case 0:
                type(selected_entry.username)
            # password
            case 1:
                type(selected_entry.password)
            # otp
            case 2:
                type(selected_entry.otp)
        break

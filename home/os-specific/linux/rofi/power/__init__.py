import rofi_menu

def main():
    rofi_menu.run(
        rofi_menu.Menu(
            prompt="power",
            items=[
                rofi_menu.ShellItem("Lock", "loginctl lock-session"),
                rofi_menu.ShellItem("Reboot", "systemctl reboot"),
                rofi_menu.ShellItem("Shutdown", "systemctl poweroff"),
            ],
        )
    )

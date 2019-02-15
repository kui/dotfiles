import sys
import os
import datetime
import subprocess
import string

import ckit

from keyhac import *
from ckit.ckit_const import *

IGNORED_APP_NAMES = [
    "org.gnu.Emacs",
    "com.apple.Terminal",
    "com.googlecode.iterm2",
    "com.factorio",
]

IGNORED_WINDOW_TITLES = [
    "Alacritty",
]

IDE_APP_NAMES = [
    "com.jetbrains.intellij",
]

BROWSER_APP_NAMES = [
    "com.google.Chrome",
]

TERMINAL_APP_NAMES = [
    "com.apple.Terminal",
    "com.googlecode.iterm2",
]

def configure(keymap):
    global is_marked

    keymap.editor = "Emacs"
    keymap.setFont( "Osaka-Mono", 16 )
    keymap.setTheme("black")

    ########################################################
    # Global
    ########################################################
    def check_ignored_app(acc):
        app_name = get_app_name(acc)
        if app_name in IGNORED_APP_NAMES:
            return False

        try:
            window_title = acc["AXTitle"]
            if window_title in IGNORED_WINDOW_TITLES:
                return False
        except:
            pass

        return True
    global_keymap = keymap.defineWindowKeymap(check_func=check_ignored_app)
    global_mx_keymap = keymap.defineMultiStrokeKeymap()
    is_marked = False

    def send_keys(*keys):
        return keymap.InputKeyCommand(*list(keys))

    def forward_char():
        send_keys("Right")()
    def backward_char():
        send_keys("Left")()
    def forward_word():
        send_keys("Alt-Right")()
    def backward_word():
        send_keys("Alt-Left")()
    def prev_line():
        send_keys("Up")()
    def next_line():
        send_keys("Down")()

    def kill_region():
        reset_mark()
        send_keys("Cmd-X")()

    def kill_ring_save():
        reset_mark()
        send_keys("Cmd-C")()

    def backword_kill_word():
        send_keys("Cmd-X")()

    def backword_kill_word_or_region():
        global is_marked
        if is_marked:
            kill_region()
        else:
            backword_kill_word()

    def mark_set():
        global is_marked
        send_keys("D-LShift")()
        is_marked = True

    def reset_mark():
        global is_marked
        if is_marked:
            send_keys("U-LShift")()
            is_marked = False

    set_map(global_keymap, {
        "Ctrl-F": forward_char,
        "Ctrl-B": backward_char,
        "Cmd-F":  forward_word,
        "Cmd-B":  backward_word,
        "Ctrl-P": prev_line,
        "Ctrl-N": next_line,
        "Ctrl-A": "Cmd-Left",
        "Ctrl-E": "Cmd-Right",

        "Ctrl-V": "PageDown",
        "Cmd-V":  "PageUp",

        "Ctrl-W": backword_kill_word_or_region,
        "Cmd-W":  kill_ring_save,
        "Ctrl-Y": "Cmd-V",
        "Ctrl-K": ["Shift-Cmd-Right", "Cmd-X"],

        "Ctrl-H": "Back",
        "Ctrl-D": "Delete",
        "Cmd-D": "Alt-Delete",

        "Ctrl-S": "Cmd-F",
        "Ctrl-OpenBracket": "Esc",
        "Ctrl-Slash": "Cmd-Z",

        "Ctrl-G": reset_mark,
        "Ctrl-Space": mark_set,

        "Cmd-K": "Cmd-W",
        "Ctrl-X": global_mx_keymap,
    })
    set_map(global_mx_keymap, {
        "H": "Cmd-A",
        "Ctrl-S": "Cmd-S",
        "Ctrl-F": "Cmd-O",
        "Ctrl-C": "Cmd-Q",
    })

    ########################################################
    # IDE
    ########################################################
    def check_ide(acc):
        app_name = get_app_name(acc)
        return app_name in IDE_APP_NAMES

    def ide_kill_region():
        reset_mark()
        send_keys("Cmd-X")()

    ide_keymap = keymap.defineWindowKeymap(check_func=check_ide)
    ide_mx_keymap = keymap.defineMultiStrokeKeymap()
    set_map(ide_keymap, {
        "Ctrl-W": ide_kill_region,
        "Cmd-X": "Cmd-3",
        "Ctrl-O": "F3",
        "Cmd-I": "Cmd-E",
        "Ctrl-S": "Cmd-J",
        "Ctrl-Semicolon": "Cmd-Slash",
        "Cmd-Slash": "Ctrl-Space",
        "Ctrl-X": ide_mx_keymap,
    })
    set_map(ide_mx_keymap, {
        "H": "Cmd-A",
        "Ctrl-S": "Cmd-S",
        "Ctrl-F": "Shift-Cmd-R",
        "Ctrl-C": "Cmd-Q",
    })

    ########################################################
    # Web Browser
    ########################################################
    def check_web_browser(acc):
        app_name = get_app_name(acc)
        return app_name in BROWSER_APP_NAMES

    browser_keymap = keymap.defineWindowKeymap(check_func=check_web_browser)
    set_map(browser_keymap, {
        "Ctrl-T": "Cmd-T",
        "Ctrl-L": "Cmd-L",
        "Ctrl-R": "Cmd-R",
        "Shift-Ctrl-T": "Shift-Cmd-T",
        "Shift-Ctrl-L": "Shift-Cmd-L",
        "Shift-Ctrl-R": "Shift-Cmd-R",
        "Cmd-Shift-Comma": "Cmd-Up",
        "Cmd-Shift-Period": "Cmd-Down",
    })

    ########################################################
    # terminal app
    ########################################################
    def check_terminal_app(acc):
        app_name = get_app_name(acc)
        return app_name in TERMINAL_APP_NAMES
    terminal_app_keymap = keymap.defineWindowKeymap(check_func=check_terminal_app)
    cmd_map = {}
    for c in list(string.ascii_uppercase):
        cmd_map["Cmd-" + c] = "Alt-" + c
        cmd_map["Cmd-Shift-" + c] = "Alt-Shift-" + c
    set_map(terminal_app_keymap, cmd_map)

def get_app_name(acc_elem):
    return ckit.getApplicationNameByPid(acc_elem.pid)

def set_map(keymap, defined_map):
    for k, v in defined_map.items():
        keymap[k] = v

// TODO refactor some frequent patterns
// such as: { from: fromKey(...), to: [ sendKey(...) ] }

const Modes = {
  Unset: 0,
  Cx: 1,
  Mark: 2,
};

const baseExcludedApps = [
  '^org\\.gnu\\.Emacs$',
  '^org\\.gnu\\.AquamacsEmacs$',
  '^org\\.gnu\\.Aquamacs$',
  '^org\\.pqrs\\.unknownapp.conkeror$',
  '^com\\.microsoft\\.rdc$',
  '^com\\.microsoft\\.rdc\\.mac$',
  '^com\\.microsoft\\.rdc\\.macos$',
  '^com\\.microsoft\\.rdc\\.osx\\.beta$',
  '^net\\.sf\\.cord$',
  '^com\\.thinomenon\\.RemoteDesktopConnection$',
  '^com\\.itap-mobile\\.qmote$',
  '^com\\.nulana\\.remotixmac$',
  '^com\\.p5sys\\.jump\\.mac\\.viewer$',
  '^com\\.p5sys\\.jump\\.mac\\.viewer\\.web$',
  '^com\\.teamviewer\\.TeamViewer$',
  '^com\\.vmware\\.horizon$',
  '^com\\.2X\\.Client\\.Mac$',
  '^com\\.apple\\.Terminal$',
  '^com\\.googlecode\\.iterm2$',
  '^co\\.zeit\\.hyperterm$',
  '^co\\.zeit\\.hyper$',
  '^io\\.alacritty$',
  '^net\\.kovidgoyal\\.kitty$',
  '^org\\.vim\\.',
  '^com\\.qvacua\\.VimR$',
  '^com\\.vmware\\.fusion$',
  '^com\\.vmware\\.horizon$',
  '^com\\.vmware\\.view$',
  '^com\\.parallels\\.desktop$',
  '^com\\.parallels\\.vm$',
  '^com\\.parallels\\.desktop\\.console$',
  '^org\\.virtualbox\\.app\\.VirtualBoxVM$',
  '^com\\.vmware\\.proxyApp\\.',
  '^com\\.parallels\\.winapp\\.',
  '^org\\.x\\.X11$',
  '^com\\.apple\\.x11$',
  '^org\\.macosforge\\.xquartz\\.X11$',
  '^org\\.macports\\.X11$',
  '^com\\.microsoft\\.VSCode$'
];

const webBrowsers = [
  '^com\\.google\\.Chrome$',
];

const ides = [
  '^com\\.jetbrains\\.intellij$',
];

const terminals = [
  '^com\\.apple\\.Terminal$',
  '^com\\.googlecode\\.iterm2$',
];

module.exports = {
  title: `kui's Bindings`,
  rules: [
    ////////////////////////////////////////////////////////////
    {
      description: 'Terminal',
      manipulators: conditionsGroup([
        {
          type: 'frontmost_application_if',
          bundle_identifiers: terminals
        }
      ], [
        {
          from: fromKey('any? + left_option'),
          to: [ sendKey('left_command') ],
        },
        {
          from: fromKey('any? + left_command'),
          to: [ sendKey('left_option') ],
        },
        {
          from: fromKey('option + shift? + tab'),
          to: [ sendKey('command + tab') ],
        },
      ]),
    },

    /////////////////////////////////////////////////////////////
    {
      description: 'Web Browser',
      manipulators: conditionsGroup([
        {
          type: 'frontmost_application_if',
          bundle_identifiers: webBrowsers,
        },
      ], [
        {
          from: fromKey('control + shift? + t'),
          to: [ sendKey('command + t') ],
        },
        {
          from: fromKey('control + shift? + l'),
          to: [ sendKey('command + l') ],
        },
        {
          from: fromKey('control + shift? + r'),
          to: [ sendKey('command + r') ],
        },
        {
          from: fromKey('command + shift + period'),
          to: [ sendKey('command + down_arrow') ],
        },
        {
          from: fromKey('command + shift + comma'),
          to: [ sendKey('command + up_arrow') ],
        },
      ]),
    },

    /////////////////////////////////////////////////////////////
    {
      description: 'IDE',
      manipulators: conditionsGroup([
        {
          type: 'frontmost_application_if',
          bundle_identifiers: ides,
        },
      ], [
        {
          from: fromKey('command + x'),
          to: [ sendKey('command + 3') ],
        },
        {
          from: fromKey('command + i'),
          to: [ sendKey('command + e') ],
        },
        {
          from: fromKey('control + o'),
          to: [ sendKey('f3') ],
        },
        {
          from: fromKey('control + s'),
          to: [ sendKey('command + j') ],
        },
        {
          from: fromKey('command + slash'),
          to: [ sendKey('control + spacebar') ],
        },
        {
          conditions: [{
            type: 'variable_if',
            name: 'mode',
            value: Modes.Cx,
          }],
          from: fromKey('control + f'),
          to: [
            sendKey('command + shift + r'),
            unsetMode(),
          ],
        },
      ]),
    },

    /////////////////////////////////////////////////
    {
      description: 'Like Emacs',
      manipulators: conditionsGroup([
        {
          type: 'frontmost_application_unless',
          bundle_identifiers: baseExcludedApps,
        },
      ], [
        // Mark mode
        {
          from: fromKey('control + spacebar'),
          to: [ setMode(Modes.Mark) ],
        },
        ...conditionsGroup(
          [{
            type: 'variable_if',
            name: 'mode',
            value: Modes.Mark,
          }],
          [
            {
              from: fromKey('control + b'),
              to: [ sendKey('shift + left_arrow') ],
            },
            {
              from: fromKey('command + b'),
              to: [ sendKey('shift + option + left_arrow') ],
            },
            {
              from: fromKey('control + f'),
              to: [ sendKey('shift + right_arrow') ],
            },
            {
              from: fromKey('command + f'),
              to: [ sendKey('shift + option + right_arrow') ],
            },
            {
              from: fromKey('control + n'),
              to: [ sendKey('shift + down_arrow') ],
            },
            {
              from: fromKey('control + p'),
              to: [ sendKey('shift + up_arrow') ],
            },
            {
              from: fromKey('control + v'),
              to: [ sendKey('shift + page_down') ],
            },
            {
              from: fromKey('command + v'),
              to: [ sendKey('shift + page_up') ],
            },
            {
              from: fromKey('control + a'),
              to: [ sendKey('shift + command + left_arrow') ],
            },
            {
              from: fromKey('control + e'),
              to: [ sendKey('shift + command + right_arrow') ],
            },
            {
              from: fromKey('control + g'),
              to: [ unsetMode() ],
            },
          ]
        ),

        // C-x Prefix Bindings
        {
          from: fromKey('control + x'),
          to: [ setMode(Modes.Cx) ],
        },
        ...conditionsGroup(
          [{
            type: 'variable_if',
            name: 'mode',
            value: Modes.Cx,
          }],
          [
            {
              from: fromKey('h'),
              to: [
                sendKey('command + a'),
                unsetMode(),
              ],
            },
            {
              from: fromKey('control + s'),
              to: [
                sendKey('command + s'),
                unsetMode(),
              ],
            },
            {
              from: fromKey('control + f'),
              to: [
                sendKey('command + o'),
                unsetMode(),
              ],
            },
            {
              from: fromKey('control + c'),
              to: [
                sendKey('command + q'),
                unsetMode(),
              ],
            },
            {
              from: fromKey('control + g'),
              to: [ unsetMode() ],
            },

            // Escape C-x mode if unknown key
            // TODO Fix that this expression escapes mode if modifier key was pressed.
            {
              from: { any: 'key_code' },
              to: [ unsetMode() ],
            },
          ]
        ),

        // Delete (C-d, C-h)
        {
          from: fromKey('control + d'),
          to: [
            sendKey('delete_forward'),
            unsetMode(),
          ],
        },
        {
          from: fromKey('control + h'),
          to: [
            sendKey('delete_or_backspace'),
            unsetMode(),
          ],
        },

        // Tab (C-i)
        {
          from: fromKey('control + i'),
          to: [ sendKey('tab') ],
        },

        // Esc (C-[)
        {
          conditions: [{
            keyboard_types: [ 'ansi', 'iso' ],
            type: 'keyboard_type_if'
          }],
          from: fromKey('control + open_bracket'),
          to: [ sendKey('escape') ],
        },
        {
          conditions: [{
            keyboard_types: [ 'jis' ],
            type: 'keyboard_type_if'
          }],
          from: fromKey('control + close_bracket'),
          to: [ sendKey('escape') ],
        },
        {
          from: fromKey('control + g'),
          to: [ sendKey('escape') ],
        },

        // Cursor Moves (C-n, C-p, C-f, C-b, M-f, M-b, C-a, C-e)
        {
          from: fromKey('control + b'),
          to: [ sendKey('left_arrow') ],
        },
        {
          from: fromKey('command + b'),
          to: [ sendKey('option + left_arrow') ],
        },
        {
          from: fromKey('control + f'),
          to: [ sendKey('right_arrow') ],
        },
        {
          from: fromKey('command + f'),
          to: [ sendKey('option + right_arrow') ],
        },
        {
          from: fromKey('control + n'),
          to: [ sendKey('down_arrow') ],
        },
        {
          from: fromKey('control + p'),
          to: [ sendKey('up_arrow') ],
        },
        {
          from: fromKey('control + a'),
          to: [ sendKey('command + left_arrow') ],
        },
        {
          from: fromKey('control + e'),
          to: [ sendKey('command + right_arrow') ],
        },

        // Page Up/Down
        {
          from: fromKey('control + v'),
          to: [ sendKey('page_down') ],
        },
        {
          from: fromKey('command + v'),
          to: [ sendKey('page_up') ],
        },

        // Cut/Copy/Paste
        {
          from: fromKey('control + w'),
          to: [
            sendKey('command + x'),
            unsetMode(),
          ],
        },
        {
          from: fromKey('command + w'),
          to: [
            sendKey('command + c'),
            unsetMode(),
          ],
        },
        {
          from: fromKey('control + y'),
          to: [ sendKey('command + v') ],
        },

        // Kill Line
        {
          from: fromKey('control + k'),
          to: [
            sendKey('shift + command + right_arrow'),
            sendKey('command + x'),
            unsetMode(),
          ],
        },

        // Search
        {
          from: fromKey('control + s'),
          to: [ sendKey('command + f') ],
        },

        // Undo
        {
          from: fromKey('control + slash'),
          to: [ sendKey('command + z') ],
        },

        // M-k
        {
          from: fromKey('command + k'),
          to: [ sendKey('command + w') ],
        }
      ]),
    },
  ]
};

function conditionsGroup(conditions, manipulators) {
  return manipulators.map((m) => {
    m.type = 'basic';
    m.conditions = (m.conditions || []).concat(conditions);
    return m;
  });
}

// Use in "from"
function fromKey(str) {
  const binds = str
        .split('+')
        .map(s => s.trim().toLowerCase());
  const keyCode = binds.pop();
  const modifiers = binds.reduce((mod, keyString) => {
    const isOptional = keyString.endsWith('?');
    const key = keyString.replace(/\?$/, '');
    if (isOptional) {
      mod.optional = (mod.optional || []).concat(key);
    } else {
      mod.mandatory = (mod.mandatory || []).concat(key);
    }
    return mod;
  }, {});
  return {
    key_code: keyCode,
    modifiers,
  };
}

// use in "to"
function sendKey(str) {
  const binds = str
        .split('+')
        .map(s => s.trim().toLowerCase());
  const keyCode = binds.pop();
  return {
    key_code: keyCode,
    modifiers: binds,
  };
}

function setVariable(name, value) {
  return { set_variable: { name, value } };
}

function setMode(modeName) {
  return setVariable('mode', modeName);
}

function unsetMode() {
  return setVariable('mode', Modes.Unset);
}

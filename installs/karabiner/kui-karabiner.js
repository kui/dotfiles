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
]

const baseConditions = [{
  type: 'frontmost_application_unless',
  bundle_identifiers: baseExcludedApps,
}];

const markModeConditions = [
  {
    type: 'variable_if',
    name: 'mark_mode',
    value: 1,
  },
  ...baseConditions
];
const cxModeCondition = {
    type: 'variable_if',
    name: 'c_x_mode',
    value: 1,
};
const cxModeBaseConditions = [
  cxModeCondition,
  ...baseConditions
];

const webBrowserConditions = [
  {
    type: 'frontmost_application_if',
    bundle_identifiers: [
      '^com\\.google\\.Chrome$',
    ],
  }
]

const ideConditions = [
  {
    type: 'frontmost_application_if',
    bundle_identifiers: [
      '^com\\.jetbrains\\.intellij$',
    ]
  },
];

const terminalConditions = [
  {
    type: 'frontmost_application_if',
    bundle_identifiers: [
      '^com\\.apple\\.Terminal$',
      '^com\\.googlecode\\.iterm2$',
    ]
  }
];

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

// console.log(JSON.stringify(fromKey('option+any?+f'), 0, 2));

module.exports = {
  title: 'kui\'s Bindings',
  rules: [
    ////////////////////////////////////////////////////////////
    {
      description: 'Terminal',
      manipulators: [
        {
          type: 'basic',
          conditions: terminalConditions,
          from: fromKey('any? + left_option'),
          to: [ sendKey('left_command') ],
        },
        {
          type: 'basic',
          conditions: terminalConditions,
          from: fromKey('any? + left_command'),
          to: [ sendKey('left_option') ],
        },
        {
          type: 'basic',
          conditions: terminalConditions,
          from: fromKey('option + shift? + tab'),
          to: [ sendKey('command + tab') ],
        },
      ],
    },

    /////////////////////////////////////////////////////////////
    {
      description: 'Web Browser',
      manipulators: [
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('control + shift? + t'),
          to: [ sendKey('command + t') ],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('control + shift? + l'),
          to: [ sendKey('command + l') ],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('control + shift? + r'),
          to: [ sendKey('command + r') ],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('command + shift + period'),
          to: [ sendKey('command + down_arrow') ],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('command + shift + comma'),
          to: [ sendKey('command + up_arrow') ],
        },
      ]
    },

    /////////////////////////////////////////////////////////////
    {
      description: 'IDE',
      manipulators: [
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('command + x'),
          to: [ sendKey('command + 3') ],
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('command + i'),
          to: [ sendKey('command + e') ],
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('control + o'),
          to: [ sendKey('f3') ],
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('control + s'),
          to: [ sendKey('command + j') ],
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('command + slash'),
          to: [ sendKey('control + spacebar') ],
        },
        {
          type: 'basic',
          conditions: [
            ...ideConditions,
            cxModeCondition
          ],
          from: fromKey('control + f'),
          to: [
            sendKey('command + shift + r'),
            setVariable('c_x_mode', 0),
          ],
        },
      ]
    },

    /////////////////////////////////////////////////
    {
      description: 'Like Emacs',
      manipulators: [
        // Mark Set - Base Binding
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + spacebar'),
          to: [ setVariable('mark_mode', 1) ],
        },

        // Mark Set - Cursor Move
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + b'),
          to: [ sendKey('shift + left_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('command + b'),
          to: [ sendKey('shift + option + left_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + f'),
          to: [ sendKey('shift + right_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('command + f'),
          to: [ sendKey('shift + option + right_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + n'),
          to: [ sendKey('shift + down_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + p'),
          to: [ sendKey('shift + up_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + v'),
          to: [ sendKey('shift + page_down') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('command + v'),
          to: [ sendKey('shift + page_up') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + a'),
          to: [ sendKey('shift + command + left_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + e'),
          to: [ sendKey('shift + command + right_arrow') ],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + g'),
          to: [
            {
              set_variable: {
                name: 'mark_mode',
                value: 0
              },
            },
          ],
        },

        // C-x Prefix Bindings
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + x'),
          to: [{
            set_variable: {
              name: 'c_x_mode',
              value: 1
            }
          }],
        },
        {
          type: 'basic',
          conditions: cxModeBaseConditions,
          from: fromKey('h'),
          to: [
            sendKey('command + a'),
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0
              }
            },
          ],
        },
        {
          type: 'basic',
          conditions: cxModeBaseConditions,
          from: fromKey('control + s'),
          to: [
            sendKey('command + s'),
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0
              }
            },
          ],
        },
        {
          type: 'basic',
          conditions: cxModeBaseConditions,
          from: fromKey('control + f'),
          to: [
            sendKey('command + o'),
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0
              }
            },
          ],
        },
        {
          type: 'basic',
          conditions: cxModeBaseConditions,
          from: fromKey('control + c'),
          to: [
            sendKey('command + q'),
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0
              }
            },
          ],
        },
        {
          type: 'basic',
          conditions: cxModeBaseConditions,
          from: { any: 'key_code' },
          to: [
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0,
              }
            },
          ],
        },

        {
          type: 'basic',
          conditions: cxModeBaseConditions,
          from: fromKey('control + g'),
          to: [
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0
              }
            },
          ],
        },

        // Delete (C-d, C-h)
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + d'),
          to: [ sendKey('delete_forward') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + h'),
          to: [ sendKey('delete_or_backspace') ],
        },

        // Tab (C-i)
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + i'),
          to: [ sendKey('tab') ],
        },

        // Esc (C-[)
        {
          type: 'basic',
          conditions: [
            {
              keyboard_types: [ 'ansi', 'iso' ],
              type: 'keyboard_type_if'
            },
            ...baseConditions
          ],
          from: fromKey('control + open_bracket'),
          to: [ sendKey('escape') ],
        },
        {
          type: 'basic',
          conditions: [
            {
              keyboard_types: [ 'jis' ],
              type: 'keyboard_type_if'
            },
            ...baseConditions
          ],
          from: fromKey('control + close_bracket'),
          to: [ sendKey('escape') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + g'),
          to: [ sendKey('escape') ],
        },

        // Cursor Moves (C-n, C-p, C-f, C-b, M-f, M-b, C-a, C-e)
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + b'),
          to: [ sendKey('left_arrow') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + b'),
          to: [ sendKey('option + left_arrow') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + f'),
          to: [ sendKey('right_arrow') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + f'),
          to: [ sendKey('option + right_arrow') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + n'),
          to: [ sendKey('down_arrow') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + p'),
          to: [ sendKey('up_arrow') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + a'),
          to: [ sendKey('command + left_arrow') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + e'),
          to: [ sendKey('command + right_arrow') ],
        },

        // Page Up/Down
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + v'),
          to: [ sendKey('page_down') ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + v'),
          to: [ sendKey('page_up') ],
        },

        // Cut/Copy/Paste
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + w'),
          to: [
            sendKey('command + x'),
            {
              set_variable: {
                name: 'mark_mode',
                value: 0
              }
            }
          ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + w'),
          to: [
            sendKey('command + c'),
            {
              set_variable: {
                name: 'mark_mode',
                value: 0
              }
            }
          ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + y'),
          to: [ sendKey('command + v') ],
        },

        // Kill Line
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + k'),
          to: [
            sendKey('shift + command + right_arrow'),
            sendKey('command + x'),
          ],
        },

        // Search
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + s'),
          to: [ sendKey('command + f') ],
        },

        // Undo
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + slash'),
          to: [ sendKey('command + z') ],
        },

        // M-k
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + k'),
          to: [ sendKey('command + w') ],
        }
      ]
    },
  ]
};

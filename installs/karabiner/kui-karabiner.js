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
    modifiers
  };
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
          to: [
            {
              key_code: 'left_command'
            },
          ],
        },
        {
          type: 'basic',
          conditions: terminalConditions,
          from: fromKey('any? + left_command'),
          to: [
            {
              key_code: 'left_option'
            },
          ],
        },
        {
          type: 'basic',
          conditions: terminalConditions,
          from: fromKey('option + shift? + tab'),
          to: [
            {
              key_code: 'tab',
              modifiers: [ 'command' ],
            },
          ],
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
          to: [{
            key_code: 't',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('control + shift? + l'),
          to: [{
            key_code: 'l',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('control + shift? + r'),
          to: [{
            key_code: 'r',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('command + shift + period'),
          to: [{
            key_code: 'down_arrow',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: fromKey('command + shift + comma'),
          to: [{
            key_code: 'up_arrow',
            modifiers: [ 'command' ],
          }],
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
          to: [{
            key_code: '3',
            modifiers: [ 'command' ],
          }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('command + i'),
          to: [{
            key_code: 'e',
            modifiers: [ 'command' ],
          }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('control + o'),
          to: [{ key_code: 'f3' }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('control + s'),
          to: [{
            key_code: 'j',
            modifiers: [ 'command' ],
          }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: fromKey('command + slash'),
          to: [{
            key_code: 'spacebar',
            modifiers: [ 'control' ],
          }]
        },
        {
          type: 'basic',
          conditions: [
            ...ideConditions,
            cxModeCondition
          ],
          from: fromKey('control + f'),
          to: [
            {
              key_code: 'r',
              modifiers: [ 'shift', 'command' ],
            },
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0
              }
            },
          ]
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
          to: [{
            set_variable: {
              name: 'mark_mode',
              value: 1
            }
          }],
        },

        // Mark Set - Cursor Move
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + b'),
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('command + b'),
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'option', 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + f'),
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('command + f'),
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'option', 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + n'),
          to: [{
            key_code: 'down_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + p'),
          to: [{
            key_code: 'up_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + v'),
          to: [{
            key_code: 'page_down',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('command + v'),
          to: [{
            key_code: 'page_up',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + a'),
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'command', 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: fromKey('control + e'),
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'command', 'shift' ]
          }],
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
            {
              key_code: 'a',
              modifiers: [ 'command' ],
            },
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
            {
              key_code: 's',
              modifiers: [ 'command' ],
            },
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
            {
              key_code: 'o',
              modifiers: [ 'command' ],
            },
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
            {
              key_code: 'q',
              modifiers: [ 'command' ],
            },
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
          to: [ { key_code: 'delete_forward' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + h'),
          to: [ { key_code: 'delete_or_backspace' } ],
        },

        // Tab (C-i)
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + i'),
          to: [ { key_code: 'tab' } ],
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
          to: [ { key_code: 'escape' } ],
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
          to: [ { key_code: 'escape' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + g'),
          to: [
            { key_code: 'escape' },
          ],
        },

        // Cursor Moves (C-n, C-p, C-f, C-b, M-f, M-b, C-a, C-e)
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + b'),
          to: [ { key_code: 'left_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + b'),
          to: [ {
            key_code: 'left_arrow',
            modifiers: [ 'option' ]
          } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + f'),
          to: [ { key_code: 'right_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + f'),
          to: [ {
            key_code: 'right_arrow',
            modifiers: [ 'option' ]
          } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + n'),
          to: [ { key_code: 'down_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + p'),
          to: [ { key_code: 'up_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + a'),
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'command' ]
          }],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + e'),
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'command' ]
          }],
        },

        // Page Up/Down
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + v'),
          to: [ { key_code: 'page_down' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + v'),
          to: [ { key_code: 'page_up' } ],
        },

        // Cut/Copy/Paste
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + w'),
          to: [
            {
              key_code: 'x',
              modifiers: [ 'command' ],
            },
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
            {
              key_code: 'c',
              modifiers: [ 'command' ],
            },
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
          to: [{
            key_code: 'v',
            modifiers: [ 'command' ],
          }],
        },

        // Kill Line
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + k'),
          to: [
            {
              key_code: 'right_arrow',
              modifiers: [
                'shift',
                'command'
              ]
            },
            {
              key_code: 'x',
              modifiers: [ 'command']
            },
          ],
        },

        // Search
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + s'),
          to: [{
            key_code: 'f',
            modifiers: [ 'command' ],
          }],
        },

        // Undo
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('control + slash'),
          to: [{
            key_code: 'z',
            modifiers: [ 'command' ],
          }],
        },

        // M-k
        {
          type: 'basic',
          conditions: baseConditions,
          from: fromKey('command + k'),
          to: [{
            key_code: 'w',
            modifiers: [ 'command' ],
          }],
        }
      ]
    },
  ]
};

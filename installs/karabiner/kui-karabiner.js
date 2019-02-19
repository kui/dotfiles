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
          from: {
            key_code: 'left_option',
            modifiers: {
              optional: [ 'any' ],
            },
          },
          to: [
            {
              key_code: 'left_command'
            },
          ],
        },
        {
          type: 'basic',
          conditions: terminalConditions,
          from: {
            key_code: 'left_command',
            modifiers: {
              optional: [ 'any' ],
            },
          },
          to: [
            {
              key_code: 'left_option'
            },
          ],
        },
        {
          type: 'basic',
          conditions: terminalConditions,
          from: {
            key_code: 'tab',
            modifiers: {
              mandatory: [ 'option' ],
              optional: [ 'shift' ],
            },
          },
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
          from: {
            key_code: 't',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'shift' ],
            }
          },
          to: [{
            key_code: 't',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: {
            key_code: 'l',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'shift' ],
            }
          },
          to: [{
            key_code: 'l',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: {
            key_code: 'r',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'shift' ],
            }
          },
          to: [{
            key_code: 'r',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: {
            key_code: 'period',
            modifiers: {
              mandatory: [ 'command', 'shift' ],
            }
          },
          to: [{
            key_code: 'down_arrow',
            modifiers: [ 'command' ],
          }],
        },
        {
          type: 'basic',
          conditions: webBrowserConditions,
          from: {
            key_code: 'comma',
            modifiers: {
              mandatory: [ 'command', 'shift' ],
            }
          },
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
          from: {
            key_code: 'x',
            modifiers: {
              mandatory: [ 'command' ],
              optional: [ 'any' ]
            },
          },
          to: [{
            key_code: '3',
            modifiers: [ 'command' ],
          }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: {
            key_code: 'i',
            modifiers: {
              mandatory: [ 'command' ],
              optional: [ 'any' ]
            },
          },
          to: [{
            key_code: 'e',
            modifiers: [ 'command' ],
          }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: {
            key_code: 'o',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'any' ]
            },
          },
          to: [{ key_code: 'f3' }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: {
            key_code: 's',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'any' ]
            },
          },
          to: [{
            key_code: 'j',
            modifiers: [ 'command' ],
          }]
        },
        {
          type: 'basic',
          conditions: ideConditions,
          from: {
            key_code: 'slash',
            modifiers: {
              mandatory: [ 'command' ],
              optional: [ 'any' ]
            },
          },
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
          from: {
            key_code: 'f',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'any' ]
            },
          },
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
          from: {
            key_code: 'spacebar',
            modifiers: { mandatory: [ 'control' ] }
          },
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
          from: {
            key_code: 'b',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [
                'caps_lock',
                'option'
              ]
            }
          },
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'b',
            modifiers: {
              mandatory: [ 'command' ],
              optional: [
                'caps_lock',
                'option'
              ]
            }
          },
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'option', 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'f',
            modifiers: {
              mandatory: [
                'control'
              ],
              optional: [
                'caps_lock',
                'option'
              ]
            }
          },
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'f',
            modifiers: {
              mandatory: [
                'command'
              ],
              optional: [
                'caps_lock',
              ]
            }
          },
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'option', 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'n',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [
                'caps_lock',
                'option'
              ]
            }
          },
          to: [{
            key_code: 'down_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'p',
            modifiers: {
              'mandatory': [ 'control' ],
              'optional': [
                'caps_lock',
                'option'
              ]
            }
          },
          to: [{
            key_code: 'up_arrow',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'v',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'caps_lock' ]
            }
          },
          to: [{
            key_code: 'page_down',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'v',
            modifiers: {
              mandatory: [ 'command' ],
              optional: [ 'caps_lock' ]
            }
          },
          to: [{
            key_code: 'page_up',
            modifiers: [ 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'a',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'caps_lock' ]
            }
          },
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'command', 'shift' ]
          }],
        },
        {
          type: 'basic',
          conditions: markModeConditions,
          from: {
            key_code: 'e',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'caps_lock' ]
            }
          },
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'command', 'shift' ]
          }],
        },

        // C-x Prefix Bindings
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'x',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
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
          from: {
            key_code: 'h',
          },
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
          from: {
            key_code: 's',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
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
          from: {
            key_code: 'f',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
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
          from: {
            key_code: 'c',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
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
          from: {
            any: 'key_code',
          },
          to: [
            {
              set_variable: {
                name: 'c_x_mode',
                value: 0,
              }
            },
          ],
        },

        // Cancel Modes
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'g',
            modifiers: { mandatory: [ 'control' ] }
          },
          to: [
            { key_code: 'escape' },
            {
              set_variable: {
                name: 'mark_mode',
                value: 0
              }
            },
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
          from: {
            key_code: 'd',
            modifiers: {
              mandatory: [ 'control' ],
              'optional': [ 'caps_lock', 'option' ]
            }
          },
          to: [ { key_code: 'delete_forward' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'h',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'caps_lock', 'option' ]
            }
          },
          to: [ { key_code: 'delete_or_backspace' } ],
        },

        // Tab (C-i)
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'i',
            modifiers: {
              mandatory: [ 'control' ],
              'optional': [ 'caps_lock', 'shift' ]
            }
          },
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
          from: {
            key_code: 'open_bracket',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'caps_lock' ]
            }
          },
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
          from: {
            key_code: 'close_bracket',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'caps_lock' ]
            }},
          to: [ { key_code: 'escape' } ],
        },

        // Cursor Moves (C-n, C-p, C-f, C-b, M-f, M-b, C-a, C-e)
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'b',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [
                'caps_lock',
                'shift',
                'option'
              ]
            }
          },
          to: [ { key_code: 'left_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'b',
            modifiers: {
              mandatory: [ 'command' ],
              optional: [
                'caps_lock',
                'shift',
                'option'
              ]
            }
          },
          to: [ {
            key_code: 'left_arrow',
            modifiers: [ 'option' ]
          } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'f',
            modifiers: {
              mandatory: [
                'control'
              ],
              optional: [
                'caps_lock',
                'shift',
                'option'
              ]
            }
          },
          to: [ { key_code: 'right_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'f',
            modifiers: {
              mandatory: [
                'command'
              ],
              optional: [
                'caps_lock',
                'shift',
              ]
            }
          },
          to: [ {
            key_code: 'right_arrow',
            modifiers: [ 'option' ]
          } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'n',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [
                'caps_lock',
                'shift',
                'option'
              ]
            }
          },
          to: [ { key_code: 'down_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'p',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [
                'caps_lock',
                'shift',
                'option'
              ]
            }
          },
          to: [ { key_code: 'up_arrow' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'a',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [
                'caps_lock',
                'shift',
                'option'
              ]
            }
          },
          to: [{
            key_code: 'left_arrow',
            modifiers: [ 'command' ]
          }],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'e',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [
                'caps_lock',
                'shift',
                'option'
              ]
            }
          },
          to: [{
            key_code: 'right_arrow',
            modifiers: [ 'command' ]
          }],
        },

        // Page Up/Down
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'v',
            modifiers: {
              mandatory: [ 'control' ],
              optional: [ 'caps_lock', 'shift' ]
            }
          },
          to: [ { key_code: 'page_down' } ],
        },
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'v',
            modifiers: {
              mandatory: [ 'command' ],
              optional: [ 'caps_lock', 'shift' ]
            }
          },
          to: [ { key_code: 'page_up' } ],
        },

        // Cut/Copy/Paste
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'w',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
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
          from: {
            key_code: 'w',
            modifiers: {
              mandatory: [ 'command' ],
            }
          },
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
          from: {
            key_code: 'y',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
          to: [{
            key_code: 'v',
            modifiers: [ 'command' ],
          }],
        },

        // Kill Line
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'k',
            modifiers: {
              mandatory: [ 'control' ],
            },
          },
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
          from: {
            key_code: 's',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
          to: [{
            key_code: 'f',
            modifiers: [ 'command' ],
          }],
        },

        // Undo
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'slash',
            modifiers: {
              mandatory: [ 'control' ],
            }
          },
          to: [{
            key_code: 'z',
            modifiers: [ 'command' ],
          }],
        },

        // M-k
        {
          type: 'basic',
          conditions: baseConditions,
          from: {
            key_code: 'k',
            modifiers: {
              mandatory: [ 'command' ],
            }
          },
          to: [{
            key_code: 'w',
            modifiers: [ 'command' ],
          }],
        }
      ]
    },
  ]
};

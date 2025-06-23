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
      description: 'Japanese Input Mode',
      manipulators: conditionsGroup([
      ], [
        bindKey('shift + left_command + j', 'japanese_kana'),
        bindKey('left_command + j', 'japanese_eisuu')
      ])
    },

    ////////////////////////////////////////////////////////////
    {
      description: 'Terminal',
      manipulators: conditionsGroup([
        {
          type: 'frontmost_application_if',
          bundle_identifiers: terminals
        }
      ], [
        bindKey('left_option', 'left_command'),
        bindKey('left_command', 'left_option'),
        bindKey('option + shift? + tab', 'command + tab'),
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
        bindKey('control + shift? + t', 'command + t'),
        bindKey('control + shift? + l', 'command + l'),
        bindKey('control + shift? + r', 'command + r'),
        bindKey('command + shift + period', 'command + down_arrow'),
        bindKey('command + shift + comma', 'command + up_arrow'),
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
        bindKey('command + x', 'command + 3'),
        bindKey('command + i', 'command + e'),
        bindKey('control + o', 'f3'),
        bindKey('control + s', 'command + j'),
        bindKey('command + slash', 'control + spacebar'),
        Object.assign(
          bindKey('control + f', [
            'command + shift + r',
            unsetMode()
          ]),
          { conditions: [{
            type: 'variable_if',
            name: 'mode',
            value: Modes.Cx,
          }]},
        ),
        bindKey('command + shift + period', 'command + down_arrow'),
        bindKey('command + shift + comma', 'command + up_arrow'),
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
        bindKey('control + spacebar', setMode(Modes.Mark)),
        ...conditionsGroup(
          [{
            type: 'variable_if',
            name: 'mode',
            value: Modes.Mark,
          }],
          [
            bindKey('control + b', 'shift + left_arrow'),
            bindKey('command + b', 'shift + option + left_arrow'),
            bindKey('control + f', 'shift + right_arrow'),
            bindKey('command + f', 'shift + option + right_arrow'),
            bindKey('control + n', 'shift + down_arrow'),
            bindKey('control + p', 'shift + up_arrow'),
            bindKey('control + v', 'shift + page_down'),
            bindKey('command + v', 'shift + page_up'),
            bindKey('control + a', 'shift + command + left_arrow'),
            bindKey('control + e', 'shift + command + right_arrow'),
            bindKey('control + g', unsetMode()),

            bindKey('control + d', [
              'delete_forward',
              unsetMode(),
            ]),
            bindKey('control + h', [
              'delete_or_backspace',
              unsetMode(),
            ]),
          ]
        ),

        // C-x Prefix Bindings
        bindKey('control + x', setMode(Modes.Cx)),
        ...conditionsGroup(
          [{
            type: 'variable_if',
            name: 'mode',
            value: Modes.Cx,
          }],
          [
            bindKey('h', [
              sendKey('command + a'),
              unsetMode(),
            ]),
            bindKey('control + s', [
              'command + s',
              unsetMode(),
            ]),
            bindKey('control + f', [
              'command + o',
              unsetMode(),
            ]),
            bindKey('control + c', [
              'command + q',
              unsetMode(),
            ]),
            bindKey('control + g', unsetMode()),

            // Escape C-x mode if unknown key
            // TODO Fix that this expression escapes mode if modifier key was pressed.
            {
              type: 'basic',
              from: { any: 'key_code' },
              to: [ unsetMode() ],
            },
          ]
        ),

        // Delete (C-d, C-h)
        // Note: separate definitions from markset bindings
        // Because the key repeat is disabled if the bindings is not just key mappings
        bindKey('control + d', 'delete_forward'),
        bindKey('control + h', 'delete_or_backspace'),

        // Tab (C-i)
        bindKey('control + i', 'tab'),

        // Esc (C-[)
        Object.assign(
          bindKey('control + open_bracket', 'escape'),
          { conditions: [{
            keyboard_types: [ 'ansi', 'iso' ],
            type: 'keyboard_type_if'
          }]}
        ),
        Object.assign(
          bindKey('control + close_bracket', 'escape'),
          { conditions: [{
            keyboard_types: [ 'jis' ],
            type: 'keyboard_type_if'
          }],}
        ),
        bindKey('control + g', 'escape'),

        // Cursor Moves (C-n, C-p, C-f, C-b, M-f, M-b, C-a, C-e)
        bindKey('control + b', 'left_arrow'),
        bindKey('command + b', 'option + left_arrow'),
        bindKey('control + f', 'right_arrow'),
        bindKey('command + f', 'option + right_arrow'),
        bindKey('control + n', 'down_arrow'),
        bindKey('control + p', 'up_arrow'),
        bindKey('control + a', 'command + left_arrow'),
        bindKey('control + e', 'command + right_arrow'),

        // Page Up/Down
        bindKey('control + v', 'page_down'),
        bindKey('command + v', 'page_up'),

        // Cut/Copy/Paste
        bindKey('control + w', [
          'command + x',
          unsetMode(),
        ]),
        bindKey('command + w', [
          'command + c',
          unsetMode(),
        ]),
        bindKey('control + y', 'command + v'),
        bindKey('control + k', [
          'shift + command + right_arrow',
          'command + x',
          unsetMode(),
        ]),

        bindKey('control + s', 'command + f'),
        bindKey('control + slash', 'command + z'),
        bindKey('command + k', 'command + w'),
      ]),
    },
  ]
};

function conditionsGroup(conditions, manipulators) {
  return manipulators.map((m) => {
    m.conditions = (m.conditions || []).concat(conditions);
    return m;
  });
}

function bindKey(fromKeyStr, secondArg) {
  const events = (() => {
    const t = typeString(secondArg);

    if (t === 'String') {
      return [ sendKey(secondArg) ];
    }

    if (t === 'Array') {
      return secondArg.map((a) => {
        if (typeString(a) === 'String') {
          return sendKey(a);
        }
        return a;
      });
    }

    return [ secondArg ];
  })();

  return {
    type: 'basic',
    from: fromKey(fromKeyStr),
    to: events,
  };
}

function typeString(o) {
  return Object
    .prototype
    .toString
    .apply(o)
    .replace(/^\[object (.*)\]$/, (_, m) => m);
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

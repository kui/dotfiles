local IGNORED_APP_IDS = {
   "org.gnu.Emacs",
   "com.apple.Terminal",
}

local function sendKey(modifiers, key)
   modifiers = modifiers or {}
   return function(e)
      hs.eventtap.keyStroke(modifiers, key, 0)
   end
end

local function hotkey(modifiers, key, fn)
   return hs.hotkey.new(modifiers, key, fn, nil, fn)
end

local function contains(arr, value)
   for _, v in pairs(arr) do
      if v == value then
         return true
      end
   end
   return false
end

local function isIgnoredId(id)
   return contains(IGNORED_APP_IDS, id)
end

function enableHotkeys(hotkeys)
   return function()
      for _, v in pairs(hotkeys) do
         v:enable()
      end
   end
end

function disableHotkeys(hotkeys)
   return function()
      for _, v in pairs(hotkeys) do
         v:disable()
      end
   end
end

windowFilter = hs.window.filter

function bindHotkeys(fn, hotkeys)
   windowFilter.new(fn)
      :subscribe(windowFilter.windowFocused, enableHotkeys(hotkeys))
      :subscribe(windowFilter.windowUnfocused, disableHotkeys(hotkeys))
end

hs.application.watcher.new(function(name, event, app)
      if event ~= hs.application.watcher.activated then
         return
      end
      print("AppName:  "..name)
      print("BundleID: "..app:bundleID())
end):start()

globalHotkeys = {
   -- cursours
   hotkey({'ctrl'}, 'f', sendKey(nil, 'right')),
   hotkey({'ctrl'}, 'b', sendKey(nil, 'left')),
   hotkey({'ctrl'}, 'n', sendKey(nil, 'down')),
   hotkey({'ctrl'}, 'p', sendKey(nil, 'up')),
   hotkey({'alt'}, 'f', sendKey({'alt'}, 'right')),
   hotkey({'alt'}, 'b', sendKey({'alt'}, 'left')),

   -- page
   hotkey({'ctrl'}, 'v', sendKey(nil, 'pagedown')),
   hotkey({'alt'}, 'v', sendKey(nil, 'pageup')),

   -- edit
   hotkey({'ctrl'}, 'w', sendKey({'cmd'}, 'x')),
   hotkey({'alt'}, 'w', sendKey({'cmd'}, 'c')),
   hotkey({'ctrl'}, 'y', sendKey({'cmd'}, 'v')),
   hotkey({'ctrl'}, 'h', sendKey(nil, 'delete')),
   hotkey({'ctrl'}, 'i', sendKey(nil, 'tab')),
   hotkey({'ctrl'}, 'd', sendKey({'fn'}, 'delete')),

   -- operations
   hotkey({'cmd'}, 'k', sendKey({'cmd'}, 'w')),
   hotkey({'ctrl'}, 's', sendKey({'cmd'}, 'f')),
   hotkey({'ctrl'}, '/', sendKey({'cmd'}, 'z')),
   hotkey({'ctrl'}, 'g', sendKey(nil, 'escape')),

   -- hotkey({'cmd', 'shift'}, ',', sendKey('home'))
   -- hotkey({'cmd', 'shift'}, '.', sendKey('end'))
}

bindHotkeys(
   function(w)
      local id = w:application():bundleID()
      local isIgnored = isIgnoredId(id)
      return not isIgnored
   end,
   globalHotkeys)

chromeHotkey = {
   hotkey({'ctrl'}, 'l', sendKey({'cmd'}, 'l')),
   hotkey({'ctrl'}, 't', sendKey({'cmd'}, 't')),
   hotkey({'cmd', 'shift'}, ',', sendKey({'cmd'}, 'up')),
   hotkey({'cmd', 'shift'}, '.', sendKey({'cmd'}, 'down')),
}

bindHotkeys(
   function(w)
      local id = w:application():bundleID()
      return id == "com.google.Chrome"
   end,
   chromeHotkey)

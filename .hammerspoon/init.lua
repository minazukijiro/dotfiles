local TapDance = require 'tapdance'

local switcher = hs.window.switcher.new()

function launchOrFocusTerm()
   -- local term = 'Terminal'
   -- local term = 'Alacritty'
   -- local term = 'Kitty'
   local term = 'Emacs'
   local cur = hs.application.frontmostApplication()

   if cur:name() == term then
      switcher:previous()
   else
      hs.application.launchOrFocus(term)
   end
end

local focusterm = TapDance.new('alt', 2, 200, launchOrFocusTerm)
focusterm.watcher:start()

-- function launchOrFocusEmacs()
--    hs.application.launchOrFocus('Emacs')
-- end

-- local focusemacs = TapDance.new('ctrl', 2, 200, launchOrFocusEmacs)
-- focusemacs.watcher:start()

--

local IM = require 'im'

local im = IM.new({
      Emacs = 'en',
      Kitty = 'en',
      Slack = 'ja'
})

im.watcher:start()

PaperWM = hs.loadSpoon('PaperWM')

PaperWM:bindHotkeys(PaperWM.default_hotkeys)

hs.hotkey.bind({"alt", "cmd"}, "h", PaperWM.actions.focus_left)
hs.hotkey.bind({"alt", "cmd"}, "j", PaperWM.actions.focus_down)
hs.hotkey.bind({"alt", "cmd"}, "k", PaperWM.actions.focus_up)
hs.hotkey.bind({"alt", "cmd"}, "l", PaperWM.actions.focus_right)

hs.hotkey.bind({"ctrl", "alt", "cmd"}, "h", PaperWM.actions.swap_left)
hs.hotkey.bind({"ctrl", "alt", "cmd"}, "j", PaperWM.actions.swap_down)
hs.hotkey.bind({"ctrl", "alt", "cmd"}, "k", PaperWM.actions.swap_up)
hs.hotkey.bind({"ctrl", "alt", "cmd"}, "l", PaperWM.actions.swap_right)

PaperWM:start()

--

local function keyCode(key, mods, callback)
   mods = mods or {}
   callback = callback or function() end
   return function()
      hs.eventtap.event.newKeyEvent(mods, string.lower(key), true):post()
    hs.timer.usleep(1000)
    hs.eventtap.event.newKeyEvent(mods, string.lower(key), false):post()

    callback()
  end
end

local function remapKey(mod, key, code)
   hs.hotkey.bind(mod, key, code, nil, code)
end

remapKey({'cmd'}, 'h', keyCode('left'))

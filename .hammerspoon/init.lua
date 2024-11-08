local TapDance = require 'tapdance'

function launchOrFocusTerm()
   -- local term = 'Terminal'
   -- local term = 'Alacritty'
   local term = 'Kitty'
   hs.application.launchOrFocus(term)
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

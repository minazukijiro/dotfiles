local tapdance = require 'tapdance'

function launchOrFocusKitty()
   hs.application.launchOrFocus('Alacritty')
   -- hs.application.launchOrFocus('Kitty')
   -- hs.application.launchOrFocus('Terminal')
end

focusterm = tapdance.new('alt', 2, 200, launchOrFocusKitty)
focusterm.watcher:start()

--

-- function moveToNextScreen()
--    local app = hs.window.focusedWindow()
--    app:moveToScreen(app:screen():next())
-- end

-- movenextscr = tapdance.new('', 2, 200, moveToNextScreen)
-- movenextscr.watcher:start()

--

im = require 'im'
im.applications = {
   Alacritty = 'en',
   kitty = 'en',
   slack = 'ja',
}

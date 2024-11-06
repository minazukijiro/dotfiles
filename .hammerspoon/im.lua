-- local IM = require 'im'

-- local im = IM.new({
--       'Emacs' = 'en',
--       'Kitty' = 'en',
--       'Slack' = 'ja'
-- })

-- im.watcher:start()

local application  = require('hs.application')
local watcher = application.watcher
local eventtap = require('hs.eventtap')
local keystroke = eventtap.keyStroke

local inputmethod = {}
inputmethod['en'] = 0x66
inputmethod['ja'] = 0x68

local im = {}

im.new = function(settings)
   local obj = {}

   obj.watcher = watcher.new(function(appName, eventType, appObject)
         for k, v in pairs(settings) do
            if (appName == k and eventType == watcher.activated) then
               keystroke({}, inputmethod[v], 0)
               break
            end
         end
   end)

   return obj
end

return im

-- module.applicationWatcher = watcher.new(function(appName, eventType, appObject)
--       for k,v in pairs(module.applications) do
--          if (appName == k and eventType == watcher.activated) then
--             keystroke({}, im[v], 0)
--             break
--          end
--       end
-- end):start()

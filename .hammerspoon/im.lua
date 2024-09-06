-- im = require 'im'
-- im.applications = {
--    kitty = 'en',
--    Slack = 'ja',
-- }

local application  = require('hs.application')
local watcher = application.watcher
local eventtap = require('hs.eventtap')
local keystroke = eventtap.keyStroke

local module = {}
module.applications = {}

local im = {}
im['en'] = 0x66
im['ja'] = 0x68

module.applicationWatcher = watcher.new(function(appName, eventType, appObject)
      for k,v in pairs(module.applications) do
         if (appName == k and eventType == watcher.activated) then
            keystroke({}, im[v], 0)
            break
         end
      end
end):start()

return module

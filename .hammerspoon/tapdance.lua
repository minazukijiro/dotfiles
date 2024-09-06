local timer    = require('hs.timer')
local eventtap = require('hs.eventtap')
local events   = eventtap.event.types
local keycodes = require('hs.keycodes')
local keymap   = keycodes.map

local tapdance = {}

tapdance.new = function(key, taps, tappingTerm, action)
   local obj = {}
   obj.key = key
   obj.taps = taps
   obj.tappingTerm = tappingTerm * 0.001
   obj.action = action

   local count = {}
   count.n = 0

   count.up = function()
      count.n = count.n + 1
   end

   count.reset = function()
      count.n = 0
   end

   obj.watcher = eventtap.new({events.flagsChanged}, function(ev)
         if ev:getType() == events.flagsChanged then
            local flags = ev:getFlags()
            if flags[obj.key] then
               local keycode = ev:getKeyCode()
               if keymap[obj.key] == keycode or keymap['right' .. obj.key] == keycode then
                  timer.doAfter(obj.tappingTerm, function() count.reset() end)
                  count.up()
                  if count.n == obj.taps then
                     obj.action()
                     count.reset()
                  end
               else
                  count.reset()
               end
            end
         end
   end)

   return obj
end

return tapdance

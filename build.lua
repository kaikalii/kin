-- Dependencies
local deps = {'luafilesystem', 'lpeg'}

-- Install dependencies
local luarocks_installed = io.popen('luarocks list'):read('a')
for _, dep in ipairs(deps) do
    if not luarocks_installed:match(dep) then
        os.execute('luarocks install ' .. dep)
    end
end

-- Ammend package path
package.path = package.path .. ';src/?.lua'

-- Run
require 'main'

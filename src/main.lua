require 'utils'
require 'parse'

local file = io.open('test.noot')
local input = file:read('a')

t = {1, 2, 3}
push(t, 4)

print(format(t))

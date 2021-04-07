require 'utils'
require 'lex'

local file = io.open('test.noot')
local input = file:read('a')
local tokens = lex(input)

t = {1, 2, 3}
push(t, 4)

print(format(t))

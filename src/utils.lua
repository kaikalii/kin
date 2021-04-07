function concat(t1, t2)
    for i = 1, #t2 do t1[#t1 + 1] = t2[i] end
    return t1
end

function push(t, x) table.insert(t, #t + 1, x) end

function format(val, pretty, depth)
    depth = depth or 0
    local s = ''
    if type(val) == 'table' then
        s = s .. indent(pretty and depth or 0, '{')
        if pretty then s = s .. '\n' end
        for k, v in pairs(val) do
            if type(k) == 'string' then
                s = s .. format(k, pretty, depth + 1) .. ': '
            end
            s = s .. format(v, pretty, depth + 1) .. (pretty and ',\n' or ', ')
        end
        s = s .. indent(pretty and depth or 0, '}')
    elseif type(val) == 'function' then
        s = s .. indent(pretty and depth or 0, 'function')
    else
        s = s .. indent(pretty and depth or 0, tostring(val))
    end
    return s
end

function indent(n, s)
    local indent = ''
    for _ = 1, n do indent = indent .. '    ' end
    return indent .. (s or '')
end

string.matches = function(s, pat) return #s:match(pat) == #s end

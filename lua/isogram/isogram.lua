return function(s)
	local ht = {}
	for i = 1, #s do
		local c = string.lower(string.sub(s, i, i))
		if c == " " or c == "-" then
			goto continue
		end
		if ht[c] == nil then
			ht[c] = true
		else
			return false
		end
		::continue::
	end
	return true
end

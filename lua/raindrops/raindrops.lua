return function(n)
	local result = ((n % 3 == 0) and "Pling" or "")
		.. ((n % 5 == 0) and "Plang" or "")
		.. ((n % 7 == 0) and "Plong" or "")
	return result ~= "" and result or tostring(n)
end

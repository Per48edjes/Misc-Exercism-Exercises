return function(n)
	if n <= 0 then
		error("Invalid number")
	end
	local ops = 0
	while n > 1 do
		if n % 2 == 0 then
			n = n / 2
		else
			n = 3 * n + 1
		end
		ops = ops + 1
	end
	return ops
end

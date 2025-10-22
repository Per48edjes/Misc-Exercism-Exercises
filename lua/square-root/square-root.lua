local SquareRoot = {}

function SquareRoot.square_root(radicand)
	local num = 0
	while num * num < radicand do
		num = num + 1
	end
	-- mimicking ternary operator
	return (num * num > radicand and num - 1 or num)
end

return SquareRoot

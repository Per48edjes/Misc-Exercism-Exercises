local function square_of_sum(n)
	local gauss = ((n + 1) * n) / 2
	return gauss ^ 2
end

local function sum_of_squares(n)
	return ((2 * n + 1) * n * (n + 1)) / 6
end

local function difference_of_squares(n)
	return square_of_sum(n) - sum_of_squares(n)
end

return { square_of_sum = square_of_sum, sum_of_squares = sum_of_squares, difference_of_squares = difference_of_squares }

function multiples_up_to(limit, factor)
	local multiples = {}
	for i = factor, limit - 1, factor do
		table.insert(multiples, i)
	end
	return multiples
end

function union(t1, t2)
	local set = {}
	for _, v in ipairs(t1) do
		set[v] = true
	end
	for _, v in ipairs(t2) do
		set[v] = true
	end
	local result = {}
	for k, _ in pairs(set) do
		table.insert(result, k)
	end
	return result
end

return function(base_values)
	return {
		to = function(limit)
			local unique_multiples = {}
			for _, factor in ipairs(base_values) do
				local multiples = multiples_up_to(limit, factor)
				unique_multiples = union(unique_multiples, multiples)
			end
			local sum = 0
			for _, value in ipairs(unique_multiples) do
				sum = sum + value
			end
			return sum
		end,
	}
end

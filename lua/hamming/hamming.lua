local Hamming = {}

function Hamming.compute(a, b)
	if #a == #b then
		local d = 0
		for i = 1, #a do
			if a:sub(i, i) ~= b:sub(i, i) then
				d = d + 1
			end
		end
		return d
	else
		error("strands must be of equal length")
	end
end

return Hamming

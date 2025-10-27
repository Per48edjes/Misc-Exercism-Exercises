return function(s)
	local matrix = {}

	-- Data
	local rows = setmetatable({}, { __index = table })
	local cols = {}
	for row in s:gmatch("[^\r\n]+") do
		local current_row = setmetatable({}, { __index = table })
		for elem in row:gmatch("%S+") do
			current_row:insert(tonumber(elem))
		end
		rows:insert(current_row)
	end

	-- Helper
	local function transpose()
		local t_matrix = {}
		for i = 1, #rows[1] do
			t_matrix[i] = {}
			for j = 1, #rows do
				t_matrix[i][j] = rows[j][i]
			end
		end
		return t_matrix
	end

	cols = transpose()

	-- Accessors
	matrix.row = function(idx)
		return rows[idx]
	end

	matrix.column = function(idx)
		return cols[idx]
	end

	return matrix
end

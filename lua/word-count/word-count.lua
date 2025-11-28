--
-- local function count_words(s)
-- 	s = string.lower(s)
-- 	local words = {}
-- 	for w in s:gmatch("[%w']+") do
-- 		w = string.gsub(w, "^'", "")
-- 		w = string.gsub(w, "'$", "")
-- 		if w ~= "" then
-- 			words[w] = (words[w] or 0) + 1
-- 		end
-- 	end
-- 	return words
-- end
--
-- return { count_words = count_words }

local function count_words(s)
	local words = {}
	(s:lower() .. " "):gsub("'(%a+)'", "%1"):gsub("(%w+'?%a*)", function(w)
		words[w] = (words[w] or 0) + 1
	end)
	return words
end
return {
	count_words = count_words,
}

--[[
This file is for use by Benchmarker (https://boatbomber.itch.io/benchmarker)

|WARNING| THIS RUNS IN YOUR REAL ENVIRONMENT. |WARNING|
--]]

return {
	ParameterGenerator = function()
		local lines = {}
		for line = 1, math.random(0, 1000) do
			local size = math.random(0, 1000)
			local line = {}
			table.insert(line, string.rep(" ", math.random(0, 100)))
			for i = 1, size do
				table.insert(line, string.char(math.random(97, 122)))
			end
			table.insert(lines, table.concat(line))
		end

		return table.concat(lines, "\n")
	end,

	Functions = {
		["gmatch"] = function(Profiler, str)
			local minSpaces
			for line in string.gmatch(str, "[^\n]+") do
				local spaces = string.match(line, "^ *") :: string
				if not minSpaces or #spaces < minSpaces then
					minSpaces = #spaces
				end
			end

			return minSpaces or 0
		end,

		["split"] = function(Profiler, str)
			local minSpaces
			for _, line in string.split(str, "\n") do
				local spaces = string.match(line, "^ *") :: string
				if not minSpaces or #spaces < minSpaces then
					minSpaces = #spaces
				end
			end
			return minSpaces or 0
		end,

		["find"] = function(Profiler, str)
			str = "\n" .. str
			local minLength
			local lastIndex = 1
			while true do
				local start, finish = string.find(str, "\n( *)", lastIndex)
				if not (start and finish) then
					break
				end

				local length = finish - start + 1
				if not minLength or length < minLength then
					minLength = length
				end

				lastIndex = finish + 1
			end
			return minLength or 0
		end,

		["gmatch-find"] = function(Profiler, str)
			local minSpaces
			for line in string.gmatch(str, "[^\n]+") do
				local start, finish = string.find(line, "^ +")
				if start and finish then
					local length = finish - start + 1
					if not minSpaces or length < minSpaces then
						minSpaces = length
					end
				end
			end
			return minSpaces or 0
		end,

		["split-find"] = function(Profiler, str)
			local minSpaces
			for _, line in string.split(str, "\n") do
				local start, finish = string.find(line, "^ +")
				if start and finish then
					local length = finish - start + 1
					if not minSpaces or length < minSpaces then
						minSpaces = length
					end
				end
			end
			return minSpaces or 0
		end,
	},
}

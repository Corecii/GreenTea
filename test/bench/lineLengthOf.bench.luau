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
			for i = 1, size do
				table.insert(line, string.char(math.random(97, 122)))
			end
			table.insert(lines, table.concat(line))
		end

		return table.concat(lines, "\n")
	end,

	Functions = {
		["gmatch"] = function(Profiler, str)
			local maxLength = 0
			for line in string.gmatch(str, "[^\n]+") do
				if #line > maxLength then
					maxLength = #line
				end
			end
			return maxLength
		end,

		["split"] = function(Profiler, str)
			local maxLength = 0
			for _, line in string.split(str, "\n") do
				if #line > maxLength then
					maxLength = #line
				end
			end
			return maxLength
		end,

		["find"] = function(Profiler, str)
			local maxLength = 0
			local lastIndex = 1
			while true do
				local start, finish = string.find(str, "[^\n]+", lastIndex)
				if not (start and finish) then
					break
				end

				local length = finish - start + 1
				if length > maxLength then
					maxLength = length
				end

				lastIndex = finish + 1
			end
			return maxLength
		end,
	},
}

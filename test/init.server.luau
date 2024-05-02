local runCLI = require(script.Parent.DevPackages.Jest).runCLI

local status, result = runCLI(script.Parent.GreenTea, {
	verbose = false,
	ci = false,
}, { script }):awaitStatus()

if status == "Rejected" then
	print(result)
end

-- local processServiceExists, ProcessService = pcall(function()
-- 	return game:GetService("ProcessService")
-- end)

-- if status == "Resolved" and result.results.numFailedTestSuites == 0 and result.results.numFailedTests == 0 then
-- 	if processServiceExists then
-- 		ProcessService:ExitAsync(0)
-- 	end
-- end

-- if processServiceExists then
-- 	ProcessService:ExitAsync(1)
-- end

-- return nil
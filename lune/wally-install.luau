local process = require("@lune/process")

local function exec(cmdline: string)
	local pieces = {}
	for piece in string.gmatch(" " .. cmdline, "%s+([^%s]+)") do
		table.insert(pieces, piece)
	end

	local program = table.remove(pieces, 1)
	assert(program ~= nil, "cmdline must include a program")
	local result = process.spawn(program, pieces, {
		stdio = "inherit",
	})
	if not result.ok then
		process.exit(result.code)
	end
end

exec("wally install")
exec("rojo sourcemap -o sourcemap.json ./test.project.json")
exec("wally-package-types --sourcemap sourcemap.json ./DevPackages/")
exec("wally-patch-package")
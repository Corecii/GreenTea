local process = require("@lune/process")
local fs = require("@lune/fs")

local function exec(cmdline: string | { string }, options: process.SpawnOptions?)
	local pieces = {}
	if typeof(cmdline) == "string" then
		for piece in string.gmatch(" " .. cmdline, "%s+([^%s]+)") do
			table.insert(pieces, piece)
		end
	else
		pieces = cmdline
	end

	local options = table.clone(options or {})
	if options.stdio == nil then
		options.stdio = "inherit"
	end

	local program = table.remove(pieces, 1)
	assert(program ~= nil, "cmdline must include a program")
	local result = process.spawn(program, pieces, options :: any)
	if not result.ok then
		process.exit(result.code)
	end

	return result.stdout:gsub("^[\r\n]*", ""):gsub("[\r\n]*$", "")
end

local function zip(out: string, items: { string })
	local result
	if process.os == "windows" then
		result = process.spawn("C:\\Windows\\system32\\tar.exe", { "-a", "-cf", out, unpack(items) })
	else
		result = process.spawn("zip", { "-r", out, unpack(items) })
	end

	if not result.ok then
		warn(`Failed to zip: ({result.code}) {result.stderr}`)
		process.exit(result.code)
	end
end

exec("lune run prepare-lune-version", { shell = true })

if fs.isDir("lune-build") then
	fs.removeDir("lune-build")
end

fs.writeDir("lune-build")

fs.copy("src", "lune-build/GreenTea")
zip("GreenTea-lune.zip", { "-C", "lune-build", "GreenTea" })

fs.removeDir("lune-build")

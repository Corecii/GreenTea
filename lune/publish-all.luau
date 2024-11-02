local process = require("@lune/process")
local fs = require("@lune/fs")
local serde = require("@lune/serde")

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

local function sanitizePattern(str: string)
	return string.gsub(str, "([%.%-%*%+%?%%])", "%%%1")
end

exec("git remote update")

if exec("git symbolic-ref --short HEAD") ~= "main" then
	warn("Cannot publish.")
	warn("You must be on the main branch to publish.")
	process.exit(1)
end

if exec("git rev-parse HEAD") ~= exec("git rev-parse main") then
	warn("Cannot publish.")
	warn("You must be up-to-date with GitHub to publish.")
	process.exit(1)
end

if exec("git status --porcelain") ~= "" then
	warn("Cannot publish.")
	warn("There are uncommitted changes. Commit or stash them before publishing.")
	process.exit(1)
end

local changelogRaw = fs.readFile("CHANGELOG.md")
local wally = serde.decode("toml", fs.readFile("wally.toml"))

local changelogEntry = changelogRaw:match(`## {sanitizePattern(wally.package.version)}.-\n+(.-)\n+##`)

if not changelogEntry then
	warn("Cannot publish.")
	warn(`No changelog entry found for {wally.package.version}`)
	process.exit(1)
end

exec("rojo build -o GreenTea.rbxm default.project.json")

exec({
	"gh",
	"release",
	"create",
	`v{wally.package.version}`,
	"-t",
	`v{wally.package.version}`,
	"-n",
	changelogEntry,
	"GreenTea.rbxm",
})

fs.removeFile("GreenTea.rbxm")

exec("wally publish")
exec("wally publish", { cwd = "src-t-standalone-wally" })

exec("pesde install")
exec("pesde publish --yes")

exec("pesde install", { cwd = "src-t-standalone-pesde" })
exec("pesde publish --yes", { cwd = "src-t-standalone-pesde" })

-- Publish lune version
-- (makes temporary changes which we use Git to undo)

exec("lune run prepare-lune-version", { shell = true })

exec("pesde install")
exec("pesde publish --yes")

exec("pesde install", { cwd = "src-t-standalone-pesde" })
exec("pesde publish --yes", { cwd = "src-t-standalone-pesde" })

exec("git reset --hard HEAD", { shell = true })

-- Publish docs

exec("lune run moonwave -- build --publish", { shell = true })

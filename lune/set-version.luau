local fs = require("@lune/fs")
local serde = require("@lune/serde")
local process = require("@lune/process")

local wally = serde.decode("toml", fs.readFile("wally.toml"))

local lastVersion = wally.package.version
local nextVersion = assert(process.args[1], "version required!\n\nusage:\n  lune set-version -- <version>")

assert(
	string.match(nextVersion, "^[0-9]+%.[0-9]+%.[0-9]+$")
		or string.match(nextVersion, "^[0-9]+%.[0-9]+%.[0-9]+%-[%w%._]+$")
		or string.match(nextVersion, "^[0-9]+%.[0-9]+%.[0-9]+%-[%w%._]+%+[%w%._]+$")
		or string.match(nextVersion, "^[0-9]+%.[0-9]+%.[0-9]+%+[%w%._]+$"),
	"version must be a semver"
)

local function sanitizePattern(str: string)
	return string.gsub(str, "([%.%-%*%+%?%%])", "%%%1")
end

local function replaceVersion(file: string, from: string, to: string)
	fs.writeFile(file, (string.gsub(fs.readFile(file), sanitizePattern(from), to)))
end

replaceVersion("README.md", `@{lastVersion}`, `@{nextVersion}`)
replaceVersion("wally.toml", `version = "{lastVersion}"`, `version = "{nextVersion}"`)
replaceVersion("src-t-standalone/wally.toml", `version = "{lastVersion}"`, `version = "{nextVersion}"`)
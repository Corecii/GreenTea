--[[
	This is a simplified Roblox -> Lune converter specifically for GreenTea.
	I have chosen this approach over DarkLua and other because...
	1. String requires are coming to Roblox soon, which makes this less necessary.
	2. DarkLua and similar add a lot of complexity for something that is really
	   simple in this case due to GreenTea being such a small library.
]]

local fs = require("@lune/fs")
local serde = require("@lune/serde")

local function rewrite(path: string, process: (str: string) -> string)
	return fs.writeFile(path, process(fs.readFile(path)))
end

for _, name in fs.readDir("src") do
	rewrite(`src/{name}`, function(str)
		return str:gsub('require%(([^"].-)%)', function(inner)
			local target = inner:gsub("script%.Parent%.", "./"):gsub("script%.", "./")

			return `require("{target}")`
		end)
	end)
end

rewrite(`src-t-standalone-pesde/pesde.toml`, function(str)
	return str:gsub("require%(script.Parent.roblox_packages.GreenTea%)", 'require("../pkgs/GreenTea")')
end)

rewrite("pesde.toml", function(str)
	local data = serde.decode("toml", str)
	data.target.environment = "lune"
	data.target.build_files = nil

	return serde.encode("toml", data, true)
end)

rewrite("src-t-standalone-pesde/pesde.toml", function(str)
	local data = serde.decode("toml", str)
	data.target.environment = "lune"
	data.target.build_files = nil

	return serde.encode("toml", data, true)
end)

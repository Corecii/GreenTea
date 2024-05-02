
local fs = require("@lune/fs")
local process = require("@lune/process")
local serde = require("@lune/serde")

if process.env.windir ~= nil then
	-- Windows

	local appDataPath = process.env.LOCALAPPDATA
	local robloxVersionsPath = `{appDataPath}\\Roblox\\Versions`

	if not fs.isDir(robloxVersionsPath) then
		error(`Roblox versions folder not found. Please install Roblox. Checked here: {robloxVersionsPath}`)
	end

	local mostRecentStudioVersionPath
	for _, versionDirName in fs.readDir(robloxVersionsPath) do
		local versionDirPath = `{robloxVersionsPath}\\{versionDirName}`

		if not fs.isDir(versionDirPath) then
			continue
		end
		if not fs.isFile(`{versionDirPath}\\RobloxStudioBeta.exe`) then
			continue
		end

		mostRecentStudioVersionPath = versionDirPath
		break
	end

	if mostRecentStudioVersionPath == nil then
		error(`Roblox Studio not found. Please install Roblox. Checked here: {robloxVersionsPath}`)
	end

	local clientSettingsDir = `{mostRecentStudioVersionPath}\\ClientSettings`
	if not fs.isDir(clientSettingsDir) then
		fs.writeDir(clientSettingsDir)
	end

	local clientAppSettingsPath = `{clientSettingsDir}\\ClientAppSettings.json`
	local clientAppSettings

	if fs.isFile(clientAppSettingsPath) then
		clientAppSettings = serde.decode("json", fs.readFile(clientAppSettingsPath))

		local metadata = fs.metadata(clientAppSettingsPath)
		if metadata.permissions and metadata.permissions.readOnly then
			local result = process.spawn("attrib", {"-r", clientAppSettingsPath })
			assert(result.ok, `Failed to remove read-only attribute:\n{result.stderr}`)
		end
	else
		clientAppSettings = {}
	end

	clientAppSettings["FFlagEnableLoadModule"] = true

	fs.writeFile(clientAppSettingsPath, serde.encode("json", clientAppSettings))

	local result = process.spawn("attrib", {"+r", clientAppSettingsPath })
	assert(result.ok, `Failed to add read-only attribute:\n{result.stderr}`)

	print(`Wrote FFlagEnableLoadModule to ClientAppSettings.json at {clientAppSettingsPath}`)
else
	error("This script only supports windows. Please implement support for your OS. I can't because I only have a Windows machine, sorry.")
end
local fs = require("@lune/fs")
local task = require("@lune/task")
local process = require("@lune/process")

export type Path = string

local function getModifiedAt(path: Path): number
	local meta = fs.metadata(path)
	return assert(meta.modifiedAt).unixTimestampMillis
end

local function rebuild_item(options: {
	base: Path,
	out: Path,
	file: Path
})
	local basePath = `{options.base}/{options.file}`
	local outPath = `{options.out}/{options.file}`

	local baseMeta = fs.metadata(basePath)
	if not baseMeta.exists then
		local outMeta = fs.metadata(outPath)
		if outMeta.exists then
			fs.removeFile(outPath)
		end
		return
	end

	local outParent = string.match(outPath, "(.+)[/\\][^/\\]*$")
	if outParent then
		fs.writeDir(outParent)
	end

	local baseFile = fs.readFile(basePath)
	local outFile = string.gsub(
		baseFile,
		"(%-%-%-[^\n]*)\\ *(\r?\n)",
		"%1  %2"
	)

	fs.writeFile(outPath, outFile)
end

local function rebuild_directory(options: {
	base: Path,
	out: Path,
	dir: Path,
	watch: { [Path]: number }?,
})
	for _, child in fs.readDir(`{options.base}/{options.dir}`) do
		if fs.metadata(`{options.base}/{options.dir}/{child}`).kind == "dir" then
			rebuild_directory({
				base = options.base,
				out = options.out,
				dir = `{options.dir}/{child}`,
				watch = options.watch,
			})
		else
			rebuild_item({
				base = options.base,
				out = options.out,
				file = `{options.dir}/{child}`,
			})

			if options.watch then
				options.watch[`{options.dir}/{child}`] = getModifiedAt(`{options.base}/{options.dir}/{child}`)
			end
		end
	end
end

local function watch(items: { [Path]: number }, onChanged: (Path) -> ())
	local thread = task.spawn(function()
		while true do
			for path, timestamp in items do
				local meta = fs.metadata(path)
				if meta.exists and meta.modifiedAt.unixTimestampMillis > timestamp then
					onChanged(path)
				end
				task.wait(1/60)
			end
		end
	end)
	return function()
		task.cancel(thread)
	end
end

local function buildWatchlist(options: {
	base: Path,
	out: Path,
	dir: Path,
	watch: { [Path]: number },
})
	for _, child in fs.readDir(`{options.base}/{options.dir}`) do
		if fs.metadata(`{options.base}/{options.dir}/{child}`).kind == "dir" then
			buildWatchlist({
				base = options.base,
				out = options.out,
				dir = `{options.dir}/{child}`,
				watch = options.watch,
			})
		else
			options.watch[`{options.dir}/{child}`] = getModifiedAt(`{options.base}/{options.dir}/{child}`)
		end
	end
end

local function copy(options: {
	base: Path,
	out: Path,
	items: { Path },
	watch: { [Path]: number }?,
})
	for _, item in options.items do
		fs.copy(`{options.base}/{item}`, `{options.out}/{item}`, {
			overwrite = true,
		})

		if options.watch then
			if fs.metadata(`{options.base}/{item}`).kind == "file" then
				options.watch[item] = getModifiedAt(`{options.base}/{item}`)
			else
				buildWatchlist({
					base = options.base,
					out = options.out,
					dir = item,
					watch = options.watch,
				})
			end
		end
	end
end

local codeWatchList = {}
rebuild_directory({
	base = "src",
	out = "moonwave/src",
	dir = ".",
	watch = codeWatchList,
})

local copyWatchList = {}
copy({
	base = ".",
	out = "moonwave",
	items = {
		"pages",
		"CHANGELOG.md",
		"README.md",
		"moonwave.toml",
	},
	watch = copyWatchList,
})

if process.args[1] == "dev" then
	print("watching for changes to existing files...")
	watch(codeWatchList, function(filePath)
		print(`  rebuilding {filePath}`)
		rebuild_item({
			base = "src",
			out = "moonwave/src",
			file = filePath,
		})
	end)
	watch(copyWatchList, function(filePath)
		print(`  copying {filePath}`)
		copy({
			base = ".",
			out = "moonwave",
			items = {
				filePath,
			},
		})
	end)

	process.spawn("moonwave", process.args, {
		stdio = "inherit",
		cwd = "moonwave",
		shell = true,
	})
elseif process.args[1] == "build" then
	process.spawn("moonwave", process.args, {
		stdio = "inherit",
		cwd = "moonwave",
		shell = true,
	})
else
	warn(`unknown command: {process.args[1]}`)
end


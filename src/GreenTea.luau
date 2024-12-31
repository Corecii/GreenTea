local SOFT_MAX_LINE_LENGTH = 80

local greenteaConstructorsSet = {}

local function highlightWrap(str: string, highlight: string?)
	if highlight then
		return `$${highlight}$:{str}:${highlight}$$`
	else
		return str
	end
end

-- Returns the maximum number of characters in any line
local function lineLengthOf(str: string)
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
end

-- Returns the minimum number of spaces prefixing every line
local function spaceLengthOf(str: string)
	local minSpaces
	for _, line in string.split(str, "\n") do
		local start, finish = string.find(line, "^ *")
		if start and finish then
			local length = finish - start + 1
			if not minSpaces or length < minSpaces then
				minSpaces = length
			end
		end
	end
	return minSpaces or 0
end

local function tabFirst(str: string)
	return "    " .. string.gsub(str, "\n", "\n    ")
end

local function tabSecond(str: string)
	return string.gsub(str, "\n", "\n    ")
end

type Range = { min: number?, minExclusive: boolean, max: number?, maxExclusive: boolean }
type RangeIn = { min: number?, minExclusive: boolean?, max: number?, maxExclusive: boolean? }

type RangeInput = string | number | RangeIn

local function parseRange(rangeIn: RangeInput): Range
	if typeof(rangeIn) == "table" then
		if
			typeof(rangeIn.min) == "number"
			and typeof(rangeIn.max) == "number"
			and (rangeIn.minExclusive == nil or typeof(rangeIn.minExclusive) == "boolean")
			and (rangeIn.maxExclusive == nil or typeof(rangeIn.maxExclusive) == "boolean")
		then
			return {
				min = rangeIn.min,
				minExclusive = rangeIn.min and rangeIn.minExclusive or false,
				max = rangeIn.max,
				maxExclusive = rangeIn.max and rangeIn.maxExclusive or false,
			}
		else
			error("invalid range table")
		end
	end

	local rangeNum = tonumber(rangeIn)
	if rangeNum then
		return { min = nil, minExclusive = false, max = rangeNum, maxExclusive = false }
	end
	assert(type(rangeIn) == "string", "analysis hint")

	local leftBracket = "[%[%(]"
	local rightBracket = "[%]%)]"
	local numbers = ".-"

	local prefix, minStr, maxStr, suffix =
		string.match(rangeIn, `^%s*({leftBracket})%s*({numbers}),%s*({numbers})%s*({rightBracket})%s*$`)

	if not prefix then
		error(
			'invalid range string, expected format: one of "[min, max]", "(min, max)", "[min, max)", "(min, max]", or "max" (leave min/max empty for no limit)'
		)
	end

	local minExclusive = prefix == "("
	local maxExclusive = suffix == ")"

	local min: number?
	if minStr == "" then
		min = nil
		minExclusive = false
	else
		min = tonumber(minStr)
		if not min then
			error(`invalid number for min in range string: {minStr}`)
		end
	end
	local max: number?
	if maxStr == "" then
		max = nil
		maxExclusive = false
	else
		max = tonumber(maxStr)
		if not max then
			error(`invalid number for max in range string: {maxStr}`)
		end
	end

	return {
		min = min,
		minExclusive = minExclusive,
		max = max,
		maxExclusive = maxExclusive,
	}
end

local function displayRange(range: Range): string
	if not range.min and not range.max then
		return "[-inf, inf]"
	end

	local leftBracket = range.minExclusive and "(" or "["
	local rightBracket = range.maxExclusive and ")" or "]"
	return leftBracket .. (range.min or "") .. ", " .. (range.max or "") .. rightBracket
end

local function checkRange(value: number, range: Range): boolean
	if range.min then
		if range.minExclusive then
			if value <= range.min then
				return false
			end
		else
			if value < range.min then
				return false
			end
		end
	end

	if range.max then
		if range.maxExclusive then
			if value >= range.max then
				return false
			end
		else
			if value > range.max then
				return false
			end
		end
	end

	return true
end

--- @class GreenTea
local GreenTea = {}

GreenTea.__greenteaConstructorsSet = greenteaConstructorsSet

export type Cause = {
	ok: boolean,

	encompassingType: Type?,

	errs: {
		{
			type: Type,
			input: any?,
			message: string?,
		}
	},

	formatErr: (_self: any?) -> string,
}

type CauseMt = {
	__tostring: (self: any) -> string,
}

local CauseMt = {
	__tostring = function(self: Cause)
		if self.ok then
			return "ok"
		else
			return self:formatErr()
		end
	end,
}

local function newCauseTuple(...: any): { __tuple: { [number]: any, n: number } }
	return { __tuple = table.pack(...) }
end

local function expandCauseTuple(input: any?): ...any
	if type(input) == "table" and input.__tuple then
		return unpack(input.__tuple, 1, input.__tuple.n)
	else
		return input
	end
end

local Cause = {}

local function causeFormatErr(self: Cause, ...: any)
	if self.ok then
		return "ok"
	else
		local type = self.encompassingType or self.errs[#self.errs].type

		return type:formatErr(self)
	end
end

function Cause.new(ok: boolean, errs: { { type: any, input: any?, message: string? } }): Cause
	return setmetatable({ ok = ok, errs = errs, formatErr = causeFormatErr } :: Cause, CauseMt) :: any
end

function Cause.ok(): Cause
	return Cause.new(true, {})
end

function Cause.extendOk(cause: Cause?): Cause
	if not cause then
		return Cause.new(true, {})
	else
		return cause
	end
end

function Cause.err(type: any, input: any?, message: string?): Cause
	return Cause.new(false, { { type = type, input = input, message = message } })
end

function Cause.extendErr(cause: Cause?, type: any, input: any?, message: string?): Cause
	if not cause then
		return Cause.new(false, { { type = type, input = input, message = message } })
	else
		table.insert(cause.errs, { type = type, input = input, message = message })
		return cause
	end
end

function Cause.errs(errs: { { type: any, input: any?, message: string? } }): Cause
	return Cause.new(false, errs)
end

function Cause.extendErrs(cause: Cause?, errs: { { type: any, input: any?, message: string? } }): Cause
	if not cause then
		return Cause.new(false, errs)
	else
		table.move(errs, 1, #errs, #cause.errs + 1, cause.errs)
		return cause
	end
end

type Packed = { n: number, [number]: any }

type TypeMt = {
	__index: {
		matches: (self: any, ...any) -> (boolean, Cause),
		assert: (self: any, ...any) -> ...any,

		format: (self: any) -> string,
		formatErr: (self: any, cause: any, values: Packed?) -> string,
	},

	__call: (self: any, ...any) -> (boolean, string?),

	__tostring: (self: any) -> string,
}

type TypeRaw = {
	kind: string,

	meta: {
		[string]: any,
	}?,

	_matches: (...any) -> Cause,
	_format: (highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any }) -> string,

	_needsParens: boolean?,

	basic: {
		typeof: string?,
		type: string?,
	}?,

	any: {
		allowNil: boolean?,
	}?,

	unknown: {
		allowNil: boolean?,
	}?,

	custom: {
		type: Type?,
		name: string,
		typechecker: (any) -> (boolean, any?),
	}?,

	number: {
		range: Range?,
		integer: boolean?,
	}?,

	string: {
		pattern: string?,
		bytes: Range?,
		graphemes: Range?,
		unicode: boolean?,
	}?,

	thread: {
		status: { [CoroutineStatus]: boolean }?,
	}?,

	instanceIsA: {
		class: string,
	}?,

	literal: {
		value: any,
	}?,

	tuple: {
		contents: { Type },
		vararg: Type?,
	}?,

	vararg: {
		type: Type,
		length: Range?,
	}?,

	fn: {
		args: Type,
		returns: Type,
	}?,

	table: {
		contents: { [string]: Type },
		indexer: { key: Type, value: Type }?,

		array: boolean?,
		count: Range?,
		raw: boolean?,
	}?,

	intersection: {
		contents: { Type },
	}?,
	union: {
		contents: { Type },
		optional: boolean?,
	}?,
}

export type Type = typeof(setmetatable({} :: TypeRaw, {} :: TypeMt))

local Type = {}
Type.__index = Type

--- @class Type
--- Represents a type we can check against or inspect.\
--- Use the methods to check against the type.\
--- Use the properties to inspect the type.

--- @type Range { min: number?, minExclusive: boolean, max: number?, maxExclusive: boolean }
--- @within Type

--- @type RangeInput string | number | { min: number?, minExclusive: boolean?, max: number?, maxExclusive: boolean? }
--- @within GreenTea
--- A value that can be parsed as a range.\
--- As a string, can be "[min, max]", "(min, max)", "(min, max]", "(, max]" etc. or a number max inclusive.
--- As a number, it's interpreted as a number max inclusive.

--- @type Cause { ok: boolean, encompassingType: Type?, errs: { { type: any, input: any?, message: string? } }, formatErr: (self: Cause) -> string }
--- @within Type
--- Represents the result of a :matches() call.\
--- Includes a `__tostring` metamethod, which formats to a readable error.

--- @within Type
--- @prop kind string

--- @within Type
--- @prop meta { [string]: any }?
--- Contains user-specified metadata about the type.

--- @within Type
--- @prop basic { typeof: string?, type: string? }?

--- @within Type
--- @prop any { allowNil: boolean? }?

--- @within Type
--- @prop unknown { allowNil: boolean? }?

--- @within Type
--- @prop custom { type: Type?, name: string, typechecker: (any) -> (boolean, any?) }?

--- @within Type
--- @prop number { range: Range?, integer: boolean? }?

--- @within Type
--- @prop string { pattern: string?, bytes: Range?, graphemes: Range?, unicode: boolean? }?

--- @within Type
--- @prop thread { status: { [CoroutineStatus]: boolean }? }?

--- @within Type
--- @prop instanceIsA { class: string }?

--- @within Type
--- @prop literal { value: any }?

--- @within Type
--- @prop tuple { contents: { Type }, vararg: Type? }?

--- @within Type
--- @prop vararg { type: Type, length: Range? }?

--- @within Type
--- @prop fn { args: Type, returns: Type }?

--- @within Type
--- @prop table { contents: { [string]: Type }, indexer: { key: Type, value: Type }, array: boolean?, count: Range?, raw: boolean? }?

--- @within Type
--- @prop intersection { contents: { Type } }?

--- @within Type
--- @prop union { contents: { Type }, optional: boolean? }?

--- Returns a value with information about whether or not the input matches the type.
function Type.matches(self: Type, ...: any): (boolean, Cause)
	local cause = self._matches(...)
	cause.encompassingType = self
	return cause.ok, cause
end

--- Errors if the input does not match the type, otherwise returns the input.
function Type.assert<T...>(self: Type, ...: T...): T...
	local ok, cause = self:matches(...)
	if not ok then
		error(cause:formatErr())
	end
	return ...
end

--- Formats the type for display to the user.
function Type.format(self: Type): string
	return self._format({}, SOFT_MAX_LINE_LENGTH, {})
end

--- Wraps a function with type asserts for its input args and returns.\
--- \
--- This implementation is not ergonomic, and is easily confused with GreenTea.wrapFn.
--- For this reason, Type.wrapFn will be removed in the next breaking release.\
--- @deprecated v0.4.11 -- use `GreenTea.wrapFn` instead
function Type.wrapFn<T>(self: Type, fn: T): T
	assert(typeof(fn) == "function", "fn must be a function")
	assert(self.fn ~= nil, "self must be a GreenTea.fn type")

	return function(...: any)
		self.fn.args:assert(...)

		return self.fn.returns:assert(fn(...))
	end :: any
end

--- In type definitions via `typeof(Type:type())`, returns the Luau type this
--- GreenTea type represents.\
--- At runtime, returns self. This allows you to compose complex GreenTea types
--- by passing GreenTea.type() into a GreenTea constructor. (Only applies when
--- "built" with `GreenTea.build(GreenTea constructors here)`)
function Type.type(self: any): ...any
	return self
end

local isStringUnicode

if utf8.graphemes then
	function isStringUnicode(str: string)
		return pcall(utf8.graphemes, str) == true
	end
else
	function isStringUnicode(str: string)
		return utf8.len(str) ~= nil
	end
end

local function truncate(count: number, str: string)
	if not isStringUnicode(str) then
		return "<invalid unicode>"
	end

	if #str < count then
		return str
	end

	if not utf8.graphemes then
		local offset = utf8.offset(str, count + 1)
		if not offset then
			return "<invalid unicode>"
		end

		return str:sub(1, offset - 1)
	end

	local rebuiltString = {}
	for graphemeStart, graphemeFinish in utf8.graphemes(str) do
		local grapheme = str:sub(graphemeStart, graphemeFinish)
		table.insert(rebuiltString, grapheme)
		if #rebuiltString >= count then
			return `[{table.concat(rebuiltString)}...]`
		end
	end

	return str
end

function Type.__call(self: Type, ...: any): (boolean, string?)
	local ok, cause = self:matches(...)
	if ok then
		return true
	else
		return false, tostring(cause)
	end
end

function Type.__tostring(self: Type): string
	return `GreenTea.Type({self:format()})`
end

local function tostringLiteral(value: any?)
	if typeof(value) == "string" then
		return string.format("%q", value)
	else
		return tostring(value)
	end
end

local function displayInputType(...: any?)
	local inputStr, typeStr
	if select("#", ...) <= 1 then
		local input = ...
		if input and typeof(input) == "table" and input.__tuple then
			return displayInputType(expandCauseTuple(input))
		end

		if typeof(...) == "string" then
			inputStr = string.format("%q", ...)
		else
			inputStr = tostring(...)
		end

		-- disabled for now, as it can take a long time to build a GreenTea type
		-- for large inputs.

		-- local gtType = GreenTea.typeof(...) :: any
		-- typeStr = gtType:format():gsub("\n%s*", " ")

		typeStr = typeof(...)
	else
		local length = select("#", ...)
		local inputs = table.create(length)
		local inputStrBuilder = table.create(length)
		local typeStrBuilder = table.create(length)
		for i = 1, select("#", ...) do
			local input = select(i, ...)

			table.insert(inputs, GreenTea.typeof(input))

			local inputStr
			if typeof(input) == "string" then
				inputStr = string.format("%q", input)
			else
				inputStr = tostring(input)
			end
			table.insert(inputStrBuilder, inputStr)

			table.insert(typeStrBuilder, typeof(input))
		end

		-- disabled for now, as it can take a long time to build a GreenTea type
		-- for large inputs.

		-- typeStr = GreenTea.tuple(unpack(inputs))

		typeStr = `({table.concat(typeStrBuilder, ", ")})`

		inputStr = `({table.concat(inputStrBuilder, ", ")})`
	end

	inputStr = truncate(20, inputStr)
	typeStr = truncate(20, typeStr)

	return `{typeStr} ({inputStr})`
end

--- Formats the type for display to the user highlighting a specific type.
function Type.formatErr(self: Type, cause: Cause): string
	-- By the way, this is kind of hacky and not really the right way to do
	-- this. Got to replace formatting entirely with something more made for it
	-- someday. But this does the job for now, and its internals aren't exposed,
	-- so we can replace it later!

	local header = "type error"

	local headerErr = cause.errs[1]
	if headerErr.message then
		header = string.gsub(headerErr.message, "$input", displayInputType(headerErr.input))
	else
		header = `expected {headerErr.type:format()}, got {displayInputType(headerErr.input)}`
	end

	local errorsForFormat = {}
	for index, err in cause.errs do
		if not (err.message or index == 1) then
			continue
		end
		errorsForFormat[err.type] = `error{index}`
	end

	local formattedStr = self._format(errorsForFormat, SOFT_MAX_LINE_LENGTH, {})

	formattedStr = `\n{formattedStr}\n`

	local order = {}
	for numStr in string.gmatch(formattedStr, "%$%$error(%d+)%$:") do
		local num = tonumber(numStr) :: number
		table.insert(order, num)
	end

	for orderIndex = #order, 1, -1 do
		local index = order[orderIndex]
		local err = cause.errs[index]
		formattedStr = string.gsub(
			formattedStr,
			`([^\n]*)$$error{index}$:(.*):$error{index}$$([^\n]*)\n(.*)$`,
			function(pre: string, mid: string, post: string, everythingElse: string)
				local errPointer
				if string.find(mid, "\n", 1, true) == nil then
					local preNoAnnotations = string.gsub(string.gsub(pre, "%$%$error%d+%$:", ""), ":%$error%d+%$%$", "")
					errPointer = string.rep(" ", #preNoAnnotations) .. string.rep("^", #mid)
				else
					local realStr = `{pre}{mid}{post}`
					local lineLength = lineLengthOf(realStr)
					local spaceLength = spaceLengthOf(realStr)

					errPointer = string.rep(" ", spaceLength) .. string.rep("^", lineLength - spaceLength)
				end

				if err.message then
					errPointer = `{errPointer} $$error{index}$$`
				end

				local preLines = {}

				while true do
					local nextLine, newEverythingElse = string.match(everythingElse, "^([^\n]*)\n(.*)$")
					if not (nextLine and newEverythingElse) then
						break
					end
					if string.match(nextLine, "^%s*^") then
						table.insert(preLines, nextLine)
						everythingElse = newEverythingElse
					else
						break
					end
				end

				if preLines[1] then
					return `{pre}{mid}{post}\n{table.concat(preLines, "\n")}\n{errPointer}\n{everythingElse}`
				else
					return `{pre}{mid}{post}\n{errPointer}\n{everythingElse}`
				end
			end :: any
		)
	end

	for index, err in cause.errs do
		if err.message then
			local errMessage = string.gsub(err.message, "$input", displayInputType(err.input))
			formattedStr = string.gsub(formattedStr, `%$%$error{index}%$%$`, errMessage)
		end
	end

	formattedStr = string.gsub(
		formattedStr,
		"\n(%s*)(^+) ([^\n]+)",
		function(spaces, carets, errMessage): string?
			if #spaces + #carets + #errMessage + 1 <= SOFT_MAX_LINE_LENGTH then
				return nil
			end

			return `\n{spaces}{carets}\n{string.rep(" ", #spaces)}{errMessage}`
		end :: any
	)

	formattedStr = formattedStr:sub(1, -2) -- remove final newline

	return `{header}{formattedStr}`
end

--- Returns whether or not the input value is a GreenTea.Type object.
function GreenTea.isGreenTeaType(value: any)
	return typeof(value) == "table" and getmetatable(value) == Type
end

GreenTea.isGtType = GreenTea.isGreenTeaType

local function newBasicType(typeName: string, useType: boolean?)
	return function()
		local self
		self = {
			kind = "basic",
			basic = {
				typeof = if useType then nil else typeName,
				type = if useType then typeName else nil,
			},
			_matches = function(input: any, ...: any)
				local inputType = useType and type(input) or typeof(input)
				if inputType == typeName then
					return Cause.ok()
				else
					return Cause.err(self, input, `expected {typeName}, got $input`)
				end
			end,
			_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
				return highlightWrap(typeName, highlight[self])
			end,
		}
		return setmetatable(self :: any, Type)
	end
end

GreenTea.__newBasicType = newBasicType

--- Creates a new GreenTea Type that matches any value, excluding nil.
function GreenTea.any(options: { allowNil: boolean? }?): any
	local allowNil = options and options.allowNil

	local self: TypeRaw
	self = {
		kind = "any",
		any = {
			allowNil = (allowNil or nil) :: boolean?,
		},
		_matches = function(input: any, ...: any?)
			if not allowNil and input == nil then
				return Cause.err(self, input, "expected non-nil, got nil")
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			return highlightWrap("any", highlight[self])
		end,
	}
	return setmetatable(self :: any, Type)
end

--- Creates a new GreenTea Type that matches any value, excluding nil.\
--- This returns the unknown type, which has different behavior from any.\
--- The runtime typechecking behavior is the same as GreenTea.any.
function GreenTea.unknown(options: { allowNil: boolean? }?): unknown
	local allowNil = options and options.allowNil

	local self: TypeRaw
	self = {
		kind = "unknown",
		unknown = {
			allowNil = (allowNil or nil) :: boolean?,
		},
		_matches = function(input: any, ...: any)
			if not allowNil and input == nil then
				return Cause.err(self, input, "expected non-nil, got nil")
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			return highlightWrap("unknown", highlight[self])
		end,
	}
	return setmetatable(self :: any, Type)
end

--- Creates a new GreenTea Type that matches no values.
function GreenTea.never(): never
	local self: TypeRaw
	self = {
		kind = "never",
		_matches = function(input: any, ...: any)
			return Cause.err(self, input, "expected never, got $input")
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			return highlightWrap("never", highlight[self])
		end,
	}
	return setmetatable(self, Type) :: any
end

local any = nil :: any

--- Creates a new GreenTea Type that matches boolean values.
function GreenTea.boolean(): boolean
	return any
end

GreenTea.boolean = newBasicType("boolean") :: () -> boolean
GreenTea.bool = GreenTea.boolean

--- Creates a new GreenTea Type that matches number values.
function GreenTea.Instance(): Instance
	return any
end

GreenTea.Instance = newBasicType("Instance") :: () -> Instance

type CoroutineStatus = "dead" | "normal" | "running" | "suspended"
local coroutineStatuses = { "dead", "normal", "running", "suspended" }

--- Creates a new GreenTea Type that matches coroutine values.
function GreenTea.coroutine(options: { status: CoroutineStatus | { CoroutineStatus } | nil }?): thread
	local expectedStatusSet
	local expectedStatusStr = "any"
	if options and options.status then
		expectedStatusSet = {}
		if type(options.status) == "string" then
			expectedStatusSet[options.status] = true
		elseif type(options.status) == "table" then
			for _, v in options.status do
				expectedStatusSet[v] = true
			end
		end

		local expectedStatusStrBuilder = {}
		for k, _ in expectedStatusSet do
			if not table.find(coroutineStatuses, k) then
				error(`{k} is not a valid coroutine status`)
			end

			table.insert(expectedStatusStrBuilder, k)
		end

		table.sort(expectedStatusStrBuilder)
		expectedStatusStr = table.concat(expectedStatusStrBuilder, " | ")
		if #expectedStatusStrBuilder > 1 then
			expectedStatusStr = `({expectedStatusStr})`
		end
	end

	local self: TypeRaw
	self = {
		kind = "thread",
		thread = {
			status = expectedStatusSet,
		},
		_matches = function(input: any, ...: any)
			if type(input) ~= "thread" then
				return Cause.err(self, input, "expected thread, got $input")
			end

			if expectedStatusSet then
				local coroutineStatus = coroutine.status(input)

				if not expectedStatusSet[coroutineStatus] then
					return Cause.err(
						self,
						input,
						`expected thread with status {expectedStatusStr}, got thread with status {coroutineStatus}`
					)
				end
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if expectedStatusSet then
				return highlightWrap(`thread<status: {expectedStatusStr}>`, highlight[self])
			else
				return highlightWrap("thread", highlight[self])
			end
		end,
	}
	return setmetatable(self, Type) :: any
end

GreenTea.thread = GreenTea.coroutine

--- Creates a new GreenTea Type that matches buffer values.
function GreenTea.buffer(): buffer
	-- filler function to make Moonwave happy
	return any
end

GreenTea.buffer = newBasicType("buffer") :: () -> buffer

GreenTea.userdata = newBasicType("userdata", true) :: () -> any

GreenTea.Vector2 = newBasicType("Vector2") :: () -> Vector2
GreenTea.vector = newBasicType("vector", true) :: () -> Vector3
GreenTea.Vector3 = newBasicType("Vector3") :: () -> Vector3
GreenTea.CFrame = newBasicType("CFrame") :: () -> CFrame
GreenTea.Color3 = newBasicType("Color3") :: () -> Color3
GreenTea.UDim = newBasicType("UDim") :: () -> UDim
GreenTea.UDim2 = newBasicType("UDim2") :: () -> UDim2
GreenTea.Ray = newBasicType("Ray") :: () -> Ray
GreenTea.Rect = newBasicType("Rect") :: () -> Rect
GreenTea.Region3 = newBasicType("Region3") :: () -> Region3
GreenTea.BrickColor = newBasicType("BrickColor") :: () -> BrickColor
GreenTea.Font = newBasicType("Font") :: () -> Font

GreenTea.Enum = newBasicType("Enum") :: () -> Enum
GreenTea.EnumItem = newBasicType("EnumItem") :: () -> EnumItem

--- Creates a new GreenTea Type that matches nil values.
function GreenTea.none(): nil
	return any
end

GreenTea.none = newBasicType("nil") :: () -> nil

--- Creates a new GreenTea Type that matches a value literally.\
--- This is checked with a basic == comparison.
function GreenTea.literal<T>(value: T): T
	local literalStr
	if typeof(value) == "string" then
		literalStr = string.format("%q", value)
	else
		literalStr = tostring(value)
	end

	local self: TypeRaw
	self = {
		kind = "literal",
		literal = {
			value = value,
		},
		_matches = function(input: any, ...: any)
			if input == value then
				return Cause.ok()
			else
				return Cause.err(self, input, `expected literally {literalStr}, got $input`)
			end
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			local valueStr
			if typeof(value) == "string" then
				valueStr = literalStr
			else
				valueStr = `literal<{literalStr}>`
			end
			return highlightWrap(valueStr, highlight[self])
		end,
	}
	return setmetatable(self :: any, Type)
end

local allowedEndings = { "spec", "t", "d", "story", "storybook", "bench" }

--- Creates a new GreenTea Type that matches a custom typechecker
--- with GreenTea type as a base.
function GreenTea.withCustom<T>(type: T, typechecker: (any) -> (boolean, any?), name: string?): T
	if not name then
		local source, line, fnName = debug.info(typechecker, "sln")
		source = source or "unknown"
		line = line or 0
		fnName = fnName or ""

		if fnName ~= "" then
			name = fnName
		else
			local scriptNamePre, scriptNamePost = string.match(source, "([^%.]+)%.(.-)$")
			if not (scriptNamePre and scriptNamePost) then
				name = `{source}:{line}`
			else
				if table.find(allowedEndings, scriptNamePost) then
					name = `{scriptNamePre}.{scriptNamePost}:{line}`
				else
					name = `{scriptNamePost}:{line}`
				end
			end
		end
	end
	assert(name, "analysis hint")

	local innerType: Type? = if type ~= nil then GreenTea.typeof(type :: any) else nil

	local self: TypeRaw
	self = {
		kind = "custom",
		custom = {
			typechecker = typechecker,
			name = name,
			type = innerType,
		},
		_matches = function(input: any, ...: any)
			if innerType then
				local cause = innerType._matches(input)
				if not cause.ok then
					return Cause.extendErr(cause, self, input)
				end
			end

			local ok, message = typechecker(input)
			if not ok then
				return Cause.err(self, input, message)
			else
				return Cause.ok()
			end
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if recurse[self] then
				return "<cyclic>"
			end
			recurse[self] = true

			local customFormat = highlightWrap(`custom<{name}>`, highlight[self])
			if innerType then
				local innerTypeFormat = innerType._format(highlight, maxLineLength - 1, recurse)
				local combined = `{innerTypeFormat} & {customFormat}`
				if #combined > maxLineLength then
					return tabSecond(`{innerTypeFormat} & {customFormat}`)
				else
					return combined
				end
			else
				return customFormat
			end
		end,
	}

	return setmetatable(self :: any, Type)
end

--- Creates a new GreenTea Type that matches a custom typechecker
--- with `any` as the base type.
function GreenTea.custom(typechecker: (any) -> (boolean, any?), name: string?): any
	return GreenTea.withCustom(nil, typechecker, name)
end

GreenTea.__highlightWrap = highlightWrap
GreenTea.__Type = Type
GreenTea.__Cause = Cause

--- @function isA
--- @within GreenTea
--- @param class string
--- @return any
--- Call IsA to get a type that matches instances of that class.

--- @prop isA { [string]: any }
--- @within GreenTea
--- Types for individual instance classes.\
--- Index this to get a type that matches instances of that class.\
--- For dynamic class names, or for classes not yet added here,\
--- call this with GreenTea.IsA("ClassName").

--- Creates a new GreenTea Type that matches number values.\
--- Optionally, you can specify a range and whether the number must be an integer.\
--- Regardless of specified limits, the returned Luau type will be a basic number. Limit checking will only be done at runtime.\
--- NOTE: NaN is rejected by default. NaN tends to propogate and "poison" numeric values,
--- and it's rarely actually desired. If you want to _allow_ NaN, set `nan = true` in the limits table.
function GreenTea.number(limits: {
	range: RangeInput?,
	integer: boolean?,
	nan: boolean?,
}?): number
	local range = limits and limits.range and parseRange(limits.range)

	local self: TypeRaw
	self = {
		kind = "number",
		number = {
			range = range,
			integer = limits and limits.integer,
			nan = limits and limits.nan,
		},
		_matches = function(input: any, ...: any)
			if typeof(input) ~= "number" then
				return Cause.err(self, input, "expected number, got $input")
			end

			if limits then
				if range and not checkRange(input, range) then
					return Cause.err(self, input, `input out of range (input: {input})`)
				end
				if limits.integer and input % 1 ~= 0 then
					return Cause.err(self, input, `input is not an integer (input: {input})`)
				end
			end

			if not (limits and limits.nan) then
				if input ~= input then
					return Cause.err(self, input, `input is NaN`)
				end
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if not limits then
				return highlightWrap("number", highlight[self])
			end

			local limitsStr = {}
			if limits.integer then
				table.insert(limitsStr, "integer")
			end
			if limits.nan then
				table.insert(limitsStr, "NaN allowed")
			end
			if range then
				table.insert(limitsStr, `range {displayRange(range)}`)
			end

			if #limitsStr == 0 then
				return highlightWrap("number", highlight[self])
			else
				return highlightWrap(`number<{table.concat(limitsStr, ", ")}>`, highlight[self])
			end
		end,
	}
	return setmetatable(self :: any, Type) :: any
end

local function countGraphemes(str: string): number?
	if #str == 0 then
		return 0
	else
		local count = 0
		for _ in utf8.graphemes(str) do
			count += 1
		end
		return count
	end
end

local PATTERN_REPLACEMENTS = { ["\r"] = "\\r", ["\n"] = "\\n", ["\t"] = "\\t" }

--- Creates a new GreenTea Type that matches string values.\
--- Optionally, you can specify a pattern, a length range, and whether the string must be utf8.\
--- NOTE: this supports both "byte length" and "grapheme length". Grapheme length is the number of visible
--- characters. In non-english languages, the number of bytes can be significantly higher than the number
--- of graphemes.\
--- When using graphemes limit, you should still set a (much-higher) bytes limit to prevent abuse, since
--- graphemes have no upper limit to their size. For this reason, this constructor will error if you
--- do not specify a bytes limit when using graphemes limit. If you really want infinite byte length, set
--- bytes to `[0, inf]`.
function GreenTea.string(limits: {
	pattern: string?,
	bytes: RangeInput?,
	graphemes: RangeInput?,
	unicode: boolean?,
}?): string
	local byteLengthRange = limits and limits.bytes and parseRange(limits.bytes)
	local graphemeLengthRange = limits and limits.graphemes and parseRange(limits.graphemes)

	if graphemeLengthRange and not byteLengthRange then
		error(
			"graphemes limit requires bytes limit. Graphemes have no upper limit on size, so if only graphemes limit is set, the byte limit is practically infinite. If you really want infinite byte length, set bytes to `[0, inf]`"
		)
	end

	if graphemeLengthRange and not utf8.graphemes then
		error(
			"graphemes limit is not supported on your platform (your platform does not support the utf8.graphemes function)"
		)
	end

	local self: TypeRaw
	self = {
		kind = "string",
		string = {
			pattern = limits and limits.pattern,
			bytes = byteLengthRange,
			graphemes = graphemeLengthRange,
			unicode = limits and limits.unicode,
		},
		_matches = function(input: any, ...: any)
			if typeof(input) ~= "string" then
				return Cause.err(self, input, "expected string, got $input")
			end

			if limits then
				if limits.unicode then
					if not isStringUnicode(input) then
						return Cause.err(self, input, "input is not unicode")
					end
				end
				if byteLengthRange and not checkRange(#input, byteLengthRange) then
					return Cause.err(
						self,
						input,
						`input length out of range (#input: {#input} from #{truncate(15, tostringLiteral(input))})`
					)
				end
				if graphemeLengthRange then
					local graphemeLength = countGraphemes(input)
					if not graphemeLength then
						return Cause.err(self, input, "input is not a valid unicode string")
					end
					if not checkRange(graphemeLength, graphemeLengthRange) then
						return Cause.err(
							self,
							input,
							`input length out of range (# graphemes: {graphemeLength} from {truncate(
								15,
								tostringLiteral(input)
							)})`
						)
					end
				end
				if limits.pattern and not string.match(input, limits.pattern) then
					return Cause.err(
						self,
						input,
						`input does not match pattern (input: {truncate(15, tostringLiteral(input))})`
					)
				end
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if not limits then
				return highlightWrap("string", highlight[self])
			end

			local limitsStr = {}
			if limits.unicode then
				table.insert(limitsStr, "unicode")
			end
			if graphemeLengthRange then
				table.insert(limitsStr, `graphemes {displayRange(graphemeLengthRange)}`)
			end
			if byteLengthRange then
				table.insert(limitsStr, `bytes {displayRange(byteLengthRange)}`)
			end
			if limits.pattern then
				local pattern = string.gsub(limits.pattern, "[\r\n\t]", PATTERN_REPLACEMENTS)
				table.insert(limitsStr, `pattern "{pattern}"`)
			end

			if #limitsStr == 0 then
				return highlightWrap("string", highlight[self])
			else
				return highlightWrap(`string<{table.concat(limitsStr, ", ")}>`, highlight[self])
			end
		end,
	}
	return setmetatable(self :: any, Type) :: any
end

--- Creates a new GreenTea Type that matches any `typeof(value)` type.
function GreenTea.isTypeof<T>(typeName: string, value: T?): T
	return newBasicType(typeName)()
end

--- Creates a new GreenTea Type that matches any `type(value)` type.
function GreenTea.isType<T>(typeName: string, value: T?): T
	return newBasicType(typeName)()
end

--- @within GreenTea
--- @function types not listed here
--- A few methods are not listed in the docs so that they don't clutter with basic types.\
--- Here's a list: userdata, Vector2, vector, Vector3, CFrame, Color3, UDim, UDim2,
--- Ray, Rect, Region3, BrickColor, Font, Enum, EnumItem.\
--- For the most part, any Luau type you'd naturally write likely also exists under GreenTea.
--- Less common types are excluded (for now) to not clutter the library.
--- For the ones that don't exist, you can usually use `isTypeof` or `isType`.

local typePackCast: <Out...>(...any) -> Out... = function(...)
	return ...
end

--- Creates a new GreenTea Type that matches a repeating value.
function GreenTea.vararg<T>(
	type: T,
	options: {
		length: RangeInput?,
	}?
): ...T
	local lengthRange = options and options.length and parseRange(options.length)
	local varargType: Type = GreenTea.typeof(type :: any)

	local self: TypeRaw
	self = {
		kind = "vararg",
		vararg = {
			type = varargType,
			length = lengthRange,
		},
		_matches = function(...: any)
			local errs: { { type: any, input: any?, message: string? } } = {}
			local hasErrs = false

			local input = table.pack(...)
			for index = input.n, 1, -1 do
				if input[index] == nil then
					input.n -= 1
				else
					break
				end
			end

			if lengthRange and not checkRange(input.n, lengthRange) then
				table.insert(errs, {
					type = self,
					input = table.pack(...),
					message = `expected input count to be within range {displayRange(lengthRange)}`,
				})
				return Cause.new(false, errs)
			end

			if varargType.kind == "any" or varargType.kind == "unknown" then
				return Cause.ok()
			end

			for index = 1, input.n do
				local cause = varargType._matches(input[index])
				if not cause.ok then
					table.move(cause.errs, 1, #cause.errs, #errs + 1, errs)
					hasErrs = true
				end
			end
			if hasErrs then
				table.insert(errs, 1, { type = self, input = table.pack(...) })
				return Cause.new(false, errs)
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if recurse[self] then
				return "<cyclic>"
			end
			recurse[self] = true

			local result = `...{varargType._format(highlight, maxLineLength - 3, recurse)}`
			return highlightWrap(result, highlight[self])
		end,
	}
	return setmetatable(self :: TypeRaw, Type) :: any
end

--[[
	The final argument gets expanded if it's a tuple.
	All prior arguments are simplified to only the first value if they're a tuple or a vararg.
	This matches the Luau typechecker's behavior.
]]
local function simplifyGtTuples<T...>(...: T...): T...
	local args: { [number]: any, n: number } = table.pack(...)

	for index = 1, args.n do
		local value: any = args[index]
		if GreenTea.isGtType(value) then
			local value: Type = value
			if value.tuple then
				if index == args.n then
					args[index] = nil
					table.move(value.tuple.contents, 1, #value.tuple.contents, index, args)
					args.n += #value.tuple.contents - 1
					if value.tuple.vararg then
						table.insert(args, value.tuple.vararg)
						args.n += 1
					end
				else
					args[index] = value.tuple.contents[1] or value.tuple.vararg or GreenTea.none()
				end
			elseif value.vararg then
				if index ~= args.n then
					args[index] = value.vararg.type
				end
			end
		end
	end

	return typePackCast(unpack(args, 1, args.n))
end

--- Creates a new GreenTea Type that matches a tuple of values.\
--- Specify the final value with GreenTea.vararg to match a repeating final value.\
--- This properly accepts a tuple as the final or only argument, so don't fear
--- passing a tuple into this by accident.
function GreenTea.tuple<T...>(...: T...): T...
	local contents = table.pack(simplifyGtTuples(...))
	for index = 1, contents.n do
		if contents[index] == nil then
			error(
				"nil types are not allowed implicitly in tuples; specify explicitly or fix your arguments to not have nil"
			)
		end
		contents[index] = GreenTea.typeof(contents[index])
	end
	contents.n = nil :: any
	local contents: { Type } = contents :: any

	local vararg: Type?
	if contents[#contents] and contents[#contents].kind == "vararg" then
		vararg = contents[#contents]
		contents[#contents] = nil
	end

	local self: TypeRaw
	self = {
		kind = "tuple",
		tuple = {
			contents = contents,
			vararg = vararg,
		},
		_matches = function(...: any)
			local errs = {}
			local hasErrs = false

			local input = table.pack(...)
			for index = input.n, 1, -1 do
				if input[index] == nil then
					input.n -= 1
				else
					break
				end
			end
			for index, gtType in contents do
				local input = input[index]

				local cause = gtType._matches(input)
				if not cause.ok then
					table.move(cause.errs, 1, #cause.errs, #errs + 1, errs)
					hasErrs = true
				end
			end

			if hasErrs then
				table.insert(errs, 1, { type = self, input = newCauseTuple(...) })
				return Cause.new(false, errs)
			end

			if input.n <= #contents then
				return Cause.ok()
			end

			if not vararg then
				return Cause.errs({
					{ type = self, input = input[#contents + 1] },
					{
						type = self,
						input = newCauseTuple(...),
						message = `expected a tuple of {#contents} elements, got {input.n} elements from $input`,
					},
				})
			end

			return vararg._matches(select(#contents + 1, ...))
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if recurse[self] then
				return "<cyclic>"
			end
			recurse[self] = true

			local formatted = {}
			for index, expectedType in ipairs(contents) do
				local valueStr
				if expectedType._needsParens then
					valueStr = `({expectedType._format(highlight, maxLineLength - 4, recurse)})`
				else
					valueStr = expectedType._format(highlight, maxLineLength - 2, recurse)
				end
				table.insert(formatted, valueStr)
			end

			if vararg then
				table.insert(formatted, vararg._format(highlight, maxLineLength - 3, recurse))
			end

			local result = table.concat(formatted, ", ")
			if lineLengthOf(result) > maxLineLength or string.find(result, "\n", 1, true) then
				return highlightWrap(`(\n{tabFirst(table.concat(formatted, ",\n"))}\n)`, highlight[self])
			else
				return highlightWrap(`({table.concat(formatted, ", ")})`, highlight[self])
			end
		end,
	}

	return typePackCast(setmetatable(self :: any, Type))
end

--- Used for GreenTea.fn only. This is used to pass args to GreenTea.fn ergonomically.
function GreenTea.args<T...>(...: T...): (T...) -> ()
	return GreenTea.tuple(...) :: any
end

--- Used for GreenTea.fn only. This is used to pass returns to GreenTea.fn ergonomically.
function GreenTea.returns<T...>(...: T...): () -> T...
	return GreenTea.tuple(...) :: any
end

--- Creates a new GreenTea Type that matches a function.\
--- This cannot perform any runtime checks on the function's args or returns.
--- The args and returns exist primarily to have an accurate Luau type.\
--- They _can_ be inspected at runtime with `type.fn.args` and `type.fn.returns`.\
--- If you're building a module that consumes GreenTea types, you can use
--- `type.fn.args` and `type.fn.returns` to run runtime typechecking yourself.
--- For example, if you were building a RemoteFunction wrapper, you could use
--- `type.fn.returns` to check that the client returned correct values.
function GreenTea.fn<Args..., Returns...>(args: (Args...) -> (), returns: () -> Returns...): (Args...) -> Returns...
	-- The extra parens above are *IMPORTANT*. They prevent some weird behavior
	-- where Luau wants to make this function return `(Args...) -> Returns..., Args...`

	local args: Type = args :: any
	local returns: Type = returns :: any

	assert(GreenTea.isGtType(args), "args must be a GreenTea type. Use GreenTea.args to specify args.")
	assert(GreenTea.isGtType(returns), "returns must be a GreenTea type. Use GreenTea.returns returns.")

	assert(args.tuple, "args must be a GreenTea tuple type. Use GreenTea.args to specify args.")
	assert(returns.tuple, "returns must be a GreenTea tuple type. Use GreenTea.returns returns.")

	local self: TypeRaw
	self = {
		kind = "function",
		fn = {
			args = args,
			returns = returns,
		},
		_matches = function(input: any, ...: any)
			if typeof(input) ~= "function" then
				return Cause.err(self, input, "expected function")
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if recurse[self] then
				return "<cyclic>"
			end
			recurse[self] = true

			local argsFormatted = {}
			for index, argType in args.tuple.contents do
				table.insert(argsFormatted, argType._format(highlight, maxLineLength - 1, recurse))
			end
			if args.tuple.vararg then
				table.insert(argsFormatted, args.tuple.vararg._format(highlight, maxLineLength - 1, recurse))
			end

			local returnsFormatted = {}
			for index, returnType in returns.tuple.contents do
				table.insert(returnsFormatted, returnType._format(highlight, maxLineLength - 1, recurse))
			end
			if returns.tuple.vararg then
				table.insert(returnsFormatted, returns.tuple.vararg._format(highlight, maxLineLength - 1, recurse))
			end

			local argsStr, returnsStr

			if #argsFormatted == 0 then
				argsStr = "() ->"
			else
				argsStr = `({table.concat(argsFormatted, ", ")}) ->`
				if lineLengthOf(argsStr) > SOFT_MAX_LINE_LENGTH or string.find(argsStr, "\n", 1, true) then
					argsStr = `(\n{tabFirst(table.concat(argsFormatted, ",\n"))}\n) ->`
				end
			end

			local finalItem = returns.tuple.vararg or returns.tuple.contents[#returns.tuple.contents]

			if #returnsFormatted == 0 then
				returnsStr = "()"
			elseif #returnsFormatted == 1 and not finalItem.__needsParens then
				returnsStr = returnsFormatted[1]
			else
				returnsStr = `({table.concat(returnsFormatted, ", ")})`
				if lineLengthOf(returnsStr) > SOFT_MAX_LINE_LENGTH or string.find(returnsStr, "\n", 1, true) then
					returnsStr = tabSecond(`({table.concat(returnsFormatted, ",\n")})`)
				end
			end

			local result = `{argsStr} {returnsStr}`
			if lineLengthOf(result) > maxLineLength or string.find(result, "\n", 1, true) then
				return highlightWrap(`{argsStr}\n{tabFirst(returnsStr)}`, highlight[self])
			else
				return highlightWrap(result, highlight[self])
			end
		end,
	}
	return setmetatable(self :: any, Type) :: any
end

--- Creates a new GreenTea Type that matches a function with any args and any returns.
function GreenTea.anyfn(): (...any) -> ...any
	return GreenTea.fn(
		GreenTea.args(GreenTea.vararg(GreenTea.any({ allowNil = true }))),
		GreenTea.returns(GreenTea.vararg(GreenTea.any({ allowNil = true })))
	) :: any
end

--- Creates a new GreenTea Type that "smuggles" a tuple as a single value by
--- packing it into the returns.\
--- This is useful for accepting GreenTea tuple types to a spot that only accepts a single value.
--- The actual returned type here is equivalent to
--- `GreenTea.fn(GreenTea.args(), GreenTea.returns(...))`
--- so some inspections is required at runtime to pull out the tuple:
--- `type.fn.returns` is the actual tuple value.\
--- With Luau typechecking, you can "unsmuggle" the tuple by doing
--- `typeof(smuggledTuple())` to get the tuple type.
function GreenTea.tuplePacked<T...>(...: T...): () -> T...
	return GreenTea.fn(GreenTea.args(), GreenTea.returns(...)) :: any
end

export type TuplePacked<T...> = () -> T...

--- Creates a new GreenTea Type that matches a table.\
--- The `contents` table should be a dictionary of string keys to types.\
--- An indexer can be provided with `{ [keyGreenTeaType] = valueType }`.\
--- This currently respects both `__iter` and `__index` on both the input
--- `contents` table and tables being checked against.\
--- Tables are non-strict by default. Extra values are allowed. If you'd
--- like to make a table strict, add an indexer that denies extra values, like
--- `[gt.any()] = gt.never()`.\
--- By default, array-like definitions do not check for holes. If you would like
--- to check for holes and ensure the array is contiguous, set array to true.\
--- If you'd like to enforce an item limit, specify it `count` the options table. This limits
--- the number of items found by the indexer. Items in the "struct portion" of the
--- table definition do not count. This is _not_ a #table check: the actual items
--- are counted one-by-one as we check them. This lets you also limit the number of items
--- in cases like dynamic dictionaries.\
--- `raw` allows for checking any key types as literally matching to values, including numbers
--- and other non-strings. This _will not_ appear correct as a Luau type; it is primarily
--- intended for compatibility with other runtime typechecking libraries.
function GreenTea.table<T>(
	contents: T,
	options: {
		array: boolean?,
		count: RangeInput?,
		raw: boolean?,
	}?
): T
	local array: boolean? = options and options.array or nil
	local countRange = if options and options.count then parseRange(options.count) else nil

	local isRaw = options and options.raw or nil

	-- note: pairs is used to avaid the __iter metamethod
	local indexerKey: Type?
	local indexerValue: Type?
	local indexerValues: { Type } = {}
	local rawContents = {}
	for key, value in contents :: any do
		if typeof(key) == "table" and GreenTea.isGtType(key) then
			if indexerKey then
				error("Only one indexer can be specified")
			else
				indexerKey = GreenTea.typeof(key)
				indexerValue = GreenTea.typeof(value)
			end
		elseif isRaw then
			rawContents[key] = GreenTea.typeof(value)
		else
			if typeof(key) == "number" then
				if indexerKey then
					error("Only one indexer can be specified")
				end
				indexerKey = GreenTea.number() :: any
				table.insert(indexerValues, GreenTea.typeof(value))
			elseif typeof(key) == "string" then
				rawContents[key] = GreenTea.typeof(value)
			else
				error("Tables must be defined as arrays or dictionaries with string keys")
			end
		end
	end

	if #indexerValues > 0 then
		if #indexerValues == 1 then
			indexerValue = indexerValues[1]
		else
			-- NOTE: this case never happens presently due to the "Only one indexer can be specified" checks.
			indexerValue = (GreenTea :: any).union(table.unpack(indexerValues))
		end
	end

	if array and (not indexerKey or indexerKey.kind ~= "number") then
		error("If array is true, the table must have an indexer with number keys")
	end

	local self: TypeRaw
	self = {
		kind = "table",
		table = {
			contents = rawContents,
			indexer = indexerKey and {
				key = indexerKey,
				value = indexerValue,
			} or nil,

			array = array :: boolean?,
			count = countRange,

			raw = isRaw,
		},
		_matches = function(input: any, ...: any)
			if typeof(input) ~= "table" then
				return Cause.err(self, input, "expected table")
			end

			local checked = {}

			local maxN = 0
			local count = 0

			local cause

			for key, expectedType in rawContents do
				local value = input[key]

				cause = expectedType._matches(value)
				if not cause.ok then
					return Cause.extendErr(cause, self, input)
				end

				checked[key] = true
			end

			for key, value in input do
				if checked[key] then
					continue
				end

				local expectedType = rawContents[key]
				if expectedType then
					cause = expectedType._matches(value)
					if not cause.ok then
						return Cause.extendErr(cause, self, input)
					end
				else
					if indexerKey then
						assert(indexerValue, "analysis hint")

						count += 1
						if countRange and not checkRange(count, countRange) then
							return Cause.err(
								self,
								input,
								`expected number of items to be in range {displayRange(countRange)}, but we saw {count} (or more) items`
							)
						end
						cause = indexerKey._matches(key)
						if not cause.ok then
							return Cause.extendErr(cause, self, input)
						end
						cause = indexerValue._matches(value)
						if not cause.ok then
							return Cause.extendErr(cause, self, input)
						end
						if typeof(key) == "number" then
							maxN = math.max(maxN, key)
							if array then
								if key < 1 then
									return Cause.err(
										self,
										input,
										`key {key} is less than 1, but we expected a contiguous array`
									)
								elseif math.floor(key) ~= key then
									return Cause.err(
										self,
										input,
										`key {key} is not an integer, but we expected a contiguous array`
									)
								elseif key ~= key then
									return Cause.err(
										self,
										input,
										`key {key} is NaN, but we expected a contiguous array`
									)
								end
							end
						end
					end
				end
			end

			if countRange and not checkRange(count, countRange) then
				return Cause.err(
					self,
					input,
					`expected number of items to be in range {displayRange(countRange)}, but we saw only {count} items`
				)
			end

			if array and count ~= maxN then
				assert(indexerValue, "analysis hint")

				cause = indexerValue._matches(nil)
				if not cause.ok then
					return Cause.extendErr(
						cause,
						self,
						input,
						`expected contiguous array, but we saw only {count} items when the max index was {maxN}`
					)
				end
			end

			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if recurse[self] then
				return "<cyclic>"
			end
			recurse[self] = true

			local items = {}
			if array then
				table.insert(items, `@array`)
			end
			if countRange then
				table.insert(items, `@count {displayRange(countRange)}`)
			end
			if indexerKey then
				assert(indexerValue, "analysis hint")
				if indexerKey.kind == "number" then
					table.insert(items, indexerValue._format(highlight, maxLineLength - 1, recurse))
				else
					local keyStr = indexerKey._format(highlight, maxLineLength - 3, recurse)
					local valueStr = indexerValue._format(highlight, maxLineLength - 1, recurse)
					local indexerStr = `[{keyStr}]: {valueStr}`
					if lineLengthOf(indexerStr) > maxLineLength then
						indexerStr = `[{keyStr}]:\n{valueStr}`
					end
					table.insert(items, indexerStr)
				end
			end
			for key: any, value in rawContents do
				local keyStr = tostring(key)
				local valueStr = value._format(highlight, maxLineLength - 3, recurse)
				local itemStr = `{keyStr}: {valueStr}`
				if lineLengthOf(itemStr) > maxLineLength then
					itemStr = `{keyStr}: {valueStr}`
				end
				table.insert(items, itemStr)
			end

			if #items == 0 then
				return highlightWrap("{}", highlight[self])
			end

			local leftCurly, rightCurly = "{", "}"

			local result = `{leftCurly} {table.concat(items, ", ")} {rightCurly}`
			if lineLengthOf(result) > maxLineLength or string.find(result, "\n", 1, true) then
				return highlightWrap(
					`{leftCurly}\n{tabFirst(table.concat(items, ",\n"))}\n{rightCurly}`,
					highlight[self]
				)
			else
				return highlightWrap(result, highlight[self])
			end
		end,
	}
	return setmetatable(self, Type) :: any
end

GreenTea.struct = GreenTea.table

function GreenTea.anyTable(options: {
	count: RangeInput?,
}?)
	return GreenTea.table({ [GreenTea.any()] = GreenTea.any() }, {
		count = options and options.count,
	})
end

--- Creates a new GreenTea Type that matches an array of values.\
--- Convenience function that calls GreenTea.table internally.
function GreenTea.array<T>(
	value: T,
	options: {
		count: RangeInput?,
	}?
): { T }
	return GreenTea.table({ GreenTea.typeof(value) }, {
		array = true,
		count = options and options.count,
	})
end

--- Creates a new GreenTea Type that matches a dictionary of values.\
--- Convenience function that calls GreenTea.table internally.
function GreenTea.dictionary<K, V>(
	key: K,
	value: V,
	options: {
		count: RangeInput?,
	}?
): { [K]: V }
	return GreenTea.table({ [GreenTea.typeof(key)] = GreenTea.typeof(value) }, {
		count = options and options.count,
	})
end

type UnionType =
	(<T1, T2>(T1, T2) -> T1 | T2)
	& (<T1, T2, T3>(T1, T2, T3) -> T1 | T2 | T3)
	& (<T1, T2, T3, T4>(T1, T2, T3, T4) -> T1 | T2 | T3 | T4)
	& (<T1, T2, T3, T4, T5>(T1, T2, T3, T4, T5) -> T1 | T2 | T3 | T4 | T5)
	& (<T>(...T) -> T)

local union: UnionType = function<T>(...: T): ...T
	local types: { [number]: any, n: number } = table.pack(...)
	for index = types.n, 1, -1 do
		if types[index] == nil then
			types.n -= 1
		else
			break
		end
	end

	local nilTypes, nonnilTypes = {}, {}
	for index = 1, types.n do
		if types[index] == nil then
			error("implicit nil type not allowed in union; specify explicitly or fix your arguments to not have nil")
		end
		local value: Type = GreenTea.typeof(types[index])
		types[index] = value
		if value.basic and (value.basic.type == "nil" or value.basic.typeof == "nil") then
			table.insert(nilTypes, value)
		else
			table.insert(nonnilTypes, value)
		end
	end
	local types: { any } = types;
	(types :: any).n = nil

	assert(#types > 0, "union must have at least one type")

	local self: TypeRaw
	self = {
		kind = "union",
		union = {
			contents = types,
			optional = #nilTypes > 0,
		},
		_needsParens = true,
		_matches = function(input: any, ...: any)
			local errs = {}
			for k, expectedType: any in types do
				local cause = expectedType._matches(input)
				if cause.ok then
					return cause
				else
					table.move(cause.errs, 1, #cause.errs, #errs + 1, errs)
				end
			end
			table.insert(errs, 1, { type = self, input = input, message = "input did not match any union member" })
			return Cause.errs(errs)
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if recurse[self] then
				return "<cyclic>"
			end
			recurse[self] = true

			if #nonnilTypes == 0 then
				return "nil"
			elseif #nonnilTypes == 1 then
				if #nilTypes > 0 then
					local optional = nonnilTypes[1]

					local result
					if optional._needsParens then
						result = `({optional._format(highlight, maxLineLength - 3, recurse)})?`
					else
						result = `{optional._format(highlight, maxLineLength - 1, recurse)}?`
					end

					return highlightWrap(result, highlight[self])
				else
					return highlightWrap(nonnilTypes[1]._format(highlight, maxLineLength, recurse), highlight[self])
				end
			end

			local formatted = {}
			for _, expectedType: any in types do
				local valueStr
				if expectedType._needsParens then
					valueStr = `({expectedType._format(highlight, maxLineLength - 4, recurse)})`
				else
					valueStr = expectedType._format(highlight, maxLineLength - 2, recurse)
				end
				table.insert(formatted, valueStr)
			end

			local result = table.concat(formatted, " | ")
			if lineLengthOf(result) > maxLineLength or string.find(result, "\n", 1, true) then
				return highlightWrap(
					formatted[1] .. tabFirst("\n| " .. table.concat(formatted, "\n| ", 2)),
					highlight[self]
				)
			else
				return highlightWrap(result, highlight[self])
			end
		end,
	}
	return setmetatable(self, Type) :: any
end :: any

--- @function union
--- @within GreenTea
--- @param ... T...
--- @return T...
--- Creates a new GreenTea Type that matches a union of types.\
--- This is analogous to the Luau `type1 | type2 | ...` syntax.\
--- This has convenient type definitions for up to 5 input types.\
--- When specifying more then 5 input types, you will have to typecast the
--- first item to a Luau type union in order to get correct types.

GreenTea.union = union

type IntersectionType =
	(<T1, T2>(T1, T2) -> T1 & T2)
	& (<T1, T2, T3>(T1, T2, T3) -> T1 & T2 & T3)
	& (<T1, T2, T3, T4>(T1, T2, T3, T4) -> T1 & T2 & T3 & T4)
	& (<T1, T2, T3, T4, T5>(T1, T2, T3, T4, T5) -> T1 & T2 & T3 & T4 & T5)
	& (<T>(...T) -> T)

local intersection: IntersectionType = function<T>(...: T): ...T
	local types = table.pack(...)
	for index = types.n, 1, -1 do
		if types[index] == nil then
			types.n -= 1
		else
			break
		end
	end

	for index = 1, types.n do
		if types[index] == nil then
			error(
				"implicit nil type not allowed in intersection; specify explicitly or fix your arguments to not have nil"
			)
		end
		types[index] = GreenTea.typeof(types[index])
	end
	local types: { any } = types;
	(types :: any).n = nil

	assert(#types > 0, "intersection must have at least one type")

	local self: TypeRaw
	self = {
		kind = "intersection",
		intersection = {
			contents = types,
		},
		_needsParens = true,
		_matches = function(input: any, ...: any)
			local errs = {}
			local hasErrs = false
			for _, expectedType: any in types do
				local cause = expectedType._matches(input)
				if not cause.ok then
					hasErrs = true
					table.move(cause.errs, 1, #cause.errs, #errs + 1, errs)
				end
			end
			if hasErrs then
				table.insert(
					errs,
					1,
					{ type = self, input = input, message = "input did not match all intersection members" }
				)
				return Cause.errs(errs)
			end
			return Cause.ok()
		end,
		_format = function(highlight: { [any]: string }, maxLineLength: number, recurse: { [any]: any })
			if recurse[self] then
				return "<cyclic>"
			end
			recurse[self] = true

			if #types == 0 then
				return "()"
			elseif #types == 1 then
				return highlightWrap(types[1]._format(highlight, maxLineLength, recurse), highlight[self])
			end

			local formatted = {}
			for _, expectedType: any in types do
				local valueStr
				if expectedType._needsParens then
					valueStr = `({expectedType._format(highlight, maxLineLength - 4, recurse)})`
				else
					valueStr = expectedType._format(highlight, maxLineLength - 2, recurse)
				end
				table.insert(formatted, valueStr)
			end

			local result = table.concat(formatted, " & ")
			if lineLengthOf(result) > maxLineLength or string.find(result, "\n", 1, true) then
				return highlightWrap(
					formatted[1] .. tabFirst("\n& " .. table.concat(formatted, "\n& ", 2)),
					highlight[self]
				)
			else
				return highlightWrap(result, highlight[self])
			end
		end,
	}
	return setmetatable(self, Type) :: any
end :: any

--- @function intersection
--- @within GreenTea
--- @param ... T...
--- @return T...
--- Creates a new GreenTea Type that matches an intersection of types.\
--- This is analogous to the Luau `type1 & type2 & ...` syntax.\
--- This has convenient type definitions for up to 5 input types.\
--- When specifying more then 5 input types, you will have to typecast the
--- first item to a Luau type intersection in order to get correct types.

GreenTea.intersection = intersection

--- Creates a new GreenTea Type that makes a value optionally,
--- like `type?` or `type | nil` in Luau types.
function GreenTea.optional<T>(value: T): T?
	return GreenTea.union(value :: T?, GreenTea.none()) :: any
end

GreenTea.oneOf = GreenTea.union

GreenTea.allOf = GreenTea.intersection

GreenTea.opt = GreenTea.optional

--- Creates a new GreenTea Type matching the inferred type of `value`.\
--- A runtime, this passes tables to `GreenTea.table`, functions to `GreenTea.fn`, and so on.\
--- This results in a runtime-inferred type roughly equivalent to the Luau typechecker's inferred types.
function GreenTea.typeof<T>(value: T, recursionCheck: { [any]: any }?): T
	if GreenTea.isGtType(value) then
		return value
	end
	if greenteaConstructorsSet[value] then
		local name = greenteaConstructorsSet[value]
		error(
			`Attempt to use a GreenTea constructor without calling it: you used {name}; did you mean to use {name}() instead?`
		)
	end

	local recursionCheck = recursionCheck or {} :: typeof(assert(recursionCheck))

	if typeof(value) == "table" then
		local newTable = {}
		for key, subValue in pairs(value :: any) do
			if recursionCheck[subValue] then
				newTable[key] = recursionCheck[subValue]
			end
			newTable[key] = GreenTea.typeof(subValue, recursionCheck)
		end
		if typeof(getmetatable(value)) == "table" then
			setmetatable(newTable :: any, getmetatable(value) :: any)
		end
		return GreenTea.table(newTable) :: any
	elseif typeof(value) == "function" then
		return GreenTea.anyfn() :: any
	elseif typeof(value) == "string" then
		return GreenTea.string() :: any
	elseif typeof(value) == "number" then
		return GreenTea.number() :: any
	end

	return GreenTea.isTypeof(typeof(value), value) :: any
end

--- Typecasts a value to a GreenTea type.\
--- At runtime this will error if the passed-in type is _not_ a GreenTea type.\
--- This exists as a convenience function because `type :: GreenTea.Type` is usually
--- not possible without an any cast like `(type :: any) :: GreenTea.Type`.\
--- \
--- **Usually, you'll want to use `GreenTea.build` when making GreenTea types.**
--- _`GreenTea.typecast` is specifically made for when you're doing tricky stuff with GreenTea types._
function GreenTea.typecast(value: any): Type
	assert(GreenTea.isGtType(value), "value must be a GreenTea type")

	return value
end

GreenTea.asGreenTeaType = GreenTea.typecast
GreenTea.asGtType = GreenTea.typecast

local function castTuple<T...>(...: any): T...
	return ...
end

-- This whole bit of acrobatics around this BuiltType table only exists so that
-- Luau LSP picks up the docs for the `:type()` method.
local BuiltType = {}

-- We use a different comment style here so that moonwave does not pick up this
-- comment and complain about that lack of BuiltType class definition.
--[[
In type definitions via `typeof(Type.type())`, returns the Luau type this
GreenTea type represents.\
At runtime, returns self. This allows you to compose complex GreenTea types
by passing GreenTea.type() into a GreenTea constructor.
]]
function BuiltType.type<T...>(self: { __getType: () -> T... }): T...
	return castTuple(self)
end

export type BuiltType<T... = ...any> = Type & { __getType: () -> T... } & typeof(BuiltType)

--- Typecasts a value to be a GreenTea type, with the type stored under the `type`
--- key so you can get the type with `typeof(result.type())`\
--- \
--- **This is the preferred way to build a GreenTea type for typical use.**
function GreenTea.build<T...>(...: T...): BuiltType<T...>
	local gtType: any

	local isTuple = select("#", ...) > 1

	-- Only treat it as a tuple if there are actually multiple values
	if isTuple then
		isTuple = false
		for index = 2, select("#", ...) do
			if select(index, ...) ~= nil then
				isTuple = true
				break
			end
		end
	end

	if isTuple then
		gtType = GreenTea.tuple(...)
	else
		gtType = GreenTea.typeof((castTuple(...)))
	end

	gtType.type = function(self: Type)
		return gtType
	end
	return gtType
end

--- Adds user-specified metadata to a type.\
--- Each key/value pair is added to the type's `meta` table.
--- This is a shallow merge: any matching key/value pair will be overwritten,
--- and all other key/value pairs will be kept as-is.
function GreenTea.meta<T>(gtType: T, meta: { [string]: any }): T
	local gtType: Type = GreenTea.typeof(gtType) :: any

	gtType.meta = gtType.meta or {}
	assert(gtType.meta, "analysis hint")

	for key, value in meta do
		gtType.meta[key] = value
	end

	return gtType :: any
end

--- Wraps a function with type asserts for its input args and returns.\
--- For example:
--- ```lua
--- gt.wrapFn(
---     gt.fn(
---         gt.args(gt.string(), gt.number()),
---         gt.returns(gt.number())
---     ),
---     function(arg1, arg2)
---         --- Luau will properly infer arg types via above definition
---         return 5
---     end
--- )
--- ```
--- Notes:
--- * If you've stored the GreenTea type in a variable, and "built" it with `GreenTea.build`,
---   then you will need to use `gtType:type()` here:
---   ```lua
---   local gtType = GreenTea.build(gt.string(), gt.number())
---   gt.wrapFn(gtType:type(), function(arg1, arg2)
---       return 5
---   end)
---   ```
--- * If you're trying to add asserts to an unknown or dynamically-provided function, you may need to cast it to `any` first:
---   ```lua
---   gt.wrapFn(
---       gt.fn(
---           gt.args(gt.string(), gt.number()),
---           gt.returns(gt.number())
---       ),
---       someFunction :: any
---   )
---   ```
function GreenTea.wrapFn<T>(gtType: T, fn: T): T
	local gtType: Type = GreenTea.typeof(gtType) :: any

	assert(typeof(fn) == "function", "fn must be a function")
	assert(gtType.fn ~= nil, "gtType must be a GreenTea.fn type")

	return function(...: any)
		gtType.fn.args:assert(...)

		return gtType.fn.returns:assert(fn(...))
	end :: any
end

--- Wraps a function with type asserts for its input args only.\
--- See `GreenTea.wrapFn` for more information.
function GreenTea.wrapFnArgs<T>(gtType: T, fn: T): T
	local gtType: Type = GreenTea.typeof(gtType) :: any

	assert(typeof(fn) == "function", "fn must be a function")
	assert(gtType.fn ~= nil, "gtType must be a GreenTea.fn type")

	return function(...: any)
		gtType.fn.args:assert(...)

		return fn(...)
	end :: any
end

--- Wraps a function with type asserts for its returns only.\
--- See `GreenTea.wrapFn` for more information.
function GreenTea.wrapFnReturns<T>(gtType: T, fn: T): T
	local gtType: Type = GreenTea.typeof(gtType) :: any

	assert(typeof(fn) == "function", "fn must be a function")
	assert(gtType.fn ~= nil, "gtType must be a GreenTea.fn type")

	return function(...: any)
		return gtType.fn.returns:assert(fn(...))
	end :: any
end

return GreenTea

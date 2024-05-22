local JestGlobals = require(script.Parent.Parent.DevPackages.JestGlobals)
local expectRaw = JestGlobals.expect
local describe = JestGlobals.describe
local it = JestGlobals.it

local expect: typeof(expectRaw) = function(input: any): any
	-- remove 2nd+ args which would otherwise be implicitly passed
	return expectRaw(input)
end :: any

local gt: any = require(script.Parent.Parent.GreenTea)

local anyValuesTable: { any } = {
	{ value = "hello", name = "string 'hello'" },
	{ value = 5, name = "number 5" },
	{ value = true, name = "boolean true" },
	{ value = false, name = "boolean false" },
	{ value = nil, name = "nil" },
	{ value = {}, name = "empty table" },
	{ value = { foo = "bar" }, name = "foobar table" },
	{ value = Instance.new("Folder"), name = "Folder Instance" },
	{ value = CFrame.new(), name = "CFrame" },
	{ value = newproxy(true), name = "newproxy(true)" },
	{ value = function() end, name = "function" },
}

local function anyValues(): () -> (string, any)
	return coroutine.wrap(function()
		for index, value in anyValuesTable do
			coroutine.yield(value.name, value.value)
		end
	end)
end

local function oneNil(): () -> (any, nil)
	return coroutine.wrap(function()
		coroutine.yield(1, nil)
	end)
end

describe("gt.isGtType", function()
	it("should return true for GreenTea Types and false otherwise", function()
		expect(gt.isGtType(gt.string())).toBe(true)

		expect(gt.isGtType({})).toBe(false)
		expect(gt.isGtType(nil)).toBe(false)
	end)
end)

describe("gt.any", function()
	for name, value in anyValues() do
		if value == nil then
			it(`should match value {name}`, function()
				expect(gt.any({ allowNil = true }):matches(value)).toBe(true)
				expect(gt.any({ allowNil = false }):matches(value)).toBe(false)
				expect(gt.any():matches(value)).toBe(false)
			end)
			continue
		end
		it(`should match value {name}`, function()
			expect(gt.any():matches(value)).toBe(true)
		end)
	end

	it("should format as 'any'", function()
		expect(gt.any():format()).toBe("any")
	end)

	it("should have a kind of 'any'", function()
		expect(gt.any().kind).toBe("any")
	end)
end)

describe("gt.unknown", function()
	for name, value in anyValues() do
		if value == nil then
			it(`should match value {name}`, function()
				expect(gt.unknown({ allowNil = true }):matches(value)).toBe(true)
				expect(gt.unknown({ allowNil = false }):matches(value)).toBe(false)
				expect(gt.unknown():matches(value)).toBe(false)
			end)
			continue
		end
		it(`should match value {name}`, function()
			expect(gt.unknown():matches(value)).toBe(true)
		end)
	end

	it("should format as 'unknown'", function()
		expect(gt.unknown():format()).toBe("unknown")
	end)

	it("should have a kind of 'unknown'", function()
		expect(gt.unknown().kind).toBe("unknown")
	end)
end)

describe("gt.never", function()
	for name, value in anyValues() do
		it(`should not match value {name}`, function()
			expect(gt.never():matches(value)).toBe(false)
		end)
	end

	it("should format as 'never'", function()
		expect(gt.never():format()).toBe("never")
	end)

	it("should have a kind of 'never'", function()
		expect(gt.never().kind).toBe("never")
	end)
end)

local function basicTest(options: {
	kind: string,
	fn: () -> any,
	alternatives: { [string]: any }?,
	values: { any } | () -> (any, any),
	formatsAs: string?,
	describeName: string?,
	skipBasicTypeCheck: boolean?,
})
	assert(typeof(options.fn) == "function", "not fn\n" .. debug.traceback())
	assert(typeof(options.fn()) == "table", "not table\n" .. debug.traceback())

	describe(options.describeName or `gt.{options.kind}`, function()
		for _, value in options.values :: any do
			it(`should match {value}`, function()
				expect(options.fn():matches(value)).toBe(true)
			end)
		end
		it("should not match other types", function()
			for name, value in anyValues() do
				if typeof(value) ~= options.kind then
					expect(options.fn():matches(value)).toBe(false)
				end
			end
		end)

		for name, value in options.alternatives or {} :: typeof(assert(options.alternatives)) do
			it(`should be the same as {name}`, function()
				expect(options.fn).toBe(value)
			end)
		end

		it(`should format as '{options.formatsAs or options.kind}'`, function()
			expect(options.fn():format()).toBe(options.formatsAs or options.kind)
		end)

		if not options.skipBasicTypeCheck then
			it(`should have a kind of 'basic' and a .basic.typeof of '{options.kind}'`, function()
				local basic = options.fn()
				expect(basic.kind).toBe("basic")
				expect(basic.basic.typeof).toBe(options.kind)
			end)
		end
	end)
end

basicTest({
	kind = "boolean",
	fn = gt.boolean,
	alternatives = { bool = gt.bool },
	values = { true, false },
})

local function basicBasicTest(name, value)
	basicTest({
		kind = name,
		fn = gt[name],
		values = { value },
	})

	basicTest({
		kind = name,
		fn = function()
			return gt.isTypeof(name)
		end,
		values = { value },
		describeName = `gt.basic({name})`,
	})
end

basicBasicTest("Instance", Instance.new("Folder"))
basicBasicTest("Vector2", Vector2.new())
basicBasicTest("Vector3", Vector3.new())
basicBasicTest("CFrame", CFrame.new())
basicBasicTest("Color3", Color3.new())
basicBasicTest("UDim", UDim.new())
basicBasicTest("UDim2", UDim2.new())
basicBasicTest("Ray", Ray.new(Vector3.new(0, 0, 0), Vector3.new(0, 0, 1)))
basicBasicTest("Rect", Rect.new())
basicBasicTest("Region3", Region3.new(Vector3.new(0, 0, 0), Vector3.new(1, 1, 1)))
basicBasicTest("BrickColor", BrickColor.new("Bright blue"))

basicTest({
	kind = "nil",
	fn = gt.none,
	values = oneNil(),
	describeName = "gt.none",
})

local literalValues: { { value: any, formats: string } } = {
	{ value = "hello", formats = '"hello"' },
	{ value = 5, formats = "literal<5>" },
	{ value = nil, formats = "literal<nil>" },
	{ value = true, formats = "literal<true>" },
}

describe("gt.literal", function()
	for _, value in literalValues do
		it(`should match {value.value} for {value.value}`, function()
			expect(gt.literal(value.value):matches(value.value)).toBe(true)
			expect(gt.literal(value.value):matches("anything else")).toBe(false)
			if value.value ~= nil then
				expect(gt.literal(value.value):matches(nil)).toBe(false)
			end
		end)
		it(`should format as '{value.formats}' for {value.value}`, function()
			expect(gt.literal(value.value):format()).toBe(value.formats)
		end)
		it(`should have a .literal.value of {value.value}`, function()
			expect(gt.literal(value.value).literal.value).toBe(value.value)
		end)
	end
end)

describe("gt.custom", function()
	it("should format as 'custom<name here>' if given a name", function()
		local namedCustom = gt.custom(function() end, "name here")
		expect(namedCustom:format()).toBe("custom<name here>")
		expect(namedCustom.custom.name).toBe("name here")
	end)
	it("should format as 'custom<source:line>' if not given a name", function()
		local unnamedCustom = gt.custom(function() end)
		expect(unnamedCustom:format()).toMatch("custom<*.*%.spec:%d+>")
		expect(unnamedCustom.custom.name).never.toBe(nil)
	end)
	it("should format as 'custom<source:line>' if not given a name and fn is named", function()
		local function fnName() end
		local unnamedCustom = gt.custom(fnName)
		expect(unnamedCustom:format()).toMatch("custom<fnName>")
		expect(unnamedCustom.custom.name).never.toBe(nil)
	end)
	it("should call the custom typechecker with the value", function()
		local calls = 0
		local function typechecker(value)
			expect(value).toBe(5)
			calls += 1
			return true
		end

		local custom = gt.custom(typechecker)
		expect(custom:matches(5)).toBe(true)
		expect(calls).toBe(1)
	end)
	it("should fail if the typechecker fails (with message)", function()
		local custom = gt.custom(function(value)
			return false, "nope"
		end)
		local ok: boolean, cause: gt.Cause = custom:matches(5)
		expect(ok).toBe(false)
		expect(cause:formatErr()).toMatch("nope")
	end)

	it("should have a kind of 'custom'", function()
		local custom = gt.custom(function() end)
		expect(custom.kind).toBe("custom")
	end)
end)

describe("gt.isA indexed", function()
	it("should match the typename of the indexer and classes inherited from it", function()
		expect(gt.isA.Part():matches(Instance.new("Part"))).toBe(true)
		expect(gt.isA.BasePart():matches(Instance.new("Part"))).toBe(true)
		expect(gt.isA.Part():matches(Instance.new("Folder"))).toBe(false)
		expect(gt.isA.Part():matches("hi")).toBe(false)
		expect(gt.isA.Part():matches(nil)).toBe(false)
	end)

	it("should format as 'classname'", function()
		expect(gt.isA.Part():format()).toBe("Part")
	end)
end)

describe("gt.isA called", function()
	it("should match the typename of the indexer and classes inherited from it", function()
		expect(gt.isA("Part"):matches(Instance.new("Part"))).toBe(true)
		expect(gt.isA("BasePart"):matches(Instance.new("Part"))).toBe(true)
		expect(gt.isA("Part"):matches(Instance.new("Folder"))).toBe(false)
		expect(gt.isA("Part"):matches("hi")).toBe(false)
		expect(gt.isA("Part"):matches(nil)).toBe(false)
	end)

	it("should format as 'classname'", function()
		expect(gt.isA("Part"):format()).toBe("Part")
	end)
end)

basicTest({
	kind = "number",
	fn = gt.number,
	values = {
		5, 5.5, -5.5, 0, math.huge, -math.huge,
	},
	skipBasicTypeCheck = true,
})

describe("gt.number limits", function()
	it("should reject NaN by default", function()
		expect(gt.number():matches(0 / 0)).toBe(false)
	end)
	it("should allow NaN only if .nan = true", function()
		expect(gt.number():matches(0 / 0)).toBe(false)
		expect(gt.number({ nan = false }):matches(0 / 0)).toBe(false)
		expect(gt.number({ nan = true }):matches(0 / 0)).toBe(true)
	end)
	it("should reject non-integers when the integer flag is set", function()
		expect(gt.number():matches(5.5)).toBe(true)
		expect(gt.number({ integer = false }):matches(5.5)).toBe(true)
		expect(gt.number({ integer = true }):matches(5.5)).toBe(false)
		expect(gt.number({ integer = true }):matches(5)).toBe(true)
	end)
	it("should respect range when set", function()
		expect(gt.number():matches(5)).toBe(true)
		expect(gt.number({ range = "[0, 10]" }):matches(5)).toBe(true)
		expect(gt.number({ range = "[0, 10]" }):matches(10)).toBe(true)
		expect(gt.number({ range = "[0, 10]" }):matches(0)).toBe(true)
		expect(gt.number({ range = "[0, 10]" }):matches(-1)).toBe(false)
		expect(gt.number({ range = "[0, 10]" }):matches(11)).toBe(false)
		expect(gt.number({ range = "[0, 10]" }):matches(-math.huge)).toBe(false)
		expect(gt.number({ range = "[0, 10]" }):matches(math.huge)).toBe(false)

		expect(gt.number({ range = "(0, 10)" }):matches(5)).toBe(true)
		expect(gt.number({ range = "(0, 10)" }):matches(10)).toBe(false)
		expect(gt.number({ range = "(0, 10)" }):matches(0)).toBe(false)
		expect(gt.number({ range = "(0, 10)" }):matches(-1)).toBe(false)
		expect(gt.number({ range = "(0, 10)" }):matches(11)).toBe(false)
		expect(gt.number({ range = "(0, 10)" }):matches(-math.huge)).toBe(false)
		expect(gt.number({ range = "(0, 10)" }):matches(math.huge)).toBe(false)
	end)

	it("should format with limits if set", function()
		expect(gt.number():format()).toBe("number")

		expect(gt.number({ range = "[0, 10]" }):format()).toBe("number<range [0, 10]>")
		expect(gt.number({ range = "(0, 10)" }):format()).toBe("number<range (0, 10)>")
		expect(gt.number({ integer = true }):format()).toBe("number<integer>")
		expect(gt.number({ nan = true }):format()).toBe("number<NaN allowed>")

		expect(gt.number({ range = "(0, 10)", integer = true, nan = true }):format()).toBe(
			"number<integer, NaN allowed, range (0, 10)>"
		)
	end)
end)

basicTest({
	kind = "string",
	fn = gt.string,
	values = {
		"hi", "hello world", "",
	},
	skipBasicTypeCheck = true,
})

local utf8Strings: { [string]: string } = {
	["Valid ASCII"] = "a",
	["Valid 2 Octet Sequence"] = "\xc3\xb1",
	["Invalid 2 Octet Sequence"] = "\xc3\x28",
	["Invalid Sequence Identifier"] = "\xa0\xa1",
	["Valid 3 Octet Sequence"] = "\xe2\x82\xa1",
	["Invalid 3 Octet Sequence (in 2nd Octet)"] = "\xe2\x28\xa1",
	["Invalid 3 Octet Sequence (in 3rd Octet)"] = "\xe2\x82\x28",
	["Valid 4 Octet Sequence"] = "\xf0\x90\x8c\xbc",
	["Invalid 4 Octet Sequence (in 2nd Octet)"] = "\xf0\x28\x8c\xbc",
	["Invalid 4 Octet Sequence (in 3rd Octet)"] = "\xf0\x90\x28\xbc",
	["Invalid 4 Octet Sequence (in 4th Octet)"] = "\xf0\x28\x8c\x28",
	["Invalid Unicode: Valid 5 Octet Sequence (but not Unicode!)"] = "\xf8\xa1\xa1\xa1\xa1",
	["Invalid Unicode: Valid 6 Octet Sequence (but not Unicode!)"] = "\xfc\xa1\xa1\xa1\xa1\xa1",
}

describe("gt.string limits", function()
	describe("should respect unicode flag", function()
		for name, value in utf8Strings do
			if name:find("^Valid") then
				it(`should match {name}`, function()
					expect(gt.string():matches(value)).toBe(true)
					expect(gt.string({ unicode = true }):matches(value)).toBe(true)
				end)
			else
				it(`should not match {name} when unicode = true`, function()
					expect(gt.string():matches(value)).toBe(true)
					expect(gt.string({ unicode = true }):matches(value)).toBe(false)
				end)
			end
		end
	end)

	it("should respect bytes value", function()
		expect(gt.string({ bytes = "[0, 10]" }):matches("hello")).toBe(true)
		expect(gt.string({ bytes = "[0, 2]" }):matches("hello")).toBe(false)
		expect(gt.string({ bytes = "[0, 5]" }):matches("hello")).toBe(true)
		expect(gt.string({ bytes = "[0, 5)" }):matches("hello")).toBe(false)
		expect(gt.string({ bytes = "[0, 5]" }):matches("")).toBe(true)
		expect(gt.string({ bytes = "(0, 5]" }):matches("")).toBe(false)
	end)

	it("should respect graphemes value", function()
		expect(gt.string({ graphemes = "[0, 10]", bytes = "[0, 100]" }):matches("hello")).toBe(true)
		expect(gt.string({ graphemes = "[0, 2]", bytes = "[0, 100]" }):matches("hello")).toBe(false)
		expect(gt.string({ graphemes = "[0, 5]", bytes = "[0, 100]" }):matches("hello")).toBe(true)
		expect(gt.string({ graphemes = "[0, 5)", bytes = "[0, 100]" }):matches("hello")).toBe(false)
		expect(gt.string({ graphemes = "[0, 5]", bytes = "[0, 100]" }):matches("")).toBe(true)
		expect(gt.string({ graphemes = "(0, 5]", bytes = "[0, 100]" }):matches("")).toBe(false)

		-- One of the first examples from https://kermitproject.org/utf8.html#glass
		-- graphemes: 36
		-- bytes : 41
		local graphemeText = "kācaṃ śaknomyattum; nopahinasti mām."

		expect(gt.string({ graphemes = "[0, 36]", bytes = "[0, 41]" }):matches(graphemeText)).toBe(true)
		expect(gt.string({ graphemes = "[0, 36)", bytes = "[0, 41]" }):matches(graphemeText)).toBe(false)
		expect(gt.string({ graphemes = "[0, 36]", bytes = "[0, 41)" }):matches(graphemeText)).toBe(false)
		expect(gt.string({ graphemes = "[0, 30]", bytes = "[0, 40]" }):matches(graphemeText)).toBe(false)
	end)

	it("should error if graphemes is specified but bytes is not", function()
		expect(function()
			gt.string({ graphemes = "[0, 10]" })
		end).toThrow()
	end)

	it("should respect pattern value", function()
		expect(gt.string({ pattern = "^[a-z]+$" }):matches("hello")).toBe(true)
		expect(gt.string({ pattern = "^[a-z]+$" }):matches("hello world")).toBe(false)
		expect(gt.string({ pattern = "^[a-z]*$" }):matches("")).toBe(true)
		expect(gt.string({ pattern = "^[a-z]+$" }):matches("123")).toBe(false)
		expect(gt.string({ pattern = "^hello" }):matches("hello world")).toBe(true)
		expect(gt.string({ pattern = "^hello" }):matches("Hello world")).toBe(false)
	end)

	it("should format with limits if set", function()
		expect(gt.string():format()).toBe("string")

		expect(gt.string({ unicode = true }):format()).toBe("string<unicode>")
		expect(gt.string({ graphemes = "[0, 10]", bytes = "[0, 10]" }):format()).toBe("string<graphemes [0, 10], bytes [0, 10]>")
		expect(gt.string({ bytes = "[0, 10]" }):format()).toBe("string<bytes [0, 10]>")
		expect(gt.string({ pattern = "[a-z]+" }):format()).toBe('string<pattern "[a-z]+">')
		expect(gt.string({ pattern = "[a-z]+\n" }):format()).toBe('string<pattern "[a-z]+\\n">')

		expect(gt.string({ unicode = true, graphemes = "[5, 20]", bytes = "[0, 10]", pattern = "[a-z]+" }):format()).toBe(
			'string<unicode, graphemes [5, 20], bytes [0, 10], pattern "[a-z]+">'
		)
	end)
end)


basicTest({
	kind = "thread",
	fn = gt.thread,
	values = {
		coroutine.create(function() end),
		task.spawn(function() end),
	},
	skipBasicTypeCheck = true,
})

describe("gt.thread options", function()
	-- selene: allow(multiple_statements)
	-- stylua: ignore
	it("should handle statuses", function()
		local running = coroutine.running()
		local dead = coroutine.create(function() end); coroutine.resume(dead)
		local suspended = coroutine.create(function() coroutine.yield() end)

		expect(gt.thread({ status = "running" }):matches(running)).toBe(true)
		expect(gt.thread({ status = { "running" }}):matches(running)).toBe(true)
		expect(gt.thread({ status = "dead" }):matches(dead)).toBe(true)
		expect(gt.thread({ status = { "dead" }}):matches(dead)).toBe(true)
		expect(gt.thread({ status = "suspended" }):matches(suspended)).toBe(true)
		expect(gt.thread({ status = { "suspended" }}):matches(suspended)).toBe(true)

		expect(gt.thread({ status = "running" }):matches(suspended)).toBe(false)
		expect(gt.thread({ status = { "running" }}):matches(suspended)).toBe(false)

		expect(gt.thread({ status = { "running", "suspended" } }):matches(running)).toBe(true)
		expect(gt.thread({ status = { "running", "suspended" } }):matches(suspended)).toBe(true)
		expect(gt.thread({ status = { "running", "suspended" } }):matches(dead)).toBe(false)
	end)

	it("should format as 'thread' or 'thread<statuses>'", function()
		expect(gt.thread():format()).toBe("thread")
		expect(gt.thread({ status = "running" }):format()).toBe("thread<status: running>")
		expect(gt.thread({ status = { "running" }}):format()).toBe("thread<status: running>")
		expect(gt.thread({ status = { "running", "suspended" } }):format()).toBe("thread<status: (running | suspended)>")

		-- should format alphabetically:
		expect(gt.thread({ status = { "suspended", "running" } }):format()).toBe("thread<status: (running | suspended)>")
	end)

	it("should not allow invalid statuses", function()
		expect(function()
			gt.thread({ status = "invalid" })
		end).toThrow()
	end)
end)

describe("gt.vararg", function()
	it("should match zero or more of the given type only", function()
		expect(gt.vararg(gt.string()):matches()).toBe(true)
		expect(gt.vararg(gt.string()):matches("hello")).toBe(true)
		expect(gt.vararg(gt.string()):matches("hello", "hello2")).toBe(true)
		expect(gt.vararg(gt.string()):matches(5)).toBe(false)
		expect(gt.vararg(gt.string()):matches("hello", 5)).toBe(false)
	end)
	it("should ignore nil values on the end of the match", function()
		expect(gt.vararg(gt.string()):matches("hello", nil)).toBe(true)
		expect(gt.vararg(gt.string()):matches("hello", "hello2", nil)).toBe(true)
		expect(gt.vararg(gt.string()):matches("hello", nil, "hello3")).toBe(false)
	end)
	it("should format as '...type'", function()
		expect(gt.vararg(gt.string()):format()).toBe("...string")
		expect(gt.vararg(gt.number()):format()).toBe("...number")
	end)
end)

describe("gt.tuple", function()
	it("should match the given types", function()
		expect(gt.tuple(gt.string(), gt.number()):matches("hello", 5)).toBe(true)
		expect(gt.tuple(gt.string(), gt.number()):matches("hello", "hello2")).toBe(false)
		expect(gt.tuple(gt.string(), gt.number()):matches("hello", 5, "extra")).toBe(false)
		expect(gt.tuple(gt.string(), gt.number()):matches("hello")).toBe(false)
		expect(gt.tuple(gt.string(), gt.opt(gt.number())):matches("hello")).toBe(true)
		expect(gt.tuple():matches()).toBe(true)
		expect(gt.tuple():matches("hello")).toBe(false)

		expect(gt.tuple(gt.string(), gt.number()):matches("hello", 5, nil, nil, nil)).toBe(true)
	end)
	it("should match the given types with vararg", function()
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello", 5)).toBe(true)
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello", "hello2")).toBe(false)
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello", 5, "extra")).toBe(false)
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello")).toBe(true)
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello", 5, 6, 7, 8)).toBe(true)

		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello", 5, 6, 7, 8, nil, nil, nil)).toBe(true)
	end)
	it("should simplify non-ending tuples or varargs", function()
		expect(gt.tuple(gt.vararg(gt.string()), gt.number()):matches("hello", 5)).toBe(true)
		expect(gt.tuple(gt.tuple(gt.string()), gt.number()):matches("hello", 5)).toBe(true)
	end)
	it("should expand ending tuples or varargs", function()
		expect(gt.tuple(gt.string(), gt.tuple(gt.number())):matches("hello", 5)).toBe(true)
		expect(gt.tuple(gt.string(), gt.tuple(gt.number(), gt.string())):matches("hello", 5, "hello2")).toBe(true)
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello", 5)).toBe(true)
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):matches("hello", 5, 6)).toBe(true)
	end)

	it("should format correctly", function()
		expect(gt.tuple(gt.string(), gt.number()):format()).toBe("(string, number)")
		expect(gt.tuple(gt.string(), gt.vararg(gt.number())):format()).toBe("(string, ...number)")
		expect(gt.tuple(gt.string(), gt.tuple(gt.number())):format()).toBe("(string, number)")
		expect(gt.tuple(gt.string(), gt.tuple(gt.number(), gt.string())):format()).toBe("(string, number, string)")
		expect(gt.tuple(gt.tuple(gt.string(), gt.number())):format()).toBe("(string, number)")
		expect(gt.tuple(gt.tuple(gt.string())):format()).toBe("(string)")
		expect(gt.tuple(gt.tuple()):format()).toBe("()")
	end)
end)

describe("gt.fn", function()
	it("should match any function", function()
		-- It's not actually possible to inspect function args or values, so we just don't.
		expect(gt.fn(gt.args(), gt.returns()):matches(function() end)).toBe(true)
		expect(gt.fn(gt.args(), gt.returns()):matches(function(a, b, c)
			return 5, 6, 7
		end)).toBe(true)

		expect(gt.fn(gt.args(gt.string()), gt.returns(gt.number(), gt.string())):matches(function() end)).toBe(true)
		expect(gt.fn(gt.args(gt.string()), gt.returns(gt.number(), gt.string())):matches(function() end)).toBe(true)
	end)

	it("should format correctly", function()
		expect(gt.fn(gt.args(), gt.returns()):format()).toBe("() -> ()")
		expect(gt.fn(gt.args(gt.string()), gt.returns(gt.number())):format()).toBe("(string) -> number")
		expect(gt.fn(gt.args(gt.string()), gt.returns(gt.number(), gt.string())):format()).toBe(
			"(string) -> (number, string)"
		)
		expect(gt.fn(gt.args(gt.vararg(gt.string())), gt.returns(gt.string(), gt.vararg(gt.number()))):format()).toBe(
			"(...string) -> (string, ...number)"
		)

		expect(gt.anyfn():format()).toBe("(...any) -> ...any")
	end)
end)

describe("gt.table", function()
	it("should match basic tables", function()
		expect(gt.table({}):matches("not a table")).toBe(false)
		expect(gt.table({}):matches({})).toBe(true)
		expect(gt.table({ a = gt.number() }):matches({ a = 5 })).toBe(true)
		expect(gt.table({ a = gt.number() }):matches({ a = "hello" })).toBe(false)
		expect(gt.table({ a = gt.number(), b = gt.string() }):matches({ a = 5, b = "hello" })).toBe(true)
		expect(gt.table({ a = gt.number(), b = gt.string() }):matches({ a = 5 })).toBe(false)
	end)
	it("should match even if there are extra keys", function()
		expect(gt.table({ a = gt.number() }):matches({ a = 5, b = "hello" })).toBe(true)
	end)
	it("should match with an indexer for extra values", function()
		expect(gt.table({ a = gt.number(), [gt.string()] = gt.string() }):matches({ a = 5, b = "hello", c = "hi" })).toBe(
			true
		)
		expect(gt.table({ a = gt.number(), [gt.string()] = gt.string() }):matches({ a = "hey", b = "hello", c = "hi" })).toBe(
			false
		)
	end)
	it("should only allow one indexer", function()
		expect(function()
			gt.table({
				[gt.string()] = gt.string(),
				[gt.string()] = gt.string(),
			})
		end).toThrow()
		expect(function()
			gt.table({
				[gt.string()] = gt.string(),
				[gt.number()] = gt.string(),
			})
		end).toThrow()
	end)
	it("should only allow string keys (unless raw = true)", function()
		expect(function()
			gt.table({
				[{}] = gt.string(),
			})
		end).toThrow()

		expect(function()
			gt.table({
				[{}] = gt.string(),
			}, { raw = true })
		end).never.toThrow()
	end)
	it("should check for contiguous array when array = true", function()
		expect(gt.table({ gt.number() }, { array = true }):matches({ 1, 2, 3 })).toBe(true)
		expect(function()
			gt.table({}, { array = true }):matches({ 1, 2, 3, 4 })
		end).toThrow()
	end)
	it("should limit indexer items when count is set", function()
		expect(gt.table({ gt.number() }, { count = "[1,2]" }):matches({ 5 })).toBe(true)
		expect(gt.table({ gt.number() }, { count = "[1,2]" }):matches({ 5, 6, 7 })).toBe(false)
		expect(gt.table({ gt.number() }, { count = "[1,2]" }):matches({})).toBe(false)
		expect(
			gt.table(
				{ [gt.number()] = gt.number(), a = gt.number(), b = gt.number() },
				{ count = "[1,2]", a = 5, b = 5 }
			)
				:matches({ [1] = 5, a = 5, b = 5 })
		).toBe(true)
	end)
	it("should respect __index and __iter", function()
		local iterT = { a = 1, b = 2, c = 3 }
		local t = setmetatable({}, {
			__iter = function()
				return pairs(iterT)
			end,
			__index = function(_, key)
				return iterT[key]
			end,
		})

		expect(gt.table({ a = gt.number(), b = gt.number(), c = gt.number() }):matches(t)).toBe(
			true
		)
		expect(gt.table({ [gt.string()] = gt.number() }):matches(t)).toBe(true)
		expect(gt.table({ [gt.string()] = gt.string() }):matches(t)).toBe(false)
	end)
	it("should not care about metatable", function()
		local meta = {}
		local withMeta = setmetatable({}, meta)
		expect(gt.table(withMeta):matches(setmetatable({}, meta))).toBe(true)
		expect(gt.table(withMeta):matches(setmetatable({}, {}))).toBe(true)
		expect(gt.table(withMeta):matches({})).toBe(true)
	end)

	it("should format correctly", function()
		expect(gt.table({}):format()).toBe("{}")
		expect(gt.table({ a = gt.number() }):format()).toBe("{ a: number }")
		expect(gt.table({ a = gt.number(), b = gt.string() }):format()).toBe("{ a: number, b: string }")

		expect(gt.table({ [gt.string()] = gt.string() }):format()).toBe("{ [string]: string }")

		expect(gt.table({ [gt.string()] = gt.string(), a = gt.number() }):format()).toBe(
			"{ [string]: string, a: number }"
		)

		local meta = {}
		local withMeta = setmetatable({}, meta)
		expect(gt.table(withMeta):format()).toBe("{}")
	end)
end)

describe("gt.union", function()
	it("should match only any of the given types", function()
		expect(gt.union(gt.number(), gt.string()):matches(5)).toBe(true)
		expect(gt.union(gt.number(), gt.string()):matches("hello")).toBe(true)
		expect(gt.union(gt.number(), gt.string()):matches(true)).toBe(false)
	end)
	it("should disallow implicit nil", function()
		expect(function()
			gt.union(gt.number(), nil, gt.string())
		end).toThrow()
	end)

	it("should format correctly", function()
		expect(gt.union(gt.number(), gt.string()):format()).toBe("number | string")
		expect(gt.union(gt.number(), gt.string(), gt.none()):format()).toBe("number | string | nil")
		expect(gt.union(gt.number(), gt.none()):format()).toBe("number?")
		expect(gt.union(gt.number()):format()).toBe("number")
		expect(gt.union(gt.none()):format()).toBe("nil")
		expect(function()
			gt.union()
		end).toThrow()
	end)
end)

describe("gt.intersection", function()
	it("should match all of the given types", function()
		expect(gt.intersection(gt.number(), gt.string()):matches(5)).toBe(false)
		expect(gt.intersection(gt.number(), gt.string()):matches("hello")).toBe(false)
		expect(gt.intersection(gt.number(), gt.any()):matches(5)).toBe(true)
		expect(
			gt.intersection(gt.table({ a = gt.number() }, gt.table({ b = gt.string() })))
				:matches({ a = 5, b = "hello" })
		).toBe(true)
		expect(gt.intersection(gt.table({ a = gt.number() }), gt.table({ b = gt.string() })):matches({ a = 5 })).toBe(
			false
		)
	end)

	it("should disallow implicit nil", function()
		expect(function()
			gt.intersection(gt.number(), nil, gt.string())
		end).toThrow()
	end)

	it("should format correctly", function()
		expect(gt.intersection(gt.number(), gt.string()):format()).toBe("number & string")
		expect(gt.intersection(gt.number(), gt.string(), gt.none()):format()).toBe("number & string & nil")
		expect(gt.intersection(gt.number()):format()).toBe("number")
		expect(gt.intersection(gt.none()):format()).toBe("nil")
		
		expect(function()
			gt.intersection()
		end).toThrow()
	end)
end)

describe("Type", function()
	describe("Type:assert", function()
		local RegExp = require(script.Parent.Parent.DevPackages.RegExp)

		it("should assert", function()
			expect(function()
				gt.number():assert(5)
			end).never.toThrow()
			expect(function()
				gt.number():assert("hello")
			end).toThrow(RegExp("expected number, got string"))
		end)
	end)
end)

describe("failure to call detection", function()
	it("should throw if non-called function is passed in to typeof", function()
		for key, value in gt do
			expect(function()
				gt.typeof(value)
				print("failed to throw for:", typeof(key), key, value)
			end).toThrow()
		end
		for key, value in pairs(gt.isA) do
			expect(function()
				gt.typeof(value)
				print("failed to throw for:", typeof(key), key, value)
			end).toThrow()
		end
		for key, value in pairs(gt.t) do
			expect(function()
				gt.typeof(value)
				print("failed to throw for:", typeof(key), key, value)
			end).toThrow()
		end
	end)
end)
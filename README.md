# 🍵 GreenTea
An experimental runtime typechecker for Roblox Luau, with matching 'tooling-time' Luau types.

[Docs](https://corecii.github.io/GreenTea/)

## Features
- Check types at runtime (like [the t package](https://github.com/osyrisrblx/t))
- Build Luau types and runtime types simultaneously (so you have less duplicate code!)
- Pretty errors that tell you exactly where you went wrong, even for unions and intersections
- Inspect types at runtime, so you can procedurally check types in weird scenarios, such as when checking a set of Instance attributes.
- Infer types from values
- `t` compatibility (via `GreenTea.t` — all `t` tests pass!)

## Experimental
While this package has had a lot of work put into it, it's largely an experiment. It's a first iteration, so the API can use some improvement — especially the structure of the generated Type objects, which could have a better shape.

Please leave feedback on the issues page. Thank you!

## Install

### with Wally (for Rojo)
1. [Install Wally](https://wally.run/install)
2. Add `GreenTea = "corecii/greentea@0.4.11"` to your `wally.toml`

### with pesde (for Rojo or Lune)
1. [Install pesde](https://docs.pesde.daimond113.com/installation)
2. Run `pesde add corecii/greentea`\
   or add `GreenTea = { name = "corecii/greentea", version = "^0.4.11" }` to your `pesde.toml`

### Standalone (for non-Rojo)
- Download from [the Releases page](https://github.com/corecii/greentea/releases)

## How Does It Work?

GreenTea's runtime code looks like a typical runtime typechecker:
you call functions to create typecheckers and compose them together.

It has two tricks:
1. Its function type definitions _lie_ and say that it's returning _the type that you're checking for_ instead of a GreenTea.Type object.
  So while you're composing a runtime type, you're _also_ composing a Luau type definition!
2. The typechecker it produces is actually an _object_, which gives you a runtime-inspectable
  type definition you can do more advanced stuff with. This also means prettier errors that look a lot like Luau type errors.

## Examples

### Easy Mode

```lua
local gt = require(path.to.GreenTea)

local stringType = gt.build(gt.string())

-- equivalent to takesString(value: string)
local function takesString(value: typeof(stringType.type()))
	-- will error if value is not a string
	stringType:assert(value)

	-- ...
end
```

Using `gt` is recommended, because `GreenTea` is a lot to type and `gt` is not!

### How it Works

```lua
local stringTypeRaw = gt.string()

-- stringTypeRaw is typed in Luau as `string`
-- but at runtime, it's really a GreenTea.Type object.
-- This makes it easy to split it up into a runtime
-- typechecker and a Luau type:

-- give a name to the type
type stringTypeLuau = typeof(stringTypeRaw.type())

-- cast the object to the runtime typechecker type it really is
local stringTypeChecker = gt.typecast(stringTypeRaw)

-- You can think of this like "tricking" Luau into
-- "giving" us a `string` type.
-- This is all `GreenTea.build` does -- it typecasts
-- the value into a GreenTea.Type object, but includes
-- the original type os you can use `typeof`.

-- But the magic is this also works for more complex types!

local catTypeRaw = gt.table({
	age = gt.number(),
	meowSound = gt.string(),
	breed = {
		[gt.string()] = gt.number(),
	},
})

type catTypeLuau = typeof(catTypeRaw)
-- which is the same as...
type catTypeSame = {
	age: number,
	meowSound: string,
	breed: { [string]: number },
}

-- and we can get a runtime typechecker...
local catTypeChecker = gt.typecast(catTypeRaw)

-- OR we can make it really easy on ourselves if we just use `build` from the beginning:

local catType = gt.build(gt.table{
	age = gt.number(),
	meowSound = gt.string(),
	breed = {
		[gt.indexer(gt.string())] = gt.number(),
	},
}))

-- catType == catTypeChecker
-- catTypeLuau == typeof(catTypeChecker.type())
```

Using `build` is recommended because it makes this mostly type-safe:
- The returned value is properly typed a GreenTea Type object, so you
  have typechecked access to GreenTea.Type's properties and methods.
- TypeObject.type() can still be passed to GreenTea constructors to
  pass the GreenTea type in.
- `build` runs `GreenTea.typeof` internally, so you can pass it simple tables
  and they'll be converted to the proper `GreenTea.table` type.

In this way, "built" types are for direct use, and "unbuilt" types are for composition
with other GreenTea constructors.

### Taking in GreenTea types

GreenTea is great for libraries that want to do typechecking, like RemoteEvent wrappers, because you can make the API very ergonomic:

```lua
-- RemoteWrapper.luau

function RemoteWrapper.new<T>(greenTeaType: T): RemoteWrapper<T>
	local self = {
		typechecker = GreenTea.build(greenTeaType),
		...
	}

	return setmetatable(self, RemoteWrapper)
end

function RemoteWrapper.onServerEvent<T>(self: RemoteWrapper<T>, fn: (player: Player, params: T) -> ())
	self.event:OnServerEvent(function(player: Player, paramsMaybe: any?)
		local params = self.typechecker:assert(params)
		fn(player, params)
	end)
end

```
```lua
-- CatSpawner.luau

local SpawnCat = RemoteWrapper.new({
	cat = {
		age = gt.number(),
		meowSound = gt.string(),
		...
	},
	location = gt.CFrame(),
	...
})

SpawnVehicle:onServerEvent(function(player, params)
	-- params is properly typed as { cat: { age: number, ...}, location: CFrame, ... }
end)
```

## Lune Support

GreenTea has not been extensively tested on Lune.
Some Roblox-specific typecheckers might not work, and there may be other issues.
Please report any issues here: https://github.com/corecii/greentea/issues

## `t` Compatibility

`GreenTea.t` includes all of the methods from the `t` package. See [the t package's readme](https://github.com/osyrisrblx/t/blob/master/README.md) for more details.

### As a drop-in replacement

You can include the `greentea-t-standalone` package as an easy, drop-in replacement for a codebase which already uses `t`. Just add it to your `wally.toml`:

> **with Wally (for Rojo)**
> 1. [Install Wally](https://wally.run/install)
> 2. Add `t = "corecii/greentea-t-standalone@0.4.11"` to your `wally.toml`

> **with pesde (for Rojo)**
> 1. [Install pesde](https://docs.pesde.daimond113.com/installation)
> 2. Add `t = { name = "corecii/greentea_t_standalone", version = "^0.4.11" }` to your `pesde.toml`

This package just exports `GreenTea.t` to make drop-in replacement easy.

## Inspiration

This library was largely inspired by `t`, but also my experience working with t types, working with Luau types, and working with systems that need inspectable type definitions. This is my first attempt at solving all of these problems. I'd really like to see the inversion of this library -- where inspectable runtime type definitions are built from Luau types. But that's more work, so I'll leave it as a TODO!

## Future Plans
- Split up `GreenTea.luau` into multiple files. It's too long. But I'm tired and want to move on and actually use this library for cool things.
- Stop lying to the typechecker once it's able to handle all of GreenTea's features.
- Inverted GreenTea, where runtime typecheckers are built from Luau types.
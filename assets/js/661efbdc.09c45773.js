"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[720],{30321:e=>{e.exports=JSON.parse('{"functions":[{"name":"isGreenTeaType","desc":"Returns whether or not the input value is a GreenTea.Type object.","params":[{"name":"value","desc":"","lua_type":"any"}],"returns":[],"function_type":"static","source":{"line":663,"path":"src/GreenTea.luau"}},{"name":"any","desc":"Creates a new GreenTea Type that matches any value, excluding nil.","params":[{"name":"options","desc":"","lua_type":"{ allowNil: boolean? }?"}],"returns":[{"desc":"","lua_type":"any\\n"}],"function_type":"static","source":{"line":697,"path":"src/GreenTea.luau"}},{"name":"unknown","desc":"Creates a new GreenTea Type that matches any value, excluding nil.\\\\  \\nThis returns the unknown type, which has different behavior from any.\\\\  \\nThe runtime typechecking behavior is the same as GreenTea.any.","params":[{"name":"options","desc":"","lua_type":"{ allowNil: boolean? }?"}],"returns":[{"desc":"","lua_type":"unknown\\n"}],"function_type":"static","source":{"line":723,"path":"src/GreenTea.luau"}},{"name":"never","desc":"Creates a new GreenTea Type that matches no values.","params":[],"returns":[{"desc":"","lua_type":"never\\n"}],"function_type":"static","source":{"line":747,"path":"src/GreenTea.luau"}},{"name":"boolean","desc":"Creates a new GreenTea Type that matches boolean values.","params":[],"returns":[{"desc":"","lua_type":"boolean\\n"}],"function_type":"static","source":{"line":764,"path":"src/GreenTea.luau"}},{"name":"Instance","desc":"Creates a new GreenTea Type that matches number values.","params":[],"returns":[{"desc":"","lua_type":"Instance\\n"}],"function_type":"static","source":{"line":772,"path":"src/GreenTea.luau"}},{"name":"coroutine","desc":"Creates a new GreenTea Type that matches coroutine values.","params":[{"name":"options","desc":"","lua_type":"{ status: CoroutineStatus | { CoroutineStatus } | nil }?"}],"returns":[{"desc":"","lua_type":"thread\\n"}],"function_type":"static","source":{"line":782,"path":"src/GreenTea.luau"}},{"name":"buffer","desc":"Creates a new GreenTea Type that matches buffer values.","params":[],"returns":[{"desc":"","lua_type":"buffer\\n"}],"function_type":"static","source":{"line":847,"path":"src/GreenTea.luau"}},{"name":"none","desc":"Creates a new GreenTea Type that matches nil values.","params":[],"returns":[{"desc":"","lua_type":"nil\\n"}],"function_type":"static","source":{"line":873,"path":"src/GreenTea.luau"}},{"name":"literal","desc":"Creates a new GreenTea Type that matches a value literally.\\\\  \\nThis is checked with a basic == comparison.","params":[{"name":"value","desc":"","lua_type":"T"}],"returns":[{"desc":"","lua_type":"T\\n"}],"function_type":"static","source":{"line":881,"path":"src/GreenTea.luau"}},{"name":"withCustom","desc":"Creates a new GreenTea Type that matches a custom typechecker\\nwith GreenTea type as a base.","params":[{"name":"type","desc":"","lua_type":"T"},{"name":"typechecker","desc":"","lua_type":"(any) -> (boolean, any?)"},{"name":"name","desc":"","lua_type":"string?"}],"returns":[{"desc":"","lua_type":"T\\n"}],"function_type":"static","source":{"line":919,"path":"src/GreenTea.luau"}},{"name":"custom","desc":"Creates a new GreenTea Type that matches a custom typechecker\\nwith `any` as the base type.","params":[{"name":"typechecker","desc":"","lua_type":"(any) -> (boolean, any?)"},{"name":"name","desc":"","lua_type":"string?"}],"returns":[{"desc":"","lua_type":"any\\n"}],"function_type":"static","source":{"line":994,"path":"src/GreenTea.luau"}},{"name":"isA","desc":"Call IsA to get a type that matches instances of that class.","params":[{"name":"class","desc":"","lua_type":"string"}],"returns":[{"desc":"","lua_type":"any"}],"function_type":"static","source":{"line":1007,"path":"src/GreenTea.luau"}},{"name":"number","desc":"Creates a new GreenTea Type that matches number values.\\\\  \\nOptionally, you can specify a range and whether the number must be an integer.\\\\  \\nRegardless of specified limits, the returned Luau type will be a basic number. Limit checking will only be done at runtime.\\\\  \\nNOTE: NaN is rejected by default. NaN tends to propogate and \\"poison\\" numeric values,\\nand it\'s rarely actually desired. If you want to _allow_ NaN, set `nan = true` in the limits table.","params":[{"name":"limits","desc":"","lua_type":"{\\n\\trange: RangeInput?,\\n\\tinteger: boolean?,\\n\\tnan: boolean?,\\n}?"}],"returns":[{"desc":"","lua_type":"number\\n"}],"function_type":"static","source":{"line":1020,"path":"src/GreenTea.luau"}},{"name":"string","desc":"Creates a new GreenTea Type that matches string values.\\\\  \\nOptionally, you can specify a pattern, a length range, and whether the string must be utf8.\\\\  \\nNOTE: this supports both \\"byte length\\" and \\"grapheme length\\". Grapheme length is the number of visible\\ncharacters. In non-english languages, the number of bytes can be significantly higher than the number\\nof graphemes.\\\\  \\nWhen using graphemes limit, you should still set a (much-higher) bytes limit to prevent abuse, since\\ngraphemes have no upper limit to their size. For this reason, this constructor will error if you\\ndo not specify a bytes limit when using graphemes limit. If you really want infinite byte length, set\\nbytes to `[0, inf]`.","params":[{"name":"limits","desc":"","lua_type":"{\\n\\tpattern: string?,\\n\\tbytes: RangeInput?,\\n\\tgraphemes: RangeInput?,\\n\\tunicode: boolean?,\\n}?"}],"returns":[{"desc":"","lua_type":"string\\n"}],"function_type":"static","source":{"line":1104,"path":"src/GreenTea.luau"}},{"name":"isTypeof","desc":"Creates a new GreenTea Type that matches any `typeof(value)` type.","params":[{"name":"typeName","desc":"","lua_type":"string"},{"name":"value","desc":"","lua_type":"T?"}],"returns":[{"desc":"","lua_type":"T\\n"}],"function_type":"static","source":{"line":1199,"path":"src/GreenTea.luau"}},{"name":"isType","desc":"Creates a new GreenTea Type that matches any `type(value)` type.","params":[{"name":"typeName","desc":"","lua_type":"string"},{"name":"value","desc":"","lua_type":"T?"}],"returns":[{"desc":"","lua_type":"T\\n"}],"function_type":"static","source":{"line":1204,"path":"src/GreenTea.luau"}},{"name":"types not listed here","desc":"A few methods are not listed in the docs so that they don\'t clutter with basic types.  \\nHere\'s a list: userdata, Vector2, vector, Vector3, CFrame, Color3, UDim, UDim2,\\nRay, Rect, Region3, BrickColor, Font, Enum, EnumItem.  \\nFor the most part, any Luau type you\'d naturally write likely also exists under GreenTea.\\nLess common types are excluded (for now) to not clutter the library.\\nFor the ones that don\'t exist, you can usually use `isTypeof` or `isType`.","params":[],"returns":[],"function_type":"static","source":{"line":1216,"path":"src/GreenTea.luau"}},{"name":"vararg","desc":"Creates a new GreenTea Type that matches a repeating value.","params":[{"name":"type","desc":"","lua_type":"T"},{"name":"options","desc":"","lua_type":"{\\n\\tlength: RangeInput?,\\n}?"}],"returns":[{"desc":"","lua_type":"...T\\n"}],"function_type":"static","source":{"line":1222,"path":"src/GreenTea.luau"}},{"name":"tuple","desc":"Creates a new GreenTea Type that matches a tuple of values.\\\\  \\nSpecify the final value with GreenTea.vararg to match a repeating final value.\\\\  \\nThis properly accepts a tuple as the final or only argument, so don\'t fear\\npassing a tuple into this by accident.","params":[{"name":"...","desc":"","lua_type":"T..."}],"returns":[{"desc":"","lua_type":"T...\\n"}],"function_type":"static","source":{"line":1323,"path":"src/GreenTea.luau"}},{"name":"args","desc":"Used for GreenTea.fn only. This is used to pass args to GreenTea.fn ergonomically.","params":[{"name":"...","desc":"","lua_type":"T..."}],"returns":[{"desc":"","lua_type":"(T...) -> ()\\n"}],"function_type":"static","source":{"line":1427,"path":"src/GreenTea.luau"}},{"name":"returns","desc":"Used for GreenTea.fn only. This is used to pass returns to GreenTea.fn ergonomically.","params":[{"name":"...","desc":"","lua_type":"T..."}],"returns":[{"desc":"","lua_type":"() -> T...\\n"}],"function_type":"static","source":{"line":1432,"path":"src/GreenTea.luau"}},{"name":"fn","desc":"Creates a new GreenTea Type that matches a function.\\\\  \\nThis cannot perform any runtime checks on the function\'s args or returns.\\nThe args and returns exist primarily to have an accurate Luau type.\\\\  \\nThey _can_ be inspected at runtime with `type.fn.args` and `type.fn.returns`.\\\\  \\nIf you\'re building a module that consumes GreenTea types, you can use\\n`type.fn.args` and `type.fn.returns` to run runtime typechecking yourself.\\nFor example, if you were building a RemoteFunction wrapper, you could use\\n`type.fn.returns` to check that the client returned correct values.","params":[{"name":"args","desc":"","lua_type":"(Args...) -> ()"},{"name":"returns","desc":"","lua_type":"() -> Returns..."}],"returns":[{"desc":"","lua_type":"(Args...) -> Returns...\\n"}],"function_type":"static","source":{"line":1444,"path":"src/GreenTea.luau"}},{"name":"anyfn","desc":"Creates a new GreenTea Type that matches a function with any args and any returns.","params":[],"returns":[{"desc":"","lua_type":"(...any) -> ...any\\n"}],"function_type":"static","source":{"line":1526,"path":"src/GreenTea.luau"}},{"name":"tuplePacked","desc":"Creates a new GreenTea Type that \\"smuggles\\" a tuple as a single value by\\npacking it into the returns.\\\\  \\nThis is useful for accepting GreenTea tuple types to a spot that only accepts a single value.\\nThe actual returned type here is equivalent to\\n`GreenTea.fn(GreenTea.args(), GreenTea.returns(...))`\\nso some inspections is required at runtime to pull out the tuple:\\n`type.fn.returns` is the actual tuple value.\\\\  \\nWith Luau typechecking, you can \\"unsmuggle\\" the tuple by doing\\n`typeof(smuggledTuple())` to get the tuple type.","params":[{"name":"...","desc":"","lua_type":"T..."}],"returns":[{"desc":"","lua_type":"() -> T...\\n"}],"function_type":"static","source":{"line":1539,"path":"src/GreenTea.luau"}},{"name":"table","desc":"Creates a new GreenTea Type that matches a table.\\\\  \\nThe `contents` table should be a dictionary of string keys to types.\\\\  \\nAn indexer can be provided with `{ [keyGreenTeaType] = valueType }`.\\\\  \\nThis will attempt to match metatables as well, but it\'s only a shallow == comparison.\\\\  \\nBecause this attempts to match metatables, it ignores __iter and __index metamethods unless respectIter is specified.\\\\  \\nTables are non-strict by default. Extra values are allowed. If you\'d\\nlike to make a table strict, add an indexer that denies extra values, like\\n`[gt.any()] = gt.never()`.\\\\  \\nBy default, array-like definitions do not check for holes. If you would like\\nto check for holes and ensure the array is contiguous, set array to true.\\\\  \\nIf you\'d like to enforce an item limit, specify it `count` the options table. This limits\\nthe number of items found by the indexer. Items in the \\"struct portion\\" of the\\ntable definition do not count. This is _not_ a #table check: the actual items\\nare counted one-by-one as we check them. This lets you also limit the number of items\\nin cases like dynamic dictionaries.\\\\  \\n`raw` allows for checking any key types as literally matching to values, including numbers\\nand other non-strings. This _will not_ appear correct as a Luau type; it is primarily\\nintended for compatibility with other runtime typechecking libraries.","params":[{"name":"contents","desc":"","lua_type":"T"},{"name":"options","desc":"","lua_type":"{\\n\\trespectIter: boolean?,\\n\\tarray: boolean?,\\n\\tcount: RangeInput?,\\n\\traw: boolean?,\\n}?"}],"returns":[{"desc":"","lua_type":"T\\n"}],"function_type":"static","source":{"line":1579,"path":"src/GreenTea.luau"}},{"name":"array","desc":"Creates a new GreenTea Type that matches an array of values.\\\\\\nConvenience function that calls GreenTea.table internally.","params":[{"name":"value","desc":"","lua_type":"T"},{"name":"options","desc":"","lua_type":"{\\n\\tcount: RangeInput?,\\n}?"}],"returns":[{"desc":"","lua_type":"{ T }\\n"}],"function_type":"static","source":{"line":1805,"path":"src/GreenTea.luau"}},{"name":"dictionary","desc":"Creates a new GreenTea Type that matches a dictionary of values.\\\\\\nConvenience function that calls GreenTea.table internally.","params":[{"name":"key","desc":"","lua_type":"K"},{"name":"value","desc":"","lua_type":"V"},{"name":"options","desc":"","lua_type":"{\\n\\tcount: RangeInput?,\\n}?"}],"returns":[{"desc":"","lua_type":"{ [K]: V }\\n"}],"function_type":"static","source":{"line":1816,"path":"src/GreenTea.luau"}},{"name":"union","desc":"Creates a new GreenTea Type that matches a union of types.\\\\  \\nThis is analogous to the Luau `type1 | type2 | ...` syntax.\\\\  \\nThis has convenient type definitions for up to 5 input types.\\\\  \\nWhen specifying more then 5 input types, you will have to typecast the\\nfirst item to a Luau type union in order to get correct types.","params":[{"name":"...","desc":"","lua_type":"T..."}],"returns":[{"desc":"","lua_type":"T..."}],"function_type":"static","source":{"line":1939,"path":"src/GreenTea.luau"}},{"name":"intersection","desc":"Creates a new GreenTea Type that matches an intersection of types.\\\\  \\nThis is analogous to the Luau `type1 & type2 & ...` syntax.\\\\  \\nThis has convenient type definitions for up to 5 input types.\\\\  \\nWhen specifying more then 5 input types, you will have to typecast the\\nfirst item to a Luau type intersection in order to get correct types.","params":[{"name":"...","desc":"","lua_type":"T..."}],"returns":[{"desc":"","lua_type":"T..."}],"function_type":"static","source":{"line":2045,"path":"src/GreenTea.luau"}},{"name":"optional","desc":"Creates a new GreenTea Type that makes a value optionally,\\nlike `type?` or `type | nil` in Luau types.","params":[{"name":"value","desc":"","lua_type":"T"}],"returns":[{"desc":"","lua_type":"T?\\n"}],"function_type":"static","source":{"line":2050,"path":"src/GreenTea.luau"}},{"name":"typeof","desc":"Creates a new GreenTea Type matching the inferred type of `value`.\\\\  \\nA runtime, this passes tables to `GreenTea.table`, functions to `GreenTea.fn`, and so on.\\\\  \\nThis results in a runtime-inferred type roughly equivalent to the Luau typechecker\'s inferred types.","params":[{"name":"value","desc":"","lua_type":"T"},{"name":"recursionCheck","desc":"","lua_type":"{ [any]: any }?"}],"returns":[{"desc":"","lua_type":"T\\n"}],"function_type":"static","source":{"line":2063,"path":"src/GreenTea.luau"}},{"name":"typecast","desc":"Typecasts a value to a GreenTea type.\\\\  \\nAt runtime this will error if the passed-in type is _not_ a GreenTea type.\\\\  \\nThis exists as a convenience function because `type :: GreenTea.Type` is usually\\nnot possible without an any cast like `(type :: any) :: GreenTea.Type`.\\\\  \\nThis errors at runtime if the passed-in type is not a GreenTea type.","params":[{"name":"value","desc":"","lua_type":"any"}],"returns":[{"desc":"","lua_type":"Type\\n"}],"function_type":"static","source":{"line":2102,"path":"src/GreenTea.luau"}},{"name":"build","desc":"Typecasts a value to be a GreenTea type, with the type stored under the `type`\\nkey so you can get the type with `typeof(result.type())`","params":[{"name":"...","desc":"","lua_type":"T..."}],"returns":[{"desc":"","lua_type":"Type & { __getType: () -> T... } & typeof(BuiltType)\\n"}],"function_type":"static","source":{"line":2131,"path":"src/GreenTea.luau"}}],"properties":[{"name":"isA","desc":"Types for individual instance classes.\\\\  \\nIndex this to get a type that matches instances of that class.\\\\  \\nFor dynamic class names, or for classes not yet added here,\\\\  \\ncall this with GreenTea.IsA(\\"ClassName\\").","lua_type":"{ [string]: any }","source":{"line":1014,"path":"src/GreenTea.luau"}}],"types":[{"name":"RangeInput","desc":"A value that can be parsed as a range.  \\nAs a string, can be \\"[min, max]\\", \\"(min, max)\\", \\"(min, max]\\", \\"(, max]\\" etc. or a number max inclusive.\\nAs a number, it\'s interpreted as a number max inclusive.","lua_type":"string | number | { min: number?, minExclusive: boolean?, max: number?, maxExclusive: boolean? }","source":{"line":377,"path":"src/GreenTea.luau"}}],"name":"GreenTea","desc":"","source":{"line":157,"path":"src/GreenTea.luau"}}')}}]);
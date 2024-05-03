
local GreenTea = require(script.GreenTea)
local InstanceClasses = require(script.InstanceClasses)
local tCompat = require(script.tCompat)

export type Cause = GreenTea.Cause
export type Type = GreenTea.Type
export type TuplePacked<T...> = GreenTea.TuplePacked<T...>

local export = {}

export.t = tCompat

export.isGreenTeaType = GreenTea.isGreenTeaType
export.isGtType = GreenTea.isGtType

export.any = GreenTea.any
export.unknown = GreenTea.unknown
export.never = GreenTea.never

export.boolean = GreenTea.boolean
export.bool = GreenTea.bool

export.Instance = GreenTea.Instance
export.isA = InstanceClasses
export.IsA = InstanceClasses

export.coroutine = GreenTea.coroutine
export.thread = GreenTea.thread

export.buffer = GreenTea.buffer

export.userdata = GreenTea.userdata

export.Vector2 = GreenTea.Vector2
export.vector = GreenTea.vector
export.Vector3 = GreenTea.Vector3
export.CFrame = GreenTea.CFrame
export.Color3 = GreenTea.Color3
export.UDim = GreenTea.UDim
export.UDim2 = GreenTea.UDim2
export.Ray = GreenTea.Ray
export.Rect = GreenTea.Rect
export.Region3 = GreenTea.Region3
export.BrickColor = GreenTea.BrickColor
export.Font = GreenTea.Font

export.Enum = GreenTea.Enum
export.EnumItem = GreenTea.EnumItem

export.none = GreenTea.none

export.literal = GreenTea.literal

export.withCustom = GreenTea.withCustom
export.custom = GreenTea.custom

export.number = GreenTea.number

export.string = GreenTea.string

export.isTypeof = GreenTea.isTypeof
export.isType = GreenTea.isType

export.vararg = GreenTea.vararg
export.tuple = GreenTea.tuple

export.args = GreenTea.args
export.returns = GreenTea.returns
export.fn = GreenTea.fn
export.anyfn = GreenTea.anyfn

export.tuplePacked = GreenTea.tuplePacked

export.table = GreenTea.table
export.struct = GreenTea.struct

export.anyTable = GreenTea.anyTable
export.array = GreenTea.array
export.dictionary = GreenTea.dictionary

export.union = GreenTea.union
export.oneOf = GreenTea.oneOf

export.intersection = GreenTea.intersection
export.allOf = GreenTea.allOf

export.optional = GreenTea.optional
export.opt = GreenTea.opt

export.typeof = GreenTea.typeof

export.typecast = GreenTea.typecast
export.asGreenTeaType = GreenTea.asGreenTeaType
export.asGtType = GreenTea.asGtType

export.build = GreenTea.build

table.freeze(export)
table.freeze(export.isA)
table.freeze(export.t)
table.freeze(GreenTea.__Cause)
table.freeze(GreenTea.__Type)

for key, value in export do
	GreenTea.__greenteaConstructorsSet[value] = `GreenTea.{key}`
end

for key: string, value: any in pairs(export.isA :: any) do
	GreenTea.__greenteaConstructorsSet[value] = `GreenTea.isA.{key}`
end

for key: string, value: any in pairs(export.t) do
	GreenTea.__greenteaConstructorsSet[value] = `GreenTea.t.{key}`
end

return export
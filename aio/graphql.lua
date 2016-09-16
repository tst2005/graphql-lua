do --{{
local sources, priorities = {}, {};assert(not sources["graphql.validate"],"module already exists")sources["graphql.validate"]=([===[-- <pack graphql.validate> --
local path = (...):gsub('%.[^%.]+$', '')
local rules = require(path .. '.rules')
local util = require(path .. '.util')
local introspection = require(path .. '.introspection')
local schema = require(path .. '.schema')

local function getParentField(context, name, count)
  if introspection.fieldMap[name] then return introspection.fieldMap[name] end

  count = count or 1
  local parent = context.objects[#context.objects - count]

  -- Unwrap lists and non-null types
  while parent.ofType do
    parent = parent.ofType
  end

  return parent.fields[name]
end

local visitors = {
  document = {
    enter = function(node, context)
      for _, definition in ipairs(node.definitions) do
        if definition.kind == 'fragmentDefinition' then
          context.fragmentMap[definition.name.value] = definition
        end
      end
    end,

    children = function(node, context)
      return node.definitions
    end,

    rules = { rules.uniqueFragmentNames, exit = { rules.noUnusedFragments } }
  },

  operation = {
    enter = function(node, context)
      table.insert(context.objects, context.schema[node.operation])
      context.currentOperation = node
      context.variableReferences = {}
    end,

    exit = function(node, context)
      table.remove(context.objects)
      context.currentOperation = nil
      context.variableReferences = nil
    end,

    children = function(node)
      return { node.selectionSet }
    end,

    rules = {
      rules.uniqueOperationNames,
      rules.loneAnonymousOperation,
      rules.directivesAreDefined,
      rules.variablesHaveCorrectType,
      rules.variableDefaultValuesHaveCorrectType,
      exit = {
        rules.variablesAreUsed,
        rules.variablesAreDefined
      }
    }
  },

  selectionSet = {
    children = function(node)
      return node.selections
    end,

    rules = { rules.unambiguousSelections }
  },

  field = {
    enter = function(node, context)
      local name = node.name.value

      if introspection.fieldMap[name] then
        table.insert(context.objects, introspection.fieldMap[name].kind)
      else
        local parentField = getParentField(context, name, 0)
        -- false is a special value indicating that the field was not present in the type definition.
        table.insert(context.objects, parentField and parentField.kind or false)
      end
    end,

    exit = function(node, context)
      table.remove(context.objects)
    end,

    children = function(node)
      local children = {}

      if node.arguments then
        for i = 1, #node.arguments do
          table.insert(children, node.arguments[i])
        end
      end

      if node.directives then
        for i = 1, #node.directives do
          table.insert(children, node.directives[i])
        end
      end

      if node.selectionSet then
        table.insert(children, node.selectionSet)
      end

      return children
    end,

    rules = {
      rules.fieldsDefinedOnType,
      rules.argumentsDefinedOnType,
      rules.scalarFieldsAreLeaves,
      rules.compositeFieldsAreNotLeaves,
      rules.uniqueArgumentNames,
      rules.argumentsOfCorrectType,
      rules.requiredArgumentsPresent,
      rules.directivesAreDefined,
      rules.variableUsageAllowed
    }
  },

  inlineFragment = {
    enter = function(node, context)
      local kind = false

      if node.typeCondition then
        kind = context.schema:getType(node.typeCondition.name.value) or false
      end

      table.insert(context.objects, kind)
    end,

    exit = function(node, context)
      table.remove(context.objects)
    end,

    children = function(node, context)
      if node.selectionSet then
        return {node.selectionSet}
      end
    end,

    rules = {
      rules.fragmentHasValidType,
      rules.fragmentSpreadIsPossible,
      rules.directivesAreDefined
    }
  },

  fragmentSpread = {
    enter = function(node, context)
      context.usedFragments[node.name.value] = true

      local fragment = context.fragmentMap[node.name.value]

      if not fragment then return end

      local fragmentType = context.schema:getType(fragment.typeCondition.name.value) or false

      table.insert(context.objects, fragmentType)

      if context.currentOperation then
        local seen = {}
        local function collectTransitiveVariables(referencedNode)
          if not referencedNode then return end

          if referencedNode.kind == 'selectionSet' then
            for _, selection in ipairs(referencedNode.selections) do
              if not seen[selection] then
                seen[selection] = true
                collectTransitiveVariables(selection)
              end
            end
          elseif referencedNode.kind == 'field' then
            if referencedNode.arguments then
              for _, argument in ipairs(referencedNode.arguments) do
                collectTransitiveVariables(argument)
              end
            end

            if referencedNode.selectionSet then
              collectTransitiveVariables(referencedNode.selectionSet)
            end
          elseif referencedNode.kind == 'argument' then
            return collectTransitiveVariables(referencedNode.value)
          elseif referencedNode.kind == 'listType' or referencedNode.kind == 'nonNullType' then
            return collectTransitiveVariables(referencedNode.type)
          elseif referencedNode.kind == 'variable' then
            context.variableReferences[referencedNode.name.value] = true
          elseif referencedNode.kind == 'inlineFragment' then
            return collectTransitiveVariables(referencedNode.selectionSet)
          elseif referencedNode.kind == 'fragmentSpread' then
            local fragment = context.fragmentMap[referencedNode.name.value]
            context.usedFragments[referencedNode.name.value] = true
            return fragment and collectTransitiveVariables(fragment.selectionSet)
          end
        end

        collectTransitiveVariables(fragment.selectionSet)
      end
    end,

    exit = function(node, context)
      table.remove(context.objects)
    end,

    rules = {
      rules.fragmentSpreadTargetDefined,
      rules.fragmentSpreadIsPossible,
      rules.directivesAreDefined,
      rules.variableUsageAllowed
    }
  },

  fragmentDefinition = {
    enter = function(node, context)
      kind = context.schema:getType(node.typeCondition.name.value) or false
      table.insert(context.objects, kind)
    end,

    exit = function(node, context)
      table.remove(context.objects)
    end,

    children = function(node)
      local children = {}

      for _, selection in ipairs(node.selectionSet) do
        table.insert(children, selection)
      end

      return children
    end,

    rules = {
      rules.fragmentHasValidType,
      rules.fragmentDefinitionHasNoCycles,
      rules.directivesAreDefined
    }
  },

  argument = {
    enter = function(node, context)
      if context.currentOperation then
        local value = node.value
        while value.kind == 'listType' or value.kind == 'nonNullType' do
          value = value.type
        end

        if value.kind == 'variable' then
          context.variableReferences[value.name.value] = true
        end
      end
    end,

    rules = { rules.uniqueInputObjectFields }
  },

  directive = {
    children = function(node, context)
      return node.arguments
    end
  }
}

return function(schema, tree)
  local context = {
    schema = schema,
    fragmentMap = {},
    operationNames = {},
    hasAnonymousOperation = false,
    usedFragments = {},
    objects = {},
    currentOperation = nil,
    variableReferences = nil
  }

  local function visit(node)
    local visitor = node.kind and visitors[node.kind]

    if not visitor then return end

    if visitor.enter then
      visitor.enter(node, context)
    end

    if visitor.rules then
      for i = 1, #visitor.rules do
        visitor.rules[i](node, context)
      end
    end

    if visitor.children then
      local children = visitor.children(node)
      if children then
        for _, child in ipairs(children) do
          visit(child)
        end
      end
    end

    if visitor.rules and visitor.rules.exit then
      for i = 1, #visitor.rules.exit do
        visitor.rules.exit[i](node, context)
      end
    end

    if visitor.exit then
      visitor.exit(node, context)
    end
  end

  return visit(tree)
end
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
assert(not sources["graphql.util"],"module already exists")sources["graphql.util"]=([===[-- <pack graphql.util> --
local util = {}

function util.map(t, fn)
  local res = {}
  for k, v in pairs(t) do res[k] = fn(v, k) end
  return res
end

function util.find(t, fn)
  local res = {}
  for k, v in pairs(t) do
    if fn(v, k) then return v end
  end
end

function util.filter(t, fn)
  local res = {}
  for k,v in pairs(t) do
    if fn(v) then
      table.insert(res, v)
    end
  end
  return res
end

function util.values(t)
  local res = {}
  for _, value in pairs(t) do
    table.insert(res, value)
  end
  return res
end

function util.compose(f, g)
  return function(...) return f(g(...)) end
end

function util.bind1(func, x)
  return function(y)
    return func(x, y)
  end
end

function util.trim(s)
  return s:gsub('^%s+', ''):gsub('%s$', ''):gsub('%s%s+', ' ')
end

function util.coerceValue(node, schemaType, variables)
  variables = variables or {}

  if schemaType.__type == 'NonNull' then
    return util.coerceValue(node, schemaType.ofType, variables)
  end

  if not node then
    return nil
  end

  if node.kind == 'variable' then
    return variables[node.name.value]
  end

  if schemaType.__type == 'List' then
    if node.kind ~= 'list' then
      error('Expected a list')
    end

    return util.map(node.values, function(value)
      return util.coerceValue(node.values[i], schemaType.ofType, variables)
    end)
  end

  if schemaType.__type == 'InputObject' then
    if node.kind ~= 'inputObject' then
      error('Expected an input object')
    end

    return util.map(node.values, function(field)
      if not schemaType.fields[field.name] then
        error('Unknown input object field "' .. field.name .. '"')
      end

      return util.coerceValue(field.value, schemaType.fields[field.name].kind, variables)
    end)
  end

  if schemaType.__type == 'Enum' then
    if node.kind ~= 'enum' then
      error('Expected enum value, got ' .. node.kind)
    end

    if not schemaType.values[node.value] then
      error('Invalid enum value "' .. node.value .. '"')
    end

    return node.value
  end

  if schemaType.__type == 'Scalar' then
    if schemaType.parseLiteral(node) == nil then
      error('Could not coerce "' .. tostring(node.value) .. '" to "' .. schemaType.name .. '"')
    end

    return schemaType.parseLiteral(node)
  end
end

return util
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
assert(not sources["graphql.types"],"module already exists")sources["graphql.types"]=([===[-- <pack graphql.types> --
local path = (...):gsub('%.[^%.]+$', '')
local util = require(path .. '.util')

local types = {}

function types.nonNull(kind)
  assert(kind, 'Must provide a type')

  return {
    __type = 'NonNull',
    ofType = kind
  }
end

function types.list(kind)
  assert(kind, 'Must provide a type')

  return {
    __type = 'List',
    ofType = kind
  }
end

function types.scalar(config)
  assert(type(config.name) == 'string', 'type name must be provided as a string')
  assert(type(config.serialize) == 'function', 'serialize must be a function')
  if config.parseValue or config.parseLiteral then
    assert(
      type(config.parseValue) == 'function' and type(config.parseLiteral) == 'function',
      'must provide both parseValue and parseLiteral to scalar type'
    )
  end

  local instance = {
    __type = 'Scalar',
    name = config.name,
    description = config.description,
    serialize = config.serialize,
    parseValue = config.parseValue,
    parseLiteral = config.parseLiteral
  }

  instance.nonNull = types.nonNull(instance)

  return instance
end

function types.object(config)
  assert(type(config.name) == 'string', 'type name must be provided as a string')
  if config.isTypeOf then
    assert(type(config.isTypeOf) == 'function', 'must provide isTypeOf as a function')
  end

  local fields
  if type(config.fields) == 'function' then
    fields = util.compose(util.bind1(initFields, 'Object'), config.fields)
  else
    fields = initFields('Object', config.fields)
  end

  local instance = {
    __type = 'Object',
    name = config.name,
    description = config.description,
    isTypeOf = config.isTypeOf,
    fields = fields,
    interfaces = config.interfaces
  }

  instance.nonNull = types.nonNull(instance)

  return instance
end

function types.interface(config)
  assert(type(config.name) == 'string', 'type name must be provided as a string')
  assert(type(config.fields) == 'table', 'fields table must be provided')
  if config.resolveType then
    assert(type(config.resolveType) == 'function', 'must provide resolveType as a function')
  end

  local fields
  if type(config.fields) == 'function' then
    fields = util.compose(util.bind1(initFields, 'Interface'), config.fields)
  else
    fields = initFields('Interface', config.fields)
  end

  local instance = {
    __type = 'Interface',
    name = config.name,
    description = config.description,
    fields = fields,
    resolveType = config.resolveType
  }

  instance.nonNull = types.nonNull(instance)

  return instance
end

function initFields(kind, fields)
  assert(type(fields) == 'table', 'fields table must be provided')

  local result = {}

  for fieldName, field in pairs(fields) do
    field = field.__type and { kind = field } or field
    result[fieldName] = {
      name = fieldName,
      kind = field.kind,
      description = field.description,
      deprecationReason = field.deprecationReason,
      arguments = field.arguments or {},
      resolve = kind == 'Object' and field.resolve or nil
    }
  end

  return result
end

function types.enum(config)
  assert(type(config.name) == 'string', 'type name must be provided as a string')
  assert(type(config.values) == 'table', 'values table must be provided')

  local instance
  local values = {}

  for name, entry in pairs(config.values) do
    entry = type(entry) == 'table' and entry or { value = entry }

    values[name] = {
      name = name,
      description = entry.description,
      deprecationReason = entry.deprecationReason,
      value = entry.value
    }
  end

  instance = {
    __type = 'Enum',
    name = config.name,
    description = config.description,
    values = values,
    serialize = function(name)
      return instance.values[name] and instance.values[name].value or name
    end
  }

  instance.nonNull = types.nonNull(instance)

  return instance
end

function types.union(config)
  assert(type(config.name) == 'string', 'type name must be provided as a string')
  assert(type(config.types) == 'table', 'types table must be provided')

  local instance = {
    __type = 'Union',
    name = config.name,
    types = config.types
  }

  instance.nonNull = types.nonNull(instance)

  return instance
end

function types.inputObject(config)
  assert(type(config.name) == 'string', 'type name must be provided as a string')

  local fields = {}
  for fieldName, field in pairs(config.fields) do
    field = field.__type and { kind = field } or field
    fields[fieldName] = {
      name = fieldName,
      kind = field.kind
    }
  end

  local instance = {
    __type = 'InputObject',
    name = config.name,
    description = config.description,
    fields = fields
  }

  return instance
end

local coerceInt = function(value)
  value = tonumber(value)

  if not value then return end

  if value == value and value < 2 ^ 32 and value >= -2 ^ 32 then
    return value < 0 and math.ceil(value) or math.floor(value)
  end
end

types.int = types.scalar({
  name = 'Int',
  description = "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1. ", 
  serialize = coerceInt,
  parseValue = coerceInt,
  parseLiteral = function(node)
    if node.kind == 'int' then
      return coerceInt(node.value)
    end
  end
})

types.float = types.scalar({
  name = 'Float',
  serialize = tonumber,
  parseValue = tonumber,
  parseLiteral = function(node)
    if node.kind == 'float' or node.kind == 'int' then
      return tonumber(node.value)
    end
  end
})

types.string = types.scalar({
  name = 'String',
  description = "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.",
  serialize = tostring,
  parseValue = tostring,
  parseLiteral = function(node)
    if node.kind == 'string' then
      return node.value
    end
  end
})

local function toboolean(x)
  return (x and x ~= 'false') and true or false
end

types.boolean = types.scalar({
  name = 'Boolean',
  description = "The `Boolean` scalar type represents `true` or `false`.",
  serialize = toboolean,
  parseValue = toboolean,
  parseLiteral = function(node)
    if node.kind == 'boolean' then
      return toboolean(node.value)
    else
      return nil
    end
  end
})

types.id = types.scalar({
  name = 'ID',
  serialize = tostring,
  parseValue = tostring,
  parseLiteral = function(node)
    return node.kind == 'string' or node.kind == 'int' and node.value or nil
  end
})

function types.directive(config)
  assert(type(config.name) == 'string', 'type name must be provided as a string')

  local instance = {
    __type = 'Directive',
    name = config.name,
    description = config.description,
    arguments = config.arguments,
    onQuery = config.onQuery,
    onMutation = config.onMutation,
    onField = config.onField,
    onFragmentDefinition = config.onFragmentDefinition,
    onFragmentSpread = config.onFragmentSpread,
    onInlineFragment = config.onInlineFragment
  }

  return instance
end

types.include = types.directive({
  name = 'include',
  description = 'Directs the executor to include this field or fragment only when the `if` argument is true.',
  arguments = {
    ['if'] = { kind = types.boolean.nonNull, description = 'Included when true.'}
  },
  onField = true,
  onFragmentSpread = true,
  onInlineFragment = true
})

types.skip = types.directive({
  name = 'skip',
  description = 'Directs the executor to skip this field or fragment when the `if` argument is true.',
  arguments = {
    ['if'] = { kind = types.boolean.nonNull, description = 'Skipped when true.' }
  },
  onField = true,
  onFragmentSpread = true,
  onInlineFragment = true
})

return types
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
assert(not sources["graphql.schema"],"module already exists")sources["graphql.schema"]=([===[-- <pack graphql.schema> --
local path = (...):gsub('%.[^%.]+$', '')
local types = require(path .. '.types')
local introspection = require(path .. '.introspection')

local schema = {}
schema.__index = schema

function schema.create(config)
  assert(type(config.query) == 'table', 'must provide query object')
  assert(not config.mutation or type(config.mutation) == 'table', 'mutation must be a table if provided')

  local self = setmetatable({}, schema)

  for k, v in pairs(config) do
    self[k] = v
  end

  self.directives = self.directives or {
    types.include,
    types.skip
  }

  self.typeMap = {}
  self.interfaceMap = {}
  self.directiveMap = {}

  self:generateTypeMap(self.query)
  self:generateTypeMap(self.mutation)
  self:generateTypeMap(introspection.__Schema)
  self:generateDirectiveMap()

  return self
end

function schema:generateTypeMap(node)
  if not node or (self.typeMap[node.name] and self.typeMap[node.name] == node) then return end

  if node.__type == 'NonNull' or node.__type == 'List' then
    return self:generateTypeMap(node.ofType)
  end

  if self.typeMap[node.name] and self.typeMap[node.name] ~= node then
    error('Encountered multiple types named "' .. node.name .. '"')
  end

  node.fields = type(node.fields) == 'function' and node.fields() or node.fields
  self.typeMap[node.name] = node

  if node.__type == 'Object' and node.interfaces then
    for _, interface in ipairs(node.interfaces) do
      self:generateTypeMap(interface)
      self.interfaceMap[interface.name] = self.interfaceMap[interface.name] or {}
      self.interfaceMap[interface.name][node] = node
    end
  end

  if node.__type == 'Object' or node.__type == 'Interface' or node.__type == 'InputObject' then
    for fieldName, field in pairs(node.fields) do
      if field.arguments then
        for name, argument in pairs(field.arguments) do
          local argumentType = argument.__type and argument or argument.kind
          assert(argumentType, 'Must supply type for argument "' .. name .. '" on "' .. fieldName .. '"')
          self:generateTypeMap(argumentType)
        end
      end

      self:generateTypeMap(field.kind)
    end
  end
end

function schema:generateDirectiveMap()
  for _, directive in ipairs(self.directives) do
    self.directiveMap[directive.name] = directive
  end
end

function schema:getType(name)
  if not name then return end
  return self.typeMap[name]
end

function schema:getImplementors(interface)
  local kind = self:getType(interface)
  local isInterface = kind and kind.__type == 'Interface'
  return self.interfaceMap[interface] or (isInterface and {} or nil)
end

function schema:getDirective(name)
  if not name then return false end
  return self.directiveMap[name]
end

function schema:getQueryType()
  return self.query
end

function schema:getMutationType()
  return self.mutation
end

function schema:getTypeMap()
  return self.typeMap
end

function schema:getPossibleTypes(abstractType)
  if abstractType.__type == 'Union' then
    return abstractType.types
  end

  return self:getImplementors(abstractType)
end

return schema
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
assert(not sources["graphql.rules"],"module already exists")sources["graphql.rules"]=([===[-- <pack graphql.rules> --
local path = (...):gsub('%.[^%.]+$', '')
local types = require(path .. '.types')
local util = require(path .. '.util')
local schema = require(path .. '.schema')
local introspection = require(path .. '.introspection')

local function getParentField(context, name, count)
  if introspection.fieldMap[name] then return introspection.fieldMap[name] end

  count = count or 1
  local parent = context.objects[#context.objects - count]

  -- Unwrap lists and non-null types
  while parent.ofType do
    parent = parent.ofType
  end

  return parent.fields[name]
end

local rules = {}

function rules.uniqueOperationNames(node, context)
  local name = node.name and node.name.value

  if name then
    if context.operationNames[name] then
      error('Multiple operations exist named "' .. name .. '"')
    end

    context.operationNames[name] = true
  end
end

function rules.loneAnonymousOperation(node, context)
  local name = node.name and node.name.value

  if context.hasAnonymousOperation or (not name and next(context.operationNames)) then
    error('Cannot have more than one operation when using anonymous operations')
  end

  if not name then
    context.hasAnonymousOperation = true
  end
end

function rules.fieldsDefinedOnType(node, context)
  if context.objects[#context.objects] == false then
    local parent = context.objects[#context.objects - 1]
    while parent.ofType do parent = parent.ofType end
    error('Field "' .. node.name.value .. '" is not defined on type "' .. parent.name .. '"')
  end
end

function rules.argumentsDefinedOnType(node, context)
  if node.arguments then
    local parentField = getParentField(context, node.name.value)
    for _, argument in pairs(node.arguments) do
      local name = argument.name.value
      if not parentField.arguments[name] then
        error('Non-existent argument "' .. name .. '"')
      end
    end
  end
end

function rules.scalarFieldsAreLeaves(node, context)
  if context.objects[#context.objects].__type == 'Scalar' and node.selectionSet then
    error('Scalar values cannot have subselections')
  end
end

function rules.compositeFieldsAreNotLeaves(node, context)
  local _type = context.objects[#context.objects].__type
  local isCompositeType = _type == 'Object' or _type == 'Interface' or _type == 'Union'

  if isCompositeType and not node.selectionSet then
    error('Composite types must have subselections')
  end
end

function rules.unambiguousSelections(node, context)
  local selectionMap = {}
  local seen = {}

  local function findConflict(entryA, entryB)

    -- Parent types can't overlap if they're different objects.
    -- Interface and union types may overlap.
    if entryA.parent ~= entryB.parent and entryA.__type == 'Object' and entryB.__type == 'Object' then
      return
    end

    -- Error if there are aliases that map two different fields to the same name.
    if entryA.field.name.value ~= entryB.field.name.value then
      return 'Type name mismatch'
    end

    -- Error if there are fields with the same name that have different return types.
    if entryA.definition and entryB.definition and entryA.definition ~= entryB.definition then
      return 'Return type mismatch'
    end

    -- Error if arguments are not identical for two fields with the same name.
    local argsA = entryA.field.arguments or {}
    local argsB = entryB.field.arguments or {}

    if #argsA ~= #argsB then
      return 'Argument mismatch'
    end

    local argMap = {}

    for i = 1, #argsA do
      argMap[argsA[i].name.value] = argsA[i].value
    end

    for i = 1, #argsB do
      local name = argsB[i].name.value
      if not argMap[name] then
        return 'Argument mismatch'
      elseif argMap[name].kind ~= argsB[i].value.kind then
        return 'Argument mismatch'
      elseif argMap[name].value ~= argsB[i].value.value then
        return 'Argument mismatch'
      end
    end
  end

  local function validateField(key, entry)
    if selectionMap[key] then
      for i = 1, #selectionMap[key] do
        local conflict = findConflict(selectionMap[key][i], entry)
        if conflict then
          error(conflict)
        end
      end

      table.insert(selectionMap[key], entry)
    else
      selectionMap[key] = { entry }
    end
  end

  -- Recursively make sure that there are no ambiguous selections with the same name.
  local function validateSelectionSet(selectionSet, parentType)
    for _, selection in ipairs(selectionSet.selections) do
      if selection.kind == 'field' then
        if not parentType or not parentType.fields or not parentType.fields[selection.name.value] then return end

        local key = selection.alias and selection.alias.name.value or selection.name.value
        local definition = parentType.fields[selection.name.value].kind

        local fieldEntry = {
          parent = parentType,
          field = selection,
          definition = definition
        }

        validateField(key, fieldEntry)
      elseif selection.kind == 'inlineFragment' then
        local parentType = selection.typeCondition and context.schema:getType(selection.typeCondition.name.value) or parentType
        validateSelectionSet(selection.selectionSet, parentType)
      elseif selection.kind == 'fragmentSpread' then
        local fragmentDefinition = context.fragmentMap[selection.name.value]
        if fragmentDefinition and not seen[fragmentDefinition] then
          seen[fragmentDefinition] = true
          if fragmentDefinition and fragmentDefinition.typeCondition then
            local parentType = context.schema:getType(fragmentDefinition.typeCondition.name.value)
            validateSelectionSet(fragmentDefinition.selectionSet, parentType)
          end
        end
      end
    end
  end

  validateSelectionSet(node, context.objects[#context.objects])
end

function rules.uniqueArgumentNames(node, context)
  if node.arguments then
    local arguments = {}
    for _, argument in ipairs(node.arguments) do
      local name = argument.name.value
      if arguments[name] then
        error('Encountered multiple arguments named "' .. name .. '"')
      end
      arguments[name] = true
    end
  end
end

function rules.argumentsOfCorrectType(node, context)
  if node.arguments then
    local parentField = getParentField(context, node.name.value)
    for _, argument in pairs(node.arguments) do
      local name = argument.name.value
      local argumentType = parentField.arguments[name]
      util.coerceValue(argument.value, argumentType.kind or argumentType)
    end
  end
end

function rules.requiredArgumentsPresent(node, context)
  local arguments = node.arguments or {}
  local parentField = getParentField(context, node.name.value)
  for name, argument in pairs(parentField.arguments) do
    if argument.__type == 'NonNull' then
      local present = util.find(arguments, function(argument)
        return argument.name.value == name
      end)

      if not present then
        error('Required argument "' .. name .. '" was not supplied.')
      end
    end
  end
end

function rules.uniqueFragmentNames(node, context)
  local fragments = {}
  for _, definition in ipairs(node.definitions) do
    if definition.kind == 'fragmentDefinition' then
      local name = definition.name.value
      if fragments[name] then
        error('Encountered multiple fragments named "' .. name .. '"')
      end
      fragments[name] = true
    end
  end
end

function rules.fragmentHasValidType(node, context)
  if not node.typeCondition then return end

  local name = node.typeCondition.name.value
  local kind = context.schema:getType(name)

  if not kind then
    error('Fragment refers to non-existent type "' .. name .. '"')
  end

  if kind.__type ~= 'Object' and kind.__type ~= 'Interface' and kind.__type ~= 'Union' then
    error('Fragment type must be an Object, Interface, or Union, got ' .. kind.__type)
  end
end

function rules.noUnusedFragments(node, context)
  for _, definition in ipairs(node.definitions) do
    if definition.kind == 'fragmentDefinition' then
      local name = definition.name.value
      if not context.usedFragments[name] then
        error('Fragment "' .. name .. '" was not used.')
      end
    end
  end
end

function rules.fragmentSpreadTargetDefined(node, context)
  if not context.fragmentMap[node.name.value] then
    error('Fragment spread refers to non-existent fragment "' .. node.name.value .. '"')
  end
end

function rules.fragmentDefinitionHasNoCycles(node, context)
  local seen = { [node.name.value] = true }

  local function detectCycles(selectionSet)
    for _, selection in ipairs(selectionSet.selections) do
      if selection.kind == 'inlineFragment' then
        detectCycles(selection.selectionSet)
      elseif selection.kind == 'fragmentSpread' then
        if seen[selection.name.value] then
          error('Fragment definition has cycles')
        end

        seen[selection.name.value] = true

        local fragmentDefinition = context.fragmentMap[selection.name.value]
        if fragmentDefinition and fragmentDefinition.typeCondition then
          detectCycles(fragmentDefinition.selectionSet)
        end
      end
    end
  end

  detectCycles(node.selectionSet)
end

function rules.fragmentSpreadIsPossible(node, context)
  local fragment = node.kind == 'inlineFragment' and node or context.fragmentMap[node.name.value]

  local parentType = context.objects[#context.objects - 1]
  while parentType.ofType do parentType = parentType.ofType end

  local fragmentType
  if node.kind == 'inlineFragment' then
    fragmentType = node.typeCondition and context.schema:getType(node.typeCondition.name.value) or parentType
  else
    fragmentType = context.schema:getType(fragment.typeCondition.name.value)
  end

  -- Some types are not present in the schema.  Let other rules handle this.
  if not parentType or not fragmentType then return end

  local function getTypes(kind)
    if kind.__type == 'Object' then
      return { [kind] = kind }
    elseif kind.__type == 'Interface' then
      return context.schema:getImplementors(kind.name)
    elseif kind.__type == 'Union' then
      local types = {}
      for i = 1, #kind.types do
        types[kind.types[i]] = kind.types[i]
      end
      return types
    else
      return {}
    end
  end

  local parentTypes = getTypes(parentType)
  local fragmentTypes = getTypes(fragmentType)

  local valid = util.find(parentTypes, function(kind)
    return fragmentTypes[kind]
  end)

  if not valid then
    error('Fragment type condition is not possible for given type')
  end
end

function rules.uniqueInputObjectFields(node, context)
  local function validateValue(value)
    if value.kind == 'listType' or value.kind == 'nonNullType' then
      return validateValue(value.type)
    elseif value.kind == 'inputObject' then
      local fieldMap = {}
      for _, field in ipairs(value.values) do
        if fieldMap[field.name] then
          error('Multiple input object fields named "' .. field.name .. '"')
        end

        fieldMap[field.name] = true

        validateValue(field.value)
      end
    end
  end

  validateValue(node.value)
end

function rules.directivesAreDefined(node, context)
  if not node.directives then return end

  for _, directive in pairs(node.directives) do
    if not context.schema:getDirective(directive.name.value) then
      error('Unknown directive "' .. directive.name.value .. '"')
    end
  end
end

function rules.variablesHaveCorrectType(node, context)
  local function validateType(type)
    if type.kind == 'listType' or type.kind == 'nonNullType' then
      validateType(type.type)
    elseif type.kind == 'namedType' then
      local schemaType = context.schema:getType(type.name.value)
      if not schemaType then
        error('Variable specifies unknown type "' .. tostring(type.name.value) .. '"')
      elseif schemaType.__type ~= 'Scalar' and schemaType.__type ~= 'Enum' and schemaType.__type ~= 'InputObject' then
        error('Variable types must be scalars, enums, or input objects, got "' .. schemaType.__type .. '"')
      end
    end
  end

  if node.variableDefinitions then
    for _, definition in ipairs(node.variableDefinitions) do
      validateType(definition.type)
    end
  end
end

function rules.variableDefaultValuesHaveCorrectType(node, context)
  if node.variableDefinitions then
    for _, definition in ipairs(node.variableDefinitions) do
      if definition.type.kind == 'nonNullType' and definition.defaultValue then
        error('Non-null variables can not have default values')
      elseif definition.defaultValue then
        util.coerceValue(definition.defaultValue, context.schema:getType(definition.type.name.value))
      end
    end
  end
end

function rules.variablesAreUsed(node, context)
  if node.variableDefinitions then
    for _, definition in ipairs(node.variableDefinitions) do
      local variableName = definition.variable.name.value
      if not context.variableReferences[variableName] then
        error('Unused variable "' .. variableName .. '"')
      end
    end
  end
end

function rules.variablesAreDefined(node, context)
  if context.variableReferences then
    local variableMap = {}
    for _, definition in ipairs(node.variableDefinitions or {}) do
      variableMap[definition.variable.name.value] = true
    end

    for variable in pairs(context.variableReferences) do
      if not variableMap[variable] then
        error('Unknown variable "' .. variable .. '"')
      end
    end
  end
end

function rules.variableUsageAllowed(node, context)
  if context.currentOperation then
    local variableMap = {}
    for _, definition in ipairs(context.currentOperation.variableDefinitions or {}) do
      variableMap[definition.variable.name.value] = definition
    end

    local arguments

    if node.kind == 'field' then
      arguments = { [node.name.value] = node.arguments }
    elseif node.kind == 'fragmentSpread' then
      local seen = {}
      local function collectArguments(referencedNode)
        if referencedNode.kind == 'selectionSet' then
          for _, selection in ipairs(referencedNode.selections) do
            if not seen[selection] then
              seen[selection] = true
              collectArguments(selection)
            end
          end
        elseif referencedNode.kind == 'field' and referencedNode.arguments then
          local fieldName = referencedNode.name.value
          arguments[fieldName] = arguments[fieldName] or {}
          for _, argument in ipairs(referencedNode.arguments) do
            table.insert(arguments[fieldName], argument)
          end
        elseif referencedNode.kind == 'inlineFragment' then
          return collectArguments(referencedNode.selectionSet)
        elseif referencedNode.kind == 'fragmentSpread' then
          local fragment = context.fragmentMap[referencedNode.name.value]
          return fragment and collectArguments(fragment.selectionSet)
        end
      end

      local fragment = context.fragmentMap[node.name.value]
      if fragment then
        arguments = {}
        collectArguments(fragment.selectionSet)
      end
    end

    if not arguments then return end

    for field in pairs(arguments) do
      local parentField = getParentField(context, field)
      for i = 1, #arguments[field] do
        local argument = arguments[field][i]
        if argument.value.kind == 'variable' then
          local argumentType = parentField.arguments[argument.name.value]

          local variableName = argument.value.name.value
          local variableDefinition = variableMap[variableName]
          local hasDefault = variableDefinition.defaultValue ~= nil

          local function typeFromAST(variable)
            local innerType
            if variable.kind == 'listType' then
              innerType = typeFromAST(variable.type)
              return innerType and types.list(innerType)
            elseif variable.kind == 'nonNullType' then
              innerType = typeFromAST(variable.type)
              return innerType and types.nonNull(innerType)
            else
              assert(variable.kind == 'namedType', 'Variable must be a named type')
              return context.schema:getType(variable.name.value)
            end
          end

          local variableType = typeFromAST(variableDefinition.type)

          if hasDefault and variableType.__type ~= 'NonNull' then
            variableType = types.nonNull(variableType)
          end

          local function isTypeSubTypeOf(subType, superType)
            if subType == superType then return true end

            if superType.__type == 'NonNull' then
              if subType.__type == 'NonNull' then
                return isTypeSubTypeOf(subType.ofType, superType.ofType)
              end

              return false
            elseif subType.__type == 'NonNull' then
              return isTypeSubTypeOf(subType.ofType, superType)
            end

            if superType.__type == 'List' then
              if subType.__type == 'List' then
                return isTypeSubTypeOf(subType.ofType, superType.ofType)
              end

              return false
            elseif subType.__type == 'List' then
              return false
            end

            if subType.__type ~= 'Object' then return false end

            if superType.__type == 'Interface' then
              local implementors = context.schema:getImplementors(superType.name)
              return implementors and implementors[context.schema:getType(subType.name)]
            elseif superType.__type == 'Union' then
              local types = superType.types
              for i = 1, #types do
                if types[i] == subType then
                  return true
                end
              end

              return false
            end

            return false
          end

          if not isTypeSubTypeOf(variableType, argumentType) then
            error('Variable type mismatch')
          end
        end
      end
    end
  end
end

return rules
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
assert(not sources["graphql.parse"],"module already exists")sources["graphql.parse"]=([===[-- <pack graphql.parse> --
local lpeg = require 'lpeg'
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Ct, Cmt, Cg, Cc, Cf, Cmt = lpeg.C, lpeg.Ct, lpeg.Cmt, lpeg.Cg, lpeg.Cc, lpeg.Cf, lpeg.Cmt

local line
local lastLinePos

local function pack(...)
  return { n = select('#', ...), ... }
end

-- Utility
local ws = Cmt(S(' \t\r\n') ^ 0, function(str, pos)
  str = str:sub(lastLinePos, pos)
  while str:find('\n') do
    line = line + 1
    lastLinePos = pos
    str = str:sub(str:find('\n') + 1)
  end

  return true
end)

local comma = P(',') ^ 0

local _ = V

local function maybe(pattern)
  if type(pattern) == 'string' then pattern = V(pattern) end
  return pattern ^ -1
end

local function list(pattern, min)
  if type(pattern) == 'string' then pattern = V(pattern) end
  min = min or 0
  return Ct((pattern * ws * comma * ws) ^ min)
end

-- Formatters
local function simpleValue(key)
  return function(value)
    return {
      kind = key,
      value = value
    }
  end
end

local cName = simpleValue('name')
local cInt = simpleValue('int')
local cFloat = simpleValue('float')
local cBoolean = simpleValue('boolean')
local cEnum = simpleValue('enum')

local cString = function(value)
  return {
    kind = 'string',
    value = value:gsub('\\"', '"')
  }
end

local function cList(value)
  return {
    kind = 'list',
    values = value
  }
end

local function cObjectField(name, value)
  return {
    name = name,
    value = value
  }
end

local function cObject(fields)
  return {
    kind = 'inputObject',
    values = fields
  }
end

local function cAlias(name)
  return {
    kind = 'alias',
    name = name
  }
end

local function cArgument(name, value)
  return {
    kind = 'argument',
    name = name,
    value = value
  }
end

local function cField(...)
  local tokens = pack(...)
  local field = { kind = 'field' }

  for i = 1, #tokens do
    local key = tokens[i].kind
    if not key then
      if tokens[i][1].kind == 'argument' then
        key = 'arguments'
      elseif tokens[i][1].kind == 'directive' then
        key = 'directives'
      end
    end

    field[key] = tokens[i]
  end

  return field
end

local function cSelectionSet(selections)
  return {
    kind = 'selectionSet',
    selections = selections
  }
end

local function cFragmentSpread(name, directives)
  return {
    kind = 'fragmentSpread',
    name = name,
    directives = directives
  }
end

local function cOperation(...)
  local args = pack(...)
  if args[1].kind == 'selectionSet' then
    return {
      kind = 'operation',
      operation = 'query',
      selectionSet = args[1]
    }
  else
    local result = {
      kind = 'operation',
      operation = args[1]
    }

    for i = 2, #args do
      local key = args[i].kind
      if not key then
        if args[i][1].kind == 'variableDefinition' then
          key = 'variableDefinitions'
        elseif args[i][1].kind == 'directive' then
          key = 'directives'
        end
      end

      result[key] = args[i]
    end

    return result
  end
end

local function cDocument(definitions)
  return {
    kind = 'document',
    definitions = definitions
  }
end

local function cFragmentDefinition(name, typeCondition, selectionSet)
  return {
    kind = 'fragmentDefinition',
    name = name,
    typeCondition = typeCondition,
    selectionSet = selectionSet
  }
end

local function cNamedType(name)
  return {
    kind = 'namedType',
    name = name
  }
end

local function cListType(type)
  return {
    kind = 'listType',
    type = type
  }
end

local function cNonNullType(type)
  return {
    kind = 'nonNullType',
    type = type
  }
end

local function cInlineFragment(...)
  local args = pack(...)
  local result = { kind = 'inlineFragment' }
  result.selectionSet = args[#args]
  for i = 1, #args - 1 do
    if args[i].kind == 'namedType' or args[i].kind == 'listType' or args[i].kind == 'nonNullType' then
      result.typeCondition = args[i]
    elseif args[i][1] and args[i][1].kind == 'directive' then
      result.directives = args[i]
    end
  end
  return result
end

local function cVariable(name)
  return {
    kind = 'variable',
    name = name
  }
end

local function cVariableDefinition(variable, type, defaultValue)
  return {
    kind = 'variableDefinition',
    variable = variable,
    type = type,
    defaultValue = defaultValue
  }
end

local function cDirective(name, arguments)
  return {
    kind = 'directive',
    name = name,
    arguments = arguments
  }
end

-- Simple types
local rawName = (P'_' + R('az', 'AZ')) * (P'_' + R'09' + R('az', 'AZ')) ^ 0
local name = rawName / cName
local fragmentName = (rawName - ('on' * -rawName)) / cName
local alias = ws * name * P':' * ws / cAlias

local integerPart = P'-' ^ -1 * ('0' + R'19' * R'09' ^ 0)
local intValue = integerPart / cInt
local fractionalPart = '.' * R'09' ^ 1
local exponentialPart = S'Ee' * S'+-' ^ -1 * R'09' ^ 1
local floatValue = integerPart * ((fractionalPart * exponentialPart) + fractionalPart + exponentialPart) / cFloat

local booleanValue = (P'true' + P'false') / cBoolean
local stringValue = P'"' * C((P'\\"' + 1 - S'"\n') ^ 0) * P'"' / cString
local enumValue = (rawName - 'true' - 'false' - 'null') / cEnum
local variable = ws * '$' * name / cVariable

-- Grammar
local graphQL = P {
  'document',
  document = ws * list('definition') / cDocument * -1,
  definition = _'operation' + _'fragmentDefinition',

  operationType = C(P'query' + P'mutation'),
  operation = (_'operationType' * ws * maybe(name) * maybe('variableDefinitions') * maybe('directives') * _'selectionSet' + _'selectionSet') / cOperation,
  fragmentDefinition = 'fragment' * ws * fragmentName * ws * _'typeCondition' * ws * _'selectionSet' / cFragmentDefinition,

  selectionSet = ws * '{' * ws * list('selection') * ws * '}' / cSelectionSet,
  selection = ws * (_'field' + _'fragmentSpread' + _'inlineFragment'),

  field = ws * maybe(alias) * name * maybe('arguments') * maybe('directives') * maybe('selectionSet') / cField,
  fragmentSpread = ws * '...' * ws * fragmentName * maybe('directives') / cFragmentSpread,
  inlineFragment = ws * '...' * ws * maybe('typeCondition') * maybe('directives') * _'selectionSet' / cInlineFragment,
  typeCondition = 'on' * ws * _'namedType',

  argument = ws * name * ':' * _'value' / cArgument,
  arguments = '(' * list('argument', 1) * ')',

  directive = '@' * name * maybe('arguments') / cDirective,
  directives = ws * list('directive', 1) * ws,

  variableDefinition = ws * variable * ws * ':' * ws * _'type' * (ws * '=' * _'value') ^ -1 * comma * ws / cVariableDefinition,
  variableDefinitions = ws * '(' * list('variableDefinition', 1) * ')',

  value = ws * (variable + _'objectValue' + _'listValue' + enumValue + stringValue + booleanValue + floatValue + intValue),
  listValue = '[' * list('value') * ']' / cList,
  objectFieldValue = ws * C(rawName) * ws * ':' * ws * _'value' * comma / cObjectField,
  objectValue = '{' * ws * list('objectFieldValue') * ws * '}' / cObject,

  type = _'nonNullType' + _'listType' + _'namedType',
  namedType = name / cNamedType,
  listType = '[' * ws * _'type' * ws * ']' / cListType,
  nonNullType = (_'namedType' + _'listType') * '!' / cNonNullType
}

-- TODO doesn't handle quotes that immediately follow escaped backslashes.
local function stripComments(str)
  return (str .. '\n'):gsub('(.-\n)', function(line)
    local index = 1
    while line:find('#', index) do
      local pos = line:find('#', index) - 1
      local chunk = line:sub(1, pos)
      local _, quotes = chunk:gsub('([^\\]")', '')
      if quotes % 2 == 0 then
        return chunk .. '\n'
      else
        index = pos + 2
      end
    end

    return line
  end):sub(1, -2)
end

return function(str)
  assert(type(str) == 'string', 'parser expects a string')
  str = stripComments(str)
  line, lastLinePos = 1, 1
  return graphQL:match(str) or error('Syntax error near line ' .. line, 2)
end
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
assert(not sources["graphql.introspection"],"module already exists")sources["graphql.introspection"]=([===[-- <pack graphql.introspection> --
local path = (...):gsub('%.[^%.]+$', '')
local types = require(path .. '.types')
local util = require(path .. '.util')

local __Schema, __Directive, __DirectiveLocation, __Type, __Field, __InputValue,__EnumValue, __TypeKind

__Schema = types.object({
  name = '__Schema',

  description = util.trim [[
    A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types
    and directives on the server, as well as the entry points for query and mutation operations.
  ]],

  fields = function()
    return {
      types = {
        description = 'A list of all types supported by this server.',
        kind = types.nonNull(types.list(types.nonNull(__Type))),
        resolve = function(schema)
          return util.values(schema:getTypeMap())
        end
      },

      queryType = {
        description = 'The type that query operations will be rooted at.',
        kind = __Type.nonNull,
        resolve = function(schema)
          return schema:getQueryType()
        end
      },

      mutationType = {
        description = 'If this server supports mutation, the type that mutation operations will be rooted at.',
        kind = __Type,
        resolve = function(schema)
          return schema:getMutationType()
        end
      },

      directives = {
        description = 'A list of all directives supported by this server.',
        kind = types.nonNull(types.list(types.nonNull(__Directive))),
        resolve = function(schema)
          return schema.directives
        end
      }
    }
  end
})

__Directive = types.object({
  name = '__Directive',

  description = util.trim [[
    A Directive provides a way to describe alternate runtime execution and type validation behavior
    in a GraphQL document.

    In some cases, you need to provide options to alter GraphQL’s execution
    behavior in ways field arguments will not suffice, such as conditionally including or skipping a
    field. Directives provide this by describing additional information to the executor.
  ]],

  fields = function()
    return {
      name = types.nonNull(types.string),

      description = types.string,

      locations = {
        kind = types.nonNull(types.list(types.nonNull(
          __DirectiveLocation
        ))),
        resolve = function(directive)
          local res = {}

          if directive.onQuery then table.insert(res, 'QUERY') end
          if directive.onMutation then table.insert(res, 'MUTATION') end
          if directive.onField then table.insert(res, 'FIELD') end
          if directive.onFragmentDefinition then table.insert(res, 'FRAGMENT_DEFINITION') end
          if directive.onFragmentSpread then table.insert(res, 'FRAGMENT_SPREAD') end
          if directive.onInlineFragment then table.insert(res, 'INLINE_FRAGMENT') end

          return res
        end
      },

      args = {
        kind = types.nonNull(types.list(types.nonNull(__InputValue))),
        resolve = function(field)
          local args = {}
          local transform = function(a, n)
            if a.__type then
              return { kind = a, name = n }
            else
              if a.name then return a end

              local r = { name = n }
              for k,v in pairs(a) do
                r[k] = v
              end

              return r
            end
          end

          for k, v in pairs(field.arguments or {}) do
            table.insert(args, transform(v, k))
          end

          return args
        end
      }
    }
  end
})

__DirectiveLocation = types.enum({
  name = '__DirectiveLocation',

  description = util.trim [[
    A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation
    describes one such possible adjacencies.
  ]],

  values = {
    QUERY = {
      value = 'QUERY',
      description = 'Location adjacent to a query operation.'
    },

    MUTATION = {
      value = 'MUTATION',
      description = 'Location adjacent to a mutation operation.'
    },

    FIELD = {
      value = 'FIELD',
      description = 'Location adjacent to a field.'
    },

    FRAGMENT_DEFINITION = {
      value = 'FRAGMENT_DEFINITION',
      description = 'Location adjacent to a fragment definition.'
    },

    FRAGMENT_SPREAD = {
      value = 'FRAGMENT_SPREAD',
      description = 'Location adjacent to a fragment spread.'
    },

    INLINE_FRAGMENT = {
      value = 'INLINE_FRAGMENT',
      description = 'Location adjacent to an inline fragment.'
    }
  }
})

__Type = types.object({
  name = '__Type',

  description = util.trim [[
    The fundamental unit of any GraphQL Schema is the type. There are
    many kinds of types in GraphQL as represented by the `__TypeKind` enum.

    Depending on the kind of a type, certain fields describe
    information about that type. Scalar types provide no information
    beyond a name and description, while Enum types provide their values.
    Object and Interface types provide the fields they describe. Abstract
    types, Union and Interface, provide the Object types possible
    at runtime. List and NonNull types compose other types.
  ]],

  fields = function()
    return {
      name = types.string,
      description = types.string,

      kind = {
        kind = __TypeKind.nonNull,
        resolve = function(kind)
          if kind.__type == 'Scalar' then
            return 'SCALAR'
          elseif kind.__type == 'Object' then
            return 'OBJECT'
          elseif kind.__type == 'Interface' then
            return 'INTERFACE'
          elseif kind.__type == 'Union' then
            return 'UNION'
          elseif kind.__type == 'Enum' then
            return 'ENUM'
          elseif kind.__type == 'InputObject' then
            return 'INPUT_OBJECT'
          elseif kind.__type == 'List' then
            return 'LIST'
          elseif kind.__type == 'NonNull' then
            return 'NON_NULL'
          end

          error('Unknown type ' .. kind)
        end
      },

      fields = {
        kind = types.list(types.nonNull(__Field)),
        arguments = {
          includeDeprecated = {
            kind = types.boolean,
            defaultValue = false
          }
        },
        resolve = function(kind, arguments)
          if kind.__type == 'Object' or kind.__type == 'Interface' then
            return util.filter(util.values(kind.fields), function(field)
              return arguments.includeDeprecated or field.deprecationReason == nil
            end)
          end

          return nil
        end
      },

      interfaces = {
        kind = types.list(types.nonNull(__Type)),
        resolve = function(kind)
          if kind.__type == 'Object' then
            return kind.interfaces
          end
        end
      },

      possibleTypes = {
        kind = types.list(types.nonNull(__Type)),
        resolve = function(kind, arguments, context)
          if kind.__type == 'Interface' or kind.__type == 'Union' then
            return context.schema:getPossibleTypes(kind)
          end
        end
      },

      enumValues = {
        kind = types.list(types.nonNull(__EnumValue)),
        arguments = {
          includeDeprecated = { kind = types.boolean, defaultValue = false }
        },
        resolve = function(kind, arguments)
          if kind.__type == 'Enum' then
            return util.filter(util.values(kind.values), function(value)
              return arguments.includeDeprecated or not value.deprecationReason
            end)
          end
        end
      },

      inputFields = {
        kind = types.list(types.nonNull(__InputValue)),
        resolve = function(kind)
          if kind.__type == 'InputObject' then
            return util.values(kind.fields)
          end
        end
      },

      ofType = {
        kind = __Type
      }
    }
  end
})

__Field = types.object({
  name = '__Field',

  description = util.trim [[
    Object and Interface types are described by a list of Fields, each of
    which has a name, potentially a list of arguments, and a return type.
  ]],

  fields = function()
    return {
      name = types.string.nonNull,
      description = types.string,

      args = {
        -- kind = types.list(__InputValue),
        kind = types.nonNull(types.list(types.nonNull(__InputValue))),
        resolve = function(field)
          return util.map(field.arguments or {}, function(a, n)
            if a.__type then
              return { kind = a, name = n }
            else
              if not a.name then
                local r = { name = n }

                for k,v in pairs(a) do
                  r[k] = v
                end

                return r
              else
                return a
              end
            end
          end)
        end
      },

      type = {
        kind = __Type.nonNull,
        resolve = function(field)
          return field.kind
        end
      },

      isDeprecated = {
        kind = types.boolean.nonNull,
        resolve = function(field)
          return field.deprecationReason ~= nil
        end
      },

      deprecationReason = types.string
    }
  end
})

__InputValue = types.object({
  name = '__InputValue',

  description = util.trim [[
    Arguments provided to Fields or Directives and the input fields of an
    InputObject are represented as Input Values which describe their type
    and optionally a default value.
  ]],

  fields = function()
    return {
      name = types.string.nonNull,
      description = types.string,

      type = {
        kind = types.nonNull(__Type),
        resolve = function(field)
          return field.kind
        end
      },

      defaultValue = {
        kind = types.string,
        description = 'A GraphQL-formatted string representing the default value for this input value.',
        resolve = function(inputVal)
          return inputVal.defaultValue and tostring(inputVal.defaultValue) -- TODO improve serialization a lot
        end
      }
    }
  end
})

__EnumValue = types.object({
  name = '__EnumValue',

  description = [[
    One possible value for a given Enum. Enum values are unique values, not
    a placeholder for a string or numeric value. However an Enum value is
    returned in a JSON response as a string.
  ]],

  fields = function()
    return {
      name = types.string.nonNull,
      description = types.string,
      isDeprecated = {
        kind = types.boolean.nonNull,
        resolve = function(enumValue) return enumValue.deprecationReason ~= nil end
      },
      deprecationReason = types.string
    }
  end
})

__TypeKind = types.enum({
  name = '__TypeKind',
  description = 'An enum describing what kind of type a given `__Type` is.',
  values = {
    SCALAR = {
      value = 'SCALAR',
      description = 'Indicates this type is a scalar.'
    },

    OBJECT = {
      value = 'OBJECT',
      description = 'Indicates this type is an object. `fields` and `interfaces` are valid fields.'
    },

    INTERFACE = {
      value = 'INTERFACE',
      description = 'Indicates this type is an interface. `fields` and `possibleTypes` are valid fields.'
    },

    UNION = {
      value = 'UNION',
      description = 'Indicates this type is a union. `possibleTypes` is a valid field.'
    },

    ENUM = {
      value = 'ENUM',
      description = 'Indicates this type is an enum. `enumValues` is a valid field.'
    },

    INPUT_OBJECT = {
      value = 'INPUT_OBJECT',
      description = 'Indicates this type is an input object. `inputFields` is a valid field.'
    },

    LIST = {
      value = 'LIST',
      description = 'Indicates this type is a list. `ofType` is a valid field.'
    },

    NON_NULL = {
      value = 'NON_NULL',
      description = 'Indicates this type is a non-null. `ofType` is a valid field.'
    }
  }
})

local Schema = {
  name = '__schema',
  kind = __Schema.nonNull,
  description = 'Access the current type schema of this server.',
  arguments = {},
  resolve = function(_, _, info)
    return info.schema
  end
}

local Type = {
  name = '__type',
  kind = __Type,
  description = 'Request the type information of a single type.',
  arguments = {
    name = types.string.nonNull
  },
  resolve = function(_, arguments, info)
    return info.schema:getType(arguments.name)
  end
}

local TypeName = {
  name = '__typename',
  kind = types.string.nonNull,
  description = 'The name of the current Object type at runtime.',
  arguments = {},
  resolve = function(_, _, info)
    return info.parentType.name
  end
}

return {
  __Schema = __Schema,
  __Directive = __Directive,
  __DirectiveLocation = __DirectiveLocation,
  __Type = __Type,
  __Field = __Field,
  __EnumValue = __EnumValue,
  __TypeKind = __TypeKind,
  Schema = Schema,
  Type = Type,
  TypeName = TypeName,
  fieldMap = {
    __schema = Schema,
    __type = Type,
    __typename = TypeName
  }
}
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
assert(not sources["graphql.execute"],"module already exists")sources["graphql.execute"]=([===[-- <pack graphql.execute> --
local path = (...):gsub('%.[^%.]+$', '')
local types = require(path .. '.types')
local util = require(path .. '.util')
local introspection = require(path .. '.introspection')

local function typeFromAST(node, schema)
  local innerType
  if node.kind == 'listType' then
    innerType = typeFromAST(node.type)
    return innerType and types.list(innerType)
  elseif node.kind == 'nonNullType' then
    innerType = typeFromAST(node.type)
    return innerType and types.nonNull(innerType)
  else
    assert(node.kind == 'namedType', 'Variable must be a named type')
    return schema:getType(node.name.value)
  end
end

local function getFieldResponseKey(field)
  return field.alias and field.alias.name.value or field.name.value
end

local function shouldIncludeNode(selection, context)
  if selection.directives then
    local function isDirectiveActive(key, _type)
      local directive = util.find(selection.directives, function(directive)
        return directive.name.value == key
      end)

      if not directive then return end

      local ifArgument = util.find(directive.arguments, function(argument)
        return argument.name.value == 'if'
      end)

      if not ifArgument then return end

      return util.coerceValue(ifArgument.value, _type.arguments['if'], context.variables)
    end

    if isDirectiveActive('skip', types.skip) then return false end
    if isDirectiveActive('include', types.include) == false then return false end
  end

  return true
end

local function doesFragmentApply(fragment, type, context)
  if not fragment.typeCondition then return true end

  local innerType = typeFromAST(fragment.typeCondition, context.schema)

  if innerType == type then
    return true
  elseif innerType.__type == 'Interface' then
    local implementors = context.schema:getImplementors(innerType.name)
    return implementors and implementors[type]
  elseif innerType.__type == 'Union' then
    return util.find(innerType.types, function(member)
      return member == type
    end)
  end
end

local function mergeSelectionSets(fields)
  local selections = {}

  for i = 1, #fields do
    local selectionSet = fields[i].selectionSet
    if selectionSet then
      for j = 1, #selectionSet.selections do
        table.insert(selections, selectionSet.selections[j])
      end
    end
  end

  return selections
end

local function defaultResolver(object, arguments, info)
  return object[info.fieldASTs[1].name.value]
end

local function buildContext(schema, tree, rootValue, variables, operationName)
  local context = {
    schema = schema,
    rootValue = rootValue,
    variables = variables,
    operation = nil,
    fragmentMap = {}
  }

  for _, definition in ipairs(tree.definitions) do
    if definition.kind == 'operation' then
      if not operationName and context.operation then
        error('Operation name must be specified if more than one operation exists.')
      end

      if not operationName or definition.name.value == operationName then
        context.operation = definition
      end
    elseif definition.kind == 'fragmentDefinition' then
      context.fragmentMap[definition.name.value] = definition
    end
  end

  if not context.operation then
    if operationName then
      error('Unknown operation "' .. operationName .. '"')
    else
      error('Must provide an operation')
    end
  end

  return context
end

local function collectFields(objectType, selections, visitedFragments, result, context)
  for _, selection in ipairs(selections) do
    if selection.kind == 'field' then
      if shouldIncludeNode(selection, context) then
        local name = getFieldResponseKey(selection)
        result[name] = result[name] or {}
        table.insert(result[name], selection)
      end
    elseif selection.kind == 'inlineFragment' then
      if shouldIncludeNode(selection, context) and doesFragmentApply(selection, objectType, context) then
        collectFields(objectType, selection.selectionSet.selections, visitedFragments, result, context)
      end
    elseif selection.kind == 'fragmentSpread' then
      local fragmentName = selection.name.value
      if shouldIncludeNode(selection, context) and not visitedFragments[fragmentName] then
        visitedFragments[fragmentName] = true
        local fragment = context.fragmentMap[fragmentName]
        if fragment and shouldIncludeNode(fragment, context) and doesFragmentApply(fragment, objectType, context) then
          collectFields(objectType, fragment.selectionSet.selections, visitedFragments, result, context)
        end
      end
    end
  end

  return result
end

local evaluateSelections

local function completeValue(fieldType, result, subSelections, context)
  local fieldTypeName = fieldType.__type

  if fieldTypeName == 'NonNull' then
    local innerType = fieldType.ofType
    local completedResult = completeValue(innerType, result, subSelections, context)

    if completedResult == nil then
      error('No value provided for non-null ' .. (innerType.name or innerType.__type))
    end

    return completedResult
  end

  if result == nil then
    return nil
  end

  if fieldTypeName == 'List' then
    local innerType = fieldType.ofType

    if type(result) ~= 'table' then
      error('Expected a table for ' .. innerType.name .. ' list')
    end

    local values = {}
    for i, value in ipairs(result) do
      values[i] = completeValue(innerType, value, subSelections, context)
    end

    return values
  end

  if fieldTypeName == 'Scalar' or fieldTypeName == 'Enum' then
    return fieldType.serialize(result)
  end

  if fieldTypeName == 'Object' then
    return evaluateSelections(fieldType, result, subSelections, context)
  elseif fieldTypeName == 'Interface' or fieldTypeName == 'Union' then
    local objectType = fieldType.resolveType(result)
    return evaluateSelections(objectType, result, subSelections, context)
  end

  error('Unknown type "' .. fieldTypeName .. '" for field "' .. field.name .. '"')
end

local function getFieldEntry(objectType, object, fields, context)
  local firstField = fields[1]
  local fieldName = firstField.name.value
  local responseKey = getFieldResponseKey(firstField)
  local fieldType = introspection.fieldMap[fieldName] or objectType.fields[fieldName]

  if fieldType == nil then
    return nil
  end

  local argumentMap = {}
  for _, argument in ipairs(firstField.arguments or {}) do
    argumentMap[argument.name.value] = argument
  end

  local arguments = util.map(fieldType.arguments or {}, function(argument, name)
    local supplied = argumentMap[name] and argumentMap[name].value
    return supplied and util.coerceValue(supplied, argument, context.variables) or argument.defaultValue
  end)

  local info = {
    fieldName = fieldName,
    fieldASTs = fields,
    returnType = fieldType.kind,
    parentType = objectType,
    schema = context.schema,
    fragments = context.fragmentMap,
    rootValue = context.rootValue,
    operation = context.operation,
    variableValues = context.variables
  }

  local resolvedObject = (fieldType.resolve or defaultResolver)(object, arguments, info)
  local subSelections = mergeSelectionSets(fields)

  return completeValue(fieldType.kind, resolvedObject, subSelections, context)
end

evaluateSelections = function(objectType, object, selections, context)
  local groupedFieldSet = collectFields(objectType, selections, {}, {}, context)

  return util.map(groupedFieldSet, function(fields)
    return getFieldEntry(objectType, object, fields, context)
  end)
end

return function(schema, tree, rootValue, variables, operationName)
  local context = buildContext(schema, tree, rootValue, variables, operationName)
  local rootType = schema[context.operation.operation]

  if not rootType then
    error('Unsupported operation "' .. context.operation.operation .. '"')
  end

  return evaluateSelections(rootType, rootValue, context.operation.selectionSet.selections, context)
end
]===]):gsub('\\([%]%[]===)\\([%]%[])','%1%2')
local loadstring=_G.loadstring or _G.load; local preload = require"package".preload
local add = function(name, rawcode)
	if not preload[name] then
	        preload[name] = function(...) return assert(loadstring(rawcode), "loadstring: "..name.." failed")(...) end
	else
		print("WARNING: overwrite "..name)
	end
end
for name, rawcode in pairs(sources) do add(name, rawcode, priorities[name]) end
end; --}};
do -- preload auto aliasing...
	local p = require("package").preload
	for k,v in pairs(p) do
		if k:find("%.init$") then
			local short = k:gsub("%.init$", "")
			if not p[short] then
				p[short] = v
			end
		end
	end
end
local path = (...):gsub('%.init$', '')

local graphql = {}

graphql.parse = require(path .. '.parse')
graphql.types = require(path .. '.types')
graphql.schema = require(path .. '.schema')
graphql.validate = require(path .. '.validate')
graphql.execute = require(path .. '.execute')

return graphql

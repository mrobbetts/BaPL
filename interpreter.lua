lpeg = require "lpeg"

-- Indent a string by two spaces.
function indent(s)
  return "  " .. s:gsub("\n", "\n  ")
end

-- Quick printTable function. Thought it would be fun to write one. Imperfect
--                            but serviceable.
function printTable(tbl)
  s = ""
  for k, v in pairs(tbl) do
    if type(v) == "table" then
      s = s .. "["..k.."]" .. " = {\n" .. indent(printTable(v)) .. "}\n"
    else
      s = s .. "["..k.."]" .. " = " .. tostring(v) .. "\n"
    end
  end
  return s
end

function foldBin(lst)
  local tree = lst[1]
  for i = 2, #lst, 2 do
    tree = {
      tag = "binop",
      e1 = tree,
      op = lst[i],
      e2 = lst[i + 1]
    }
  end
  return tree
end

function foldIndex(lst)
  local tree = lst[1]
  for i = 2, #lst do
    tree = {
      tag = "indexed",
      array = tree,
      index = lst[i]
    }
  end
  return tree
end

-- Encode all array dimensions for a new expression into successive ast node
-- branches.
--
-- A `new` node will always have `tag` and `exp` fields, describing that it is
-- a new array and its size. It may optionally have a subdim field, which will
-- be another `new` node.
function foldNew(fst, ...)
  if select('#', ...) == 0 then
    return {
      tag = "new",
      exp = fst,
    }
  else
    return {
      tag = "new",
      exp = fst,
      subdim = foldNew(...)
    }
  end
end

function seqNode(st1, st2)
  if (st2 == nil) then
    return st1
  else
    return {
      tag = "seq",
      st1 = st1,
      st2 = st2
    }
  end
end

function node(tag, ...)
  local labels = table.pack(...)
  return function (...)
    local params = table.pack(...)

    -- Construct a table with the right tag, then zip together
    -- the labels and params.
    r = { ["tag"] = tag }
    for k, v in ipairs(labels) do
      r[v] = params[k] -- Kindy kludgy? I'm sure there's a neater way.
    end

    return r
  end
end

-- Determine the line within `str` of character index `n`.
function getLineOf(str, n)
  -- Find the first newline in str.
  i = string.find(str, "\n", 1)
  if i and n >= i then
    -- The newline was before character n; recurse.
    return 1 + getLineOf(string.sub(str, i + 1), n - (i + 1))
  else
    -- Character n was before any newline; sentinel.
    return 1
  end
end

extent = 0
function posCheck(pattern, i)
  extent = math.max(extent, i)
  return true
end

lineComment = "#" * (lpeg.P(1) - "\n")^0
blockComment = "#{" * (lpeg.P(1) - "#}")^0 * "#}"

space = (lpeg.S(" \t\n")
         + blockComment
         + lineComment)^0
      * lpeg.P(posCheck)

digit = lpeg.R("09")^1
hexNumeral = lpeg.R("09", "af")^1
decimal = digit * (lpeg.P(".")  * digit)^-1
scient  = decimal * (lpeg.S("eE") * digit)^-1
number = ((lpeg.P("0x") * (hexNumeral / function(x) return tonumber(x, 16) end / node("number", "val"))) + (scient / tonumber / node("number", "val"))) * space

alpha = lpeg.R("az", "AZ")
alphanum  = alpha + digit
IDStarter = alpha + lpeg.S("_")
IDValids  = alphanum + lpeg.S("_")

-- List of reserved words.
reservedWords = { "return", "if", "elseif", "else", "while", "and", "or", "new", "function" }

-- Build a pattern to match any reserved word.
reservedPattern = lpeg.P(false)
for _, w in ipairs(reservedWords) do
  reservedPattern = reservedPattern + w
end
-- Follow the matching pattern with a -alphanum to exclude IDs that simply
-- start with a reserved word.
reservedPattern = reservedPattern * -alphanum

-- Function to match the string `w` against any reserved word (simply return
-- `true` or `false` based on whether it matches)
function isReserved(w, i)

  return reservedPattern:match(string.sub(w, i)) and true
end

ID = (lpeg.C(IDStarter * IDValids^0) - lpeg.P(isReserved)) * space

-- Return a general token pattern based on string `t`.
function T(t)
  return lpeg.P(t) * space
end

-- Return a reserved-word pattern based on the string `w`.
function Rw(w)
  -- print("Reserved word pattern: " .. w)
  assert(reservedPattern:match(w))
  return lpeg.P(w) * -alphanum * space
end

var = ID / node("variable", "val")

opU = lpeg.C(lpeg.S("-!")) * space
opA = lpeg.C(lpeg.S("+-")) * space
opM = lpeg.C(lpeg.S("*/%")) * space
opE = lpeg.C(lpeg.S("^")) * space
opC = lpeg.C(lpeg.P("<=") + "<" + ">=" + ">" + "!=" + "==") * space

new1 = lpeg.V("new1")
lhs  = lpeg.V("lhs")
fact = lpeg.V("fact")
expU = lpeg.V("expU")
expE = lpeg.V("expE")
expM = lpeg.V("expM")
expA = lpeg.V("expA")
expC = lpeg.V("expC")
expL = lpeg.V("expL")
expr = lpeg.V("expr")
if1  = lpeg.V("if1")
elif = lpeg.V("elif")
while1 = lpeg.V("while1")
block = lpeg.V("block")
statement = lpeg.V("statement")
statements = lpeg.V("statements")
prog = lpeg.V("prog")
funcDec = lpeg.V("funcDec")
params = lpeg.V("params")
args = lpeg.V("args")
call = lpeg.V("call")

grammar = lpeg.P{
  "prog",
  -- prog = space * statements * -1,
  prog = space * lpeg.Ct(funcDec^1) * -1,
  funcDec = Rw("function") * ID * T("(") * params * T(")") * (block + T(";")) / node("function", "name", "params", "body"),
  params = lpeg.Ct((ID * (T(",") * ID)^0)^-1),
  fact = number
       + (T("(") * expr * T(")"))
       + call
       + lhs,
  expU = ((opU * fact)                  / node("unop", "op", "exp")) + fact,
  expE = lpeg.Ct(expU * (opE * expU)^0) / foldBin,
  expM = lpeg.Ct(expE * (opM * expE)^0) / foldBin,
  expA = lpeg.Ct(expM * (opA * expM)^0) / foldBin,
  expC = lpeg.Ct(expA * (opC * expA)^0) / foldBin,
  expL = ((expC * (Rw("and") * expL)) / node("and1", "e1", "e2"))
       + ((expC * (Rw("or")  * expL)) / node("or1", "e1", "e2"))
       + expC,
  expr = expL,
  elif = ((Rw("elseif") * expr * block) * (elif + Rw("else") * block)^-1) / node("if1", "cond", "th", "el"),
  if1  = ((Rw("if")     * expr * block) * (elif + Rw("else") * block)^-1) / node("if1", "cond", "th", "el"),
  while1 = (Rw("while") * expr * block) / node("while1", "cond", "body"),
  lhs  = lpeg.Ct(var * (T("[") * expr * T("]"))^0) / foldIndex,
  new1 = (Rw("new")  * (T("[") * expr * T("]"))^0) / foldNew,
  call = ID * T("(") * args * T(")") / node("call", "funName", "args"),
  args = lpeg.Ct((expr * (T(",") * expr)^0)^-1),
  block = T("{") * statements * T("}") / node("block", "body"),
  statement = block
            + (T("var") * ID * (T("=") * (new1 + expr))^-1) / node("local", "name", "init") -- [Ex]
            + if1
            + while1
            + ((T("@") * expr)          / node("print", "exp"))
            + call
            + ((lhs * T("=") * (new1 + expr)) / node("assgn", "lhs", "exp"))
            + ((Rw("return") * expr)    / node("ret", "exp")),
  statements = (statement * (T(";") * statements)^-1 * T(";")^-1) / seqNode
}


function parse(code)
  ast = grammar:match(code)
  if not ast then
    print("error, at " .. extent .. " (line " .. getLineOf(code, extent) .. ")")
    os.exit(1)
  else
    return ast
  end
end

-- Begin Compiler type definition
Compiler = {
  funcs = {},
  vars = {},
  locals = {},
  numVars = 0,
  unops = {
    ["-"] = "neg",
    ["!"] = "not"
  },
  binops = {
    ["+"] = "add",
    ["-"] = "sub",
    ["*"] = "mul",
    ["/"] = "div",
    ["%"] = "mod",
    ["^"] = "exp",
    ["<"]  = "lt",
    ["<="] = "lte",
    [">"]  = "gt",
    [">="] = "gte",
    ["=="] = "eq",
    ["!="] = "neq"
  }
}

function Compiler:addCode(op)
  local code = self.code
  code[#code + 1] = op
end

function Compiler:addJump(op)
  self:addCode(op)
  self:addCode("xxx")
  return #(self.code)
end

-- Recursively convert the tree of multiple array subdimensions into a sequence
-- of push intructions, and return the number of dimensions we found/traversed.
function Compiler:addDims(ast)
  if ast.subdim then
    n = 1 + self:addDims(ast.subdim)
    self:codeExp(ast.exp)
    return n
  else
    self:codeExp(ast.exp)
    return 1
  end
end

function Compiler:idInUse(id)
  index = self.vars[id]
  if not index then
    return false
  else
    return true
  end
end

function Compiler:id2num(id)
  if not self:idInUse(id) then
    if self.funcs[id] then
      error("Global variable '" .. id .. "' collides with function of the same name.")
    end
    self.numVars = self.numVars + 1
    self.vars[id] = self.numVars
  end
  return self.vars[id]
end

function Compiler:findLocal(name)
  for i = #self.locals, 1, -1 do
    if self.locals[i] == name then
      return i
    end
  end

  for i = 1, #self.params do
    if name == self.params[i] then
      return -(#self.params - i)
    end
  end
  return nil
end


-- Encode the root of the AST as an expression.
function Compiler:codeExp(ast)
  if (ast.tag == "number") then
    self:addCode("push")
    self:addCode(ast.val)
  elseif (ast.tag == "binop") then
    self:codeExp(ast.e1)
    self:codeExp(ast.e2)
    self:addCode(self.binops[ast.op])
  elseif (ast.tag == "and1") then
      self:codeExp(ast.e1)
      local l1 = self:addJump("jmpzp")
      self:codeExp(ast.e2)

      -- Fix l1 to jump to the end of the `and`.
      self.code[l1] = #(self.code) - l1
  elseif (ast.tag == "or1") then
      self:codeExp(ast.e1)
      local l1 = self:addJump("jmpnzp")
      self:codeExp(ast.e2)

      -- Fix l1 to jump to the end of the `or`.
      self.code[l1] = #(self.code) - l1
  elseif (ast.tag == "unop") then
    self:codeExp(ast.exp)
    self:addCode(self.unops[ast.op])
  elseif (ast.tag == "variable") then
    local index = self:findLocal(ast.val)
    if index then
      -- Is a local
      self:addCode("loadL")
      self:addCode(index)
    else
      -- Is a global
      self:addCode("load")
      self:addCode(self:id2num(ast.val))
    end
  elseif (ast.tag == "indexed") then
    self:codeExp(ast.array)
    self:codeExp(ast.index)
    self:addCode("array_load")
  elseif (ast.tag == "new") then
    -- Push the individual array dimensions onto the stack, one at a time.
    numDims = self:addDims(ast)

    -- Finally, push the number of dimensions onto the stack, so array_new
    -- can pop them all and implement.
    self:codeExp({ tag = "number", val = numDims})
    self:addCode("array_new")
  elseif (ast.tag == "call") then
    self:codeCall(ast)
  else
    error("Unknown AST node type: " .. ast.tag)
  end
end

function Compiler:codeCall(ast)
  local func = self.funcs[ast.funName]
  if not func then
    error("Call to undefined function " .. ast.funName)
  else

    if #func.params ~= #ast.args then
      error("Incorrect number of arguments given to " .. ast.funName .. "; " .. #ast.args .. " given, " .. #func.params .. " expected.")
    end

    -- Push the function arguments onto the stack prior to the call.
    for i = 1, #ast.args do
      self:codeExp(ast.args[i])
    end

    self:addCode("call")
    self:addCode(func.code)
  end
end


function Compiler:codeAssign(ast)
  local lhs = ast.lhs

  if (lhs.tag == "variable") then
    self:codeExp(ast.exp)
    local idx = self:findLocal(lhs.val)
    if idx then
      self:addCode("storeL")
      self:addCode(idx)
    else
      self:addCode("store")
      self:addCode(self:id2num(lhs.val))
    end
  elseif (lhs.tag == "indexed") then
    self:codeExp(lhs.array)
    self:codeExp(lhs.index)
    self:codeExp(ast.exp)
    self:addCode("array_store")
  else
    error()
  end
end

-- Encode the root of the AST as a statement.
function Compiler:codeStat(ast)
  if (ast.tag == "assgn") then
    self:codeAssign(ast)
  elseif (ast.tag == "seq") then
    self:codeStat(ast.st1)
    self:codeStat(ast.st2)
  elseif (ast.tag == "local") then
    local idx = self:findLocal(ast.name)
    if idx and idx > oldLevel then   -- If idx > oldLevel, the variable has already been declared _in this block_.
      error("Redeclaration of local variable " .. ast.name)
    end
    if (ast.init) then
      self:codeExp(ast.init)
    else
      self:addCode("push")
      self:addCode(0)
    end
    self.locals[#self.locals + 1] = ast.name -- Add accounting for this variable to our local list.
  elseif (ast.tag == "block") then
    self:codeBlock(ast)
  elseif (ast.tag == "call") then
    self:codeCall(ast)
    self:addCode("pop")            -- We need to pop the result of a function
    self:addCode(1 + #self.params) -- call when that call is a statement.
  elseif (ast.tag == "ret") then
    self:codeExp(ast.exp)
    self:addCode("return")
    self:addCode(#self.locals + #self.params)
  elseif (ast.tag == "print") then
    self:codeExp(ast.exp)
    self:addCode("print")
  elseif (ast.tag == "if1") then -- If-then-else

    -- Add the code for the conditional.
    self:codeExp(ast.cond)
    local l1 = self:addJump("jmpz") -- Jump over the Then, if (~cond).

    -- Add the code for the Then.
    self:codeStat(ast.th)

    if (ast.el) then
      -- Jump over (what will be) the Else, now the Then is complete.
      local l2 = self:addJump("jmp")

      -- Fix l1 to jump to the beginning of the Else.
      self.code[l1] = #(self.code) - l1

    -- Now the Else
      self:codeStat(ast.el)

      -- Fix l2 to jump to the end of the Else.
      self.code[l2] = #(self.code) - l2

    else
      -- Fix l1 to jump to the end of the Then.
      self.code[l1] = #(self.code) - l1
    end
  elseif (ast.tag == "while1") then -- While
    local l1 = #self.code -- Save this to jump back to.
    self:codeExp(ast.cond)

    local l2 = self:addJump("jmpz") -- Skip the body if condition is false

    self:codeStat(ast.body)

    -- Jump back to the conditional, to evalute for the next iteration.
    self:addCode("jmp")
    self:addCode(l1 - #(self.code) - 1)

    -- Fix up the l2 jump to land here.
    self.code[l2] = #(self.code) - l2
  else
    error("Unknown AST node type: " .. ast.tag)
  end
end

function Compiler:codeBlock(ast)
  oldLevel = #self.locals -- Make this non-local so that shadowing check can use it from inside codeState. Don't love this data sharing, but is pretty compact!
  self:codeStat(ast.body)
  local diff = #self.locals - oldLevel
  if diff > 0 then
    -- Remove the local variables from the compiler's accounting
    for i = 1, diff do
      table.remove(self.locals)
    end

    -- Pop the local variables from the stack as we exit the block.
    self:addCode("pop")
    self:addCode(diff)
  end
end


function Compiler:codeFunction(ast)
  if not self.funcs[ast.name] then
    -- This is the first time we have seen this function, so we will create an
    -- empty code table for it, and save its params.
    local code = {}
    self.funcs[ast.name] = { code = code, params = ast.params }
    self.code = code
    self.params = ast.params

  else
    -- This function already exists and will have an (empty) code table. We must
    -- use this table as the place to generate code into.
    self.code = self.funcs[ast.name].code
    if #self.params ~= #ast.params then
      error("Redefinition of " .. ast.name .. " with different number of parameters.")
    end
    self.params = ast.params
  end

  -- Encode the function body if it has one; otherwise, we are just noting
  -- it existence (i.e. a forward declaration)
  if ast.body then
    self:codeStat(ast.body)
    self:addCode("push")
    self:addCode("0")
    self:addCode("return")
    self:addCode(#self.locals + #self.params)
  end
end

function Compiler:compile(ast)

  for i = 1, #ast do
    -- Encode the function, unless its name is already taken.
    -- if self.funcs[ast[i].name] then
    thisFun = self.funcs[ast[i].name]
    if thisFun and next(thisFun.code) ~= nil then
      error("Duplicate function name: " .. ast[i].name)
    else
      self:codeFunction(ast[i])
    end
  end

  -- Get the main() function out.
  local main = self.funcs["main"]

  -- Ensure main() exists
  if not main then
    error("No function main()")
  else
    return main
  end
end

function printableValue(v)
  if v == nil then
    return "";
  elseif (type(v) == "table") then
    s = "array[" .. v.size .. "] {" .. printableValue(v[1]);
    for i = 2, #v do
      s = s .. ", " .. printableValue(v[i])
    end
    s = s .. "}"
    return s
  else
    return v
  end
end

-- Recursively generate a multi-dimensional array. Don't love the mechanic of
-- passing in the index explicitly, but I couldn't figure out how to peel off
-- the first element of `dims` in the more classial recursive style.
function generateArray_impl(index, dims)

  if index < #dims then
    local size = dims[index]
    local r = { size = size }
    for i = 1, size do
      r[i] = generateArray_impl(index + 1, dims)
    end
    return r
  else
    return { size = dims[index] }
  end
end

function generateArray(dims)
  return generateArray_impl(1, dims)
end

function run(code, mem, stack, top)
  local pc     = 1
  local logStr = ""
  local base   = top   -- Our current stack frame base.

  while (pc <= #code) do
    if code[pc] == "push" then
      pc  = pc + 1    -- we consume two code words for a push.
      top = top + 1
      stack[top] = code[pc]
      logStr = logStr .. "\npush        " .. code[pc]
    elseif code[pc] == "pop" then
      pc = pc + 1
      local count = code[pc]
      top = top - count
      logStr = logStr .. "\npop         " .. count
    elseif code[pc] == "loadL" then
      pc = pc + 1     -- we consume two code words for a load.
      top = top + 1
      local id = code[pc]
      local val = stack[base + id]
      stack[top] = val;
      logStr = logStr .. "\nloadL       " .. id .. " -> " .. printableValue(val)
    elseif code[pc] == "load" then
      pc = pc + 1     -- we consume two code words for a load.
      top = top + 1
      id = code[pc]
      val = mem[id]
      if #mem < id then
      -- if not val then
        print(printTable(mem))
        error("Variable used before it is defined, at [PC " .. pc .. "] (variable " .. id ..")")
      else
        stack[top] = val
        logStr = logStr .. "\nload        " .. id .. " -> " .. printableValue(val)
      end
    elseif code[pc] == "storeL" then
      pc = pc + 1
      id = code[pc]
      stack[base + id] = stack[top]
      top = top - 1
      logStr = logStr .. "\nstore       " .. id .. " <- " .. printableValue(stack[top + 1])
    elseif code[pc] == "store" then
      pc = pc + 1
      id = code[pc]
      -- print("storing " .. type(stack[top]) .. " to variable " .. id)
      mem[id] = stack[top]
      top = top - 1
      -- logStr = logStr .. "\nstore       " .. code[pc] .. " <- " .. printableValue(mem[id])
      logStr = logStr .. "\nstore       " .. id .. " <- " .. printableValue(stack[top + 1])
    elseif code[pc] == "array_new" then
      -- Gather the array dims
      numDims = stack[top]
      dims = { size = numDims }
      for i = 1, numDims do
        dims[i] = stack[top - i]
      end

      -- We have consumed numDims + 1 values from the stack, but are about to
      -- push another.
      top = top - numDims

      -- Generate
      stack[top] = generateArray(dims)

      logStr = logStr .. "\narray_new   " .. printableValue(dims)
    elseif code[pc] == "array_load" then
      local index = stack[top]
      local array = stack[top - 1]
      if index > array.size then
        error("Index (" .. index .. ") out of range (array size is " .. array.size .. ")")
      else
        logStr = logStr .. "\narray_load  " .. (top - 1) .. "[" .. index .. "]" .. " -> " .. printableValue(array[index])
        top = top - 1
        stack[top] = array[index]
      end
    elseif code[pc] == "array_store" then
      local value = stack[top]
      local index = stack[top - 1]
      local array = stack[top - 2]
      if index > array.size then
        error("Index (" .. index .. ") out of range (array size is " .. array.size .. ")")
      else
        array[index] = value
        top = top - 3
        logStr = logStr .. "\narray_store " .. top + 1 .. "[" .. index .. "]" .. " <- " .. value
      end
    elseif code[pc] == "call" then
      pc = pc + 1
      local code = code[pc]
      logStr = logStr .. "\ncall..."
      local r = run(code, mem, stack, top)
      top = r[1]
      logStr = logStr .. r[2]
    elseif code[pc] == "return" then
      pc = pc + 1
      local count = code[pc]
      logStr = logStr .. "\nreturn  " .. count .. " (" .. tostring(stack[top]) .. ")"
      stack[top - count] = stack[top]
      return {top - count, logStr} -- run() will now operate like a Writer monad.
    elseif code[pc] == "print" then
      logStr = logStr .. "\nprint       \"" .. printableValue(stack[top]) .. "\""
      print(printableValue(stack[top]))
      top = top - 1
    elseif code[pc] == "jmpz" then
      top = top - 1
      -- if (stack[top + 1] == 0) then -- perform the jump / condition is false
      -- if (not stack[top + 1]) then -- perform the jump / condition is false
      if (not stack[top + 1] or stack[top + 1] == 0) then -- perform the jump / condition is false
        logStr = logStr .. "\njmp         " .. code[pc + 1]
        pc = pc + code[pc + 1] + 1
      else
        logStr = logStr .. "\nnop"
        pc = pc + 1
      end
    elseif code[pc] == "jmp" then
      logStr = logStr .. "\njmp         " .. code[pc + 1]
      pc = pc + code[pc + 1] + 1
    elseif code[pc] == "jmpzp" then
      -- Implements short-circuiting `and`. Evaluates first operand, and:
      -- - If non-false, pops the result and continues (to second operand).
      -- - If false, leaves the result in place and jumps over the second.
      if (not stack[top] or stack[top] == 0) then
        -- Perform the jump to the address in the next code word.
        logStr = logStr .. "\njmpzp       " .. code[pc + 1]
        pc = pc + code[pc + 1] + 1
      else
        -- Pop the stack (we know is non-false) and continue.
        logStr = logStr .. "\njmpzp       nop"
        top = top - 1
        pc = pc + 1
      end
    elseif code[pc] == "jmpnzp" then
      -- Implements short-circuiting `or`. Evaluates first operand, and:
      -- - If non-false, leaves the result in place and jumps over the second.
      -- - If false, pops the result and continues (to second operand).
      if (stack[top] and stack[top] ~= 0) then
        -- Perform the jump to the address in the next code word.
        logStr = logStr .. "\njmpnzp      " .. code[pc + 1]
        pc = pc + code[pc + 1] + 1
      else
        -- Pop the stack (we know is non-false) and continue.
        logStr = logStr .. "\njmpzp       nop"
        top = top - 1
        pc = pc + 1
      end
    elseif code[pc] == "add" then
      top = top - 1
      logStr = logStr .. "\nadd         " .. stack[top] .. " + " .. stack[top + 1]
      stack[top] = stack[top] + stack[top + 1]
    elseif code[pc] == "sub" then
      top = top - 1
      logStr = logStr .. "\nsub         " .. stack[top] .. " - " .. stack[top + 1]
      stack[top] = stack[top] - stack[top + 1]
    elseif code[pc] == "mul" then
      top = top - 1
      logStr = logStr .. "\nmul         " .. stack[top] .. " * " .. stack[top + 1]
      stack[top] = stack[top] * stack[top + 1]
    elseif code[pc] == "div" then
      top = top - 1
      logStr = logStr .. "\ndiv:        " .. stack[top] .. " / " .. stack[top + 1]
      stack[top] = stack[top] / stack[top + 1]
    elseif code[pc] == "mod" then
      top = top - 1
      stack[top] = stack[top] % stack[top + 1]
      logStr = logStr .. "\nmod         " .. stack[top] .. " % " .. stack[top + 1]
    elseif code[pc] == "exp" then
      top = top - 1
      logStr = logStr .. "\nexp         " .. stack[top] .. " ^ " .. stack[top + 1]
      stack[top] = stack[top] ^ stack[top + 1]
    elseif code[pc] == "neg" then
      logStr = logStr .. "\nneg         " .. "-" .. stack[top]
      stack[top] = -stack[top]
    elseif code[pc] == "not" then
      logStr = logStr .. "\nnot         " .. "!(" .. stack[top] .. ")"
      if (not stack[top]) then stack[top] = 1 else stack[top] = 0 end
    elseif code[pc] == "lte" then
      top = top - 1
      logStr = logStr .. "\nlte         " .. stack[top] .. " <= " .. stack[top + 1]
      stack[top] = stack[top] <= stack[top + 1]
    elseif code[pc] == "lt" then
      top = top - 1
      logStr = logStr .. "\nlt          " .. stack[top] .. " < " .. stack[top + 1]
      stack[top] = stack[top] < stack[top + 1]
    elseif code[pc] == "gte" then
      top = top - 1
      logStr = logStr .. "\ngte          " .. stack[top] .. " >= " .. stack[top + 1]
      stack[top] = stack[top] >= stack[top + 1]
    elseif code[pc] == "gt" then
      top = top - 1
      logStr = logStr .. "\ngt          " .. stack[top] .. " > " .. stack[top + 1]
      stack[top] = stack[top] > stack[top + 1]
    elseif code[pc] == "neq" then
      top = top - 1
      logStr = logStr .. "\nneq         " .. stack[top] .. " != " .. stack[top + 1]
      stack[top] = stack[top] ~= stack[top + 1]
    elseif code[pc] == "eq" then
      top = top - 1
      logStr = logStr .. "\neq          " .. stack[top] .. " == " .. stack[top + 1]
      stack[top] = stack[top] == stack[top + 1]
    else
      error("Unknown instruction: " .. code[pc])
    end
    pc = pc + 1 -- Consume the code word for this instruction.
  end
  return {top, logStr}
end

input = io.read("*all")
print("INPUT STRING\n" .. input .. "\n")

ast = parse(input)
print("AST\n" .. printTable(ast) .. "\n")

compiler = Compiler
local mainFunc = compiler:compile(ast)
local mainCode = mainFunc.code

print("CODE\n".. printTable(mainCode) .. "\n")

stack = {}
mem   = {}
local output = run(mainCode, mem, stack, 0)
-- print(printTable(output))

local top    = output[1]
local logStr = output[2]
print("\nRESULT\n" .. tostring(stack[top]))

-- print("stack: " .. printTable(stack))

print("\nINSTRUCTION LOG", logStr)

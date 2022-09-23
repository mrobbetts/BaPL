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
reservedWords = { "return", "if", "elseif", "else", "while", "and", "or" } -- [Ex]

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
-- opL = lpeg.C(lpeg.P("and")) * space       -- [Ex]

fact = lpeg.V("fact")
expU = lpeg.V("expU")
expE = lpeg.V("expE")
expM = lpeg.V("expM")
expA = lpeg.V("expA")
expC = lpeg.V("expC")
expL = lpeg.V("expL")                      -- [Ex]
expr = lpeg.V("expr")                      -- [Ex]
if1  = lpeg.V("if1")
elif = lpeg.V("elif")
while1 = lpeg.V("while1")
block = lpeg.V("block")
statement = lpeg.V("statement")
statements = lpeg.V("statements")

grammar = lpeg.P{
  "statements",
  fact = number + (T("(") * expr * T(")")) + var,
  expU = ((opU * fact)                  / node("unop", "op", "exp")) + fact,
  expE = lpeg.Ct(expU * (opE * expU)^0) / foldBin,
  expM = lpeg.Ct(expE * (opM * expE)^0) / foldBin,
  expA = lpeg.Ct(expM * (opA * expM)^0) / foldBin,
  expC = lpeg.Ct(expA * (opC * expA)^0) / foldBin,
  expL = ((expC * (Rw("and") * expL)) / node("and1", "e1", "e2")) + ((expC * (Rw("or") * expL)) / node("or1", "e1", "e2")) + expC,  -- [Ex]
  expr = expL,
  elif = ((Rw("elseif") * expr * block) * (elif + Rw("else") * block)^-1) / node("if1", "cond", "th", "el"),
  if1  = ((Rw("if")     * expr * block) * (elif + Rw("else") * block)^-1) / node("if1", "cond", "th", "el"),
  while1 = (Rw("while") * expr * block) / node("while1", "cond", "body"),
  block = T("{") * statements * T("}"),
  statement = block
            + if1
            + while1
            + ((T("@") * expr)          / node("print", "exp"))
            + ((ID * T("=") * expr)     / node("assgn", "id", "exp"))
            + ((Rw("return") * expr)    / node("ret", "exp")),
  statements = (statement * (T(";") * statements)^-1 * T(";")^-1) / seqNode
}

grammar = grammar * -1


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
  code = {},
  vars = {},
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
    self.numVars = self.numVars + 1
    self.vars[id] = self.numVars
  end
  return self.vars[id]
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
  elseif (ast.tag == "and1") then                -- [Ex]
      self:codeExp(ast.e1)                       -- [Ex]
      local l1 = self:addJump("jmpzp")           -- [Ex]
      self:codeExp(ast.e2)                       -- [Ex]
                                                 -- [Ex]
      -- Fix l1 to jump to the end of the `and`. -- [Ex]
      self.code[l1] = #(self.code) - l1          -- [Ex]
  elseif (ast.tag == "or1") then                 -- [Ex]
      self:codeExp(ast.e1)                       -- [Ex]
      local l1 = self:addJump("jmpnzp")          -- [Ex]
      self:codeExp(ast.e2)                       -- [Ex]
                                                 -- [Ex]
      -- Fix l1 to jump to the end of the `or`.  -- [Ex]
      self.code[l1] = #(self.code) - l1          -- [Ex]
  elseif (ast.tag == "unop") then
    self:codeExp(ast.exp)
    self:addCode(self.unops[ast.op])
  elseif (ast.tag == "variable") then
    self:addCode("load")
    self:addCode(self:id2num(ast.val))
  end
end

-- Encode the root of the AST as a statement.
function Compiler:codeStat(ast)
  if (ast.tag == "assgn") then
    self:codeExp(ast.exp)
    self:addCode("store")
    self:addCode(self:id2num(ast.id))
  elseif (ast.tag == "seq") then
    self:codeStat(ast.st1)
    self:codeStat(ast.st2)
  elseif (ast.tag == "ret") then
    self:codeExp(ast.exp)
    self:addCode("return")
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
  end
end

function Compiler:compile(ast)
  self:codeStat(ast)
  return self.code
end


function run(code, mem, stack)
  pc = 1
  top = 0
  logStr = ""
  while (pc <= #code) do
    if code[pc] == "push" then
      pc  = pc + 1    -- we consume two code words for a push.
      top = top + 1
      stack[top] = code[pc]
      logStr = logStr .. "\npush " .. code[pc]
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
        stack[top] = mem[id]
        logStr = logStr .. "\nload " .. code[pc]
      end
    elseif code[pc] == "store" then
      pc = pc + 1
      id = code[pc]
      mem[id] = stack[top]
      top = top - 1
      logStr = logStr .. "\nstore " .. code[pc]
    elseif code[pc] == "return" then
      logStr = logStr .. "\nreturn " .. tostring(stack[top])
      return logStr
    elseif code[pc] == "print" then
      logStr = logStr .. "\nprint: " .. stack[top]
      print(stack[top])
      top = top - 1
    elseif code[pc] == "jmpz" then
      -- print("jmpz")
      top = top - 1
      -- if (stack[top + 1] == 0) then -- perform the jump / condition is false
      -- if (not stack[top + 1]) then -- perform the jump / condition is false
      if (not stack[top + 1] or stack[top + 1] == 0) then -- perform the jump / condition is false
        logStr = logStr .. "\njmp " .. code[pc + 1]
        pc = pc + code[pc + 1] + 1
      else
        logStr = logStr .. "\nnop "
        pc = pc + 1
      end
      -- print("PC after jump: " .. pc)
    elseif code[pc] == "jmp" then
      logStr = logStr .. "\njmp " .. code[pc + 1]
      pc = pc + code[pc + 1] + 1
      -- print("PC after jump: " .. pc)
    elseif code[pc] == "jmpzp" then                                            -- [Ex]
      -- Implements short-circuiting `and`. Evaluates first operand, and:      -- [Ex]
      -- - If non-false, pops the result and continues (to second operand).    -- [Ex]
      -- - If false, leaves the result in place and jumps over the second.     -- [Ex]
      if (not stack[top] or stack[top] == 0) then                              -- [Ex]
        -- Perform the jump to the address in the next code word.              -- [Ex]
        logStr = logStr .. "\njmpzp: " .. code[pc + 1]                         -- [Ex]
        pc = pc + code[pc + 1] + 1                                             -- [Ex]
      else                                                                     -- [Ex]
        -- Pop the stack (we know is non-false) and continue.                  -- [Ex]
        logStr = logStr .. "\njmpzp: nop"                                      -- [Ex]
        top = top - 1                                                          -- [Ex]
        pc = pc + 1                                                            -- [Ex]
      end                                                                      -- [Ex]
    elseif code[pc] == "jmpnzp" then                                           -- [Ex]
      -- Implements short-circuiting `or`. Evaluates first operand, and:       -- [Ex]
      -- - If non-false, leaves the result in place and jumps over the second. -- [Ex]
      -- - If false, pops the result and continues (to second operand).        -- [Ex]
      if (stack[top] and stack[top] ~= 0) then                                 -- [Ex]
        -- Perform the jump to the address in the next code word.              -- [Ex]
        logStr = logStr .. "\njmpnzp: " .. code[pc + 1]                        -- [Ex]
        pc = pc + code[pc + 1] + 1                                             -- [Ex]
      else                                                                     -- [Ex]
        -- Pop the stack (we know is non-false) and continue.                  -- [Ex]
        logStr = logStr .. "\njmpzp: nop"                                      -- [Ex]
        top = top - 1                                                          -- [Ex]
        pc = pc + 1                                                            -- [Ex]
      end
    elseif code[pc] == "add" then
      top = top - 1
      logStr = logStr .. "\nadd: " .. stack[top] .. " + " .. stack[top + 1]
      stack[top] = stack[top] + stack[top + 1]
    elseif code[pc] == "sub" then
      top = top - 1
      logStr = logStr .. "\nsub: " .. stack[top] .. " - " .. stack[top + 1]
      stack[top] = stack[top] - stack[top + 1]
    elseif code[pc] == "mul" then
      top = top - 1
      logStr = logStr .. "\nmul: " .. stack[top] .. " * " .. stack[top + 1]
      stack[top] = stack[top] * stack[top + 1]
    elseif code[pc] == "div" then
      top = top - 1
      logStr = logStr .. "\ndiv: " .. stack[top] .. " / " .. stack[top + 1]
      stack[top] = stack[top] / stack[top + 1]
    elseif code[pc] == "mod" then
      top = top - 1
      stack[top] = stack[top] % stack[top + 1]
      logStr = logStr .. "\nmod: " .. stack[top] .. " % " .. stack[top + 1]
    elseif code[pc] == "exp" then
      top = top - 1
      logStr = logStr .. "\nexp: " .. stack[top] .. " ^ " .. stack[top + 1]
      stack[top] = stack[top] ^ stack[top + 1]
    elseif code[pc] == "neg" then
      logStr = logStr .. "\nneg: " .. "-" .. stack[top]
      stack[top] = -stack[top]
    elseif code[pc] == "not" then
      logStr = logStr .. "\nnot: " .. "!(" .. stack[top] .. ")"
      -- stack[top] = not stack[top]
      if (not stack[top]) then stack[top] = 1 else stack[top] = 0 end
    elseif code[pc] == "lte" then
      top = top - 1
      logStr = logStr .. "\nlte: " .. stack[top] .. " <= " .. stack[top + 1]
      stack[top] = stack[top] <= stack[top + 1]
    elseif code[pc] == "lt" then
      top = top - 1
      logStr = logStr .. "\nlt: " .. stack[top] .. " < " .. stack[top + 1]
      print("lt: " .. stack[top] .. " < " .. stack[top + 1] .. "?" )
      stack[top] = stack[top] < stack[top + 1]
      print("after lt: " .. tostring(stack[top]))
    elseif code[pc] == "gte" then
      top = top - 1
      logStr = logStr .. "\ngte: " .. stack[top] .. " >= " .. stack[top + 1]
      stack[top] = stack[top] >= stack[top + 1]
    elseif code[pc] == "gt" then
      top = top - 1
      logStr = logStr .. "\ngt: " .. stack[top] .. " < " .. stack[top + 1]
      stack[top] = stack[top] > stack[top + 1]
    elseif code[pc] == "neq" then
      top = top - 1
      logStr = logStr .. "\nneq: " .. stack[top] .. " != " .. stack[top + 1]
      stack[top] = stack[top] ~= stack[top + 1]
    elseif code[pc] == "eq" then
      top = top - 1
      logStr = logStr .. "\neq: " .. stack[top] .. " == " .. stack[top + 1]
      stack[top] = stack[top] == stack[top + 1]
    else
      error("Unknown instruction: " .. code[pc])
    end
    pc = pc + 1 -- Consume the code word for this instruction.
  end
  return logStr
end

input = io.read("*all")
print("INPUT STRING\n" .. input .. "\n")

ast = parse(input)
print("AST\n" .. printTable(ast) .. "\n")

compiler = Compiler
code = compiler:compile(ast)
-- code = compile(ast)
print("CODE\n".. printTable(code) .. "\n")

stack = {}
mem   = {}
logStr = run(code, mem, stack)
print("RESULT\n" .. tostring(stack[1]))

-- print("stack: " .. printTable(stack))

print("\nINSTRUCTION LOG", logStr)

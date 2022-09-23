lpeg = require "lpeg"

-- Parse decimal numbers
function node(num)
  return { tag = "number", val = tonumber(num) }
end

-- Parse hex numbers
function hexNode(num)
  return { tag = "number", val = tonumber(num, 16) }
end

-- Parse variable names
function varNode(var)
  return { tag = "variable", val = var }
end

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
      s = s .. "["..k.."]" .. " = " .. v .. "\n"
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

function unaryNode(op, exp)
  return {
    tag = "unop",
    op = op,
    e  = exp
  }
end

function assignNode(id, exp)
  return {
    tag = "assgn",
    id = id,
    exp = exp
  }
end

function printNode(exp)
  return {
    tag = "print",
    exp = exp
  }
end

function returnNode(exp)
  return {
    tag = "ret",
    exp = exp
  }
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

space = lpeg.S(" \n\t")^0
digit = lpeg.R("09")^1
hexNumeral = lpeg.R("09", "af")^1
decimal = digit * (lpeg.P(".")  * digit)^-1
scient  = decimal * (lpeg.S("eE") * digit)^-1
number = ((lpeg.P("0x") * (hexNumeral / hexNode)) + (scient / node)) * space

alpha = lpeg.R("az", "AZ")
alphanum  = alpha + digit
IDStarter = alpha + lpeg.S("_")
IDValids  = alphanum + lpeg.S("_")
ID = lpeg.C(IDStarter * IDValids^0) * space

var = ID / varNode
assign = space * lpeg.P("=") * space

opP = lpeg.P("(") * space
clP = lpeg.P(")") * space
opB = lpeg.P("{") * space
clB = lpeg.P("}") * space
SC  = (lpeg.P(";") * space)^1
prn = lpeg.P("@") * space
ret = lpeg.P("return") * space


opU = lpeg.C(lpeg.S("-")) * space
opA = lpeg.C(lpeg.S("+-")) * space
opM = lpeg.C(lpeg.S("*/%")) * space
opE = lpeg.C(lpeg.S("^")) * space
opC = lpeg.C(lpeg.P("<=") + "<" + ">=" + ">" + "!=" + "==") * space

fact = lpeg.V("fact")
expU = lpeg.V("expU")
expE = lpeg.V("expE")
expM = lpeg.V("expM")
expA = lpeg.V("expA")
expC = lpeg.V("expC")
block = lpeg.V("block")
statement = lpeg.V("statement")
statements = lpeg.V("statements")
-- ret = lpeg.V("return")

grammar = lpeg.P{
  "statements",
  fact = number + (opP * expC * clP) + var,
  expU = ((opU * fact) / unaryNode) + fact,
  expE = lpeg.Ct(expU * (opE * expU)^0) / foldBin,
  expM = lpeg.Ct(expE * (opM * expE)^0) / foldBin,
  expA = lpeg.Ct(expM * (opA * expM)^0) / foldBin,
  expC = lpeg.Ct(expA * (opC * expA)^0) / foldBin,
  block = opB * statements * clB,
  statement = block
            + ((prn * expC) / printNode)
            + ((ID * assign * expC) / assignNode)
            + (ret * expC) / returnNode,
  statements = (statement * (SC * statements)^-1 * SC^-1) / seqNode
}

grammar = grammar * -1

function parse(code)
  return grammar:match(code)
end

function addCode(state, op)
  local code = state.code
  code[#code + 1] = op
end

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

unops = {
  ["-"] = "neg"
}

function idInUse(state, id)
  index = state.vars[id]
  if not index then
    return false
  else
    return true
  end
end

function id2num(state, id)
  if not idInUse(state, id) then
    state.numVars = state.numVars + 1
    state.vars[id] = state.numVars
  end
  return state.vars[id]
end

function codeExp(state, ast)
  if (ast.tag == "number") then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif (ast.tag == "binop") then
    codeExp(state, ast.e1)
    codeExp(state, ast.e2)
    addCode(state, binops[ast.op])
  elseif (ast.tag == "unop") then
    codeExp(state, ast.e)
    addCode(state, unops[ast.op])
  elseif (ast.tag == "variable") then
    addCode(state, "load")
    addCode(state, id2num(state, ast.val))
  end
end

function codeStat(state, ast)
  if (ast.tag == "assgn") then
    codeExp(state, ast.exp)
    addCode(state, "store")
    addCode(state, id2num(state, ast.id))
  elseif (ast.tag == "seq") then
    codeStat(state, ast.st1)
    codeStat(state, ast.st2)
  elseif (ast.tag == "ret") then
    codeExp(state, ast.exp)
    addCode(state, "return")
  elseif (ast.tag == "print") then
    codeExp(state, ast.exp)
    addCode(state, "print")
  end
end

function compile(ast)
  local state = { code = {}, vars = {}, numVars = 0 }
  codeStat(state, ast)
  return state.code
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
      val = mem[id]                                  -- [Ex]
      if not val then                                -- [Ex]
        error("Variable used before it is defined")  -- [Ex]
      else                                           -- [Ex]
        stack[top] = mem[id]                         -- [Ex]
        logStr = logStr .. "\nload " .. code[pc]     -- [Ex]
      end
    elseif code[pc] == "store" then
      pc = pc + 1
      id = code[pc]
      mem[id] = stack[top]
      top = top - 1
      logStr = logStr .. "\nstore " .. code[pc]
    elseif code[pc] == "return" then
      logStr = logStr .. "\nreturn " .. stack[top]
      return logStr
    elseif code[pc] == "print" then
      logStr = logStr .. "\nprint: " .. stack[top]
      print(stack[top])
      top = top - 1
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
    elseif code[pc] == "lte" then
      top = top - 1
      logStr = logStr .. "\nlte: " .. stack[top] .. " <= " .. stack[top + 1]
      stack[top] = stack[top] <= stack[top + 1]
    elseif code[pc] == "lt" then
      top = top - 1
      logStr = logStr .. "\nlt: " .. stack[top] .. " < " .. stack[top + 1]
      stack[top] = stack[top] < stack[top + 1]
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
      error("Unknown instruction")
    end
    pc = pc + 1 -- Consume the code word for this instruction.
  end
  return logStr
end

input = io.read()
print("INPUT STRING\n" .. input .. "\n")

ast = parse(input)
print("AST\n" .. printTable(ast) .. "\n")

code = compile(ast)
print("CODE\n".. printTable(code) .. "\n")

stack = {}
mem   = {}
logStr = run(code, mem, stack)
print("RESULT\n" .. tostring(stack[1]))

print("\nINSTRUCTION LOG", logStr)

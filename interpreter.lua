lpeg = require "lpeg"

-- Parse decimal numbers
function node(num)
  return { tag = "number", val = tonumber(num) }
end

-- Parse hex numbers
function hexNode(num)
  return { tag = "number", val = tonumber(num, 16) }
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

-- [Part a]
function foldUnary(lst)
  return {
    tag = "unop",
    op = lst[1],
    e  = lst[2]
  }
end

space = lpeg.S(" \n\t")^0
number = ((lpeg.P("0x") * (lpeg.R("09", "af")^1 / hexNode)) + (lpeg.R("09")^1 / node)) * space

opP = lpeg.P("(") * space
clP = lpeg.P(")") * space
opU = lpeg.C(lpeg.S("-")) * space     -- [Part a]
opA = lpeg.C(lpeg.S("+-")) * space
opM = lpeg.C(lpeg.S("*/%")) * space
opE = lpeg.C(lpeg.S("^")) * space
opE = lpeg.C(lpeg.S("^")) * space
opC = lpeg.C(lpeg.P("<=") + "<" + ">=" + ">" + "!=" + "==") * space -- [Part b]

fact = lpeg.V("fact")
expU = lpeg.V("expU")
expE = lpeg.V("expE")
expM = lpeg.V("expM")
expA = lpeg.V("expA")
expC = lpeg.V("expC") -- [Part b]

grammar = lpeg.P{
  "expC",
  fact = number + opP * expC * clP,
  expU = (lpeg.Ct(opU * fact) / foldUnary) + fact,    -- [Part a]
  expE = lpeg.Ct(expU * (opE * expU)^0) / foldBin,
  expM = lpeg.Ct(expE * (opM * expE)^0) / foldBin,
  expA = lpeg.Ct(expM * (opA * expM)^0) / foldBin,
  expC = lpeg.Ct(expA * (opC * expA)^0) / foldBin,    -- [Part b]
}

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
  ["<"]  = "lt",  -- [Part b]
  ["<="] = "lte", -- [Part b]
  [">"]  = "gt",  -- [Part b]
  [">="] = "gte", -- [Part b]
  ["=="] = "eq",  -- [Part b]
  ["!="] = "neq"  -- [Part b]
}

unops = {
  ["-"] = "neg"
}

function codeExp(state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif (ast.tag == "binop") then
    codeExp(state, ast.e1)
    codeExp(state, ast.e2)
    addCode(state, binops[ast.op])
  elseif (ast.tag == "unop") then   -- [Part a]
    codeExp(state, ast.e)           -- [Part a]
    addCode(state, unops[ast.op])   -- [Part a]
  end
end

function compile(ast)
  local state = { code = {} }
  codeExp(state, ast)
  return state.code
end


function run(code, stack)
  pc = 1
  top = 0
  logStr = ""
  while (pc <= #code) do
    if code[pc] == "push" then
      pc  = pc + 1    -- we consume two code words for a push.
      top = top + 1
      stack[top] = code[pc]
      logStr = logStr .. "\npush " .. code[pc]
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
    elseif code[pc] == "neg" then                         -- [Part a]
      logStr = logStr .. "\nneg: " .. "-" .. stack[top]   -- [Part a]
      stack[top] = -stack[top]                            -- [Part a]
    elseif code[pc] == "lte" then                                             -- [Part b]
      top = top - 1                                                           -- [Part b]
      logStr = logStr .. "\nlte: " .. stack[top] .. " <= " .. stack[top + 1]  -- [Part b]
      stack[top] = stack[top] <= stack[top + 1]                               -- [Part b]
    elseif code[pc] == "lt" then                                              -- [Part b]
      top = top - 1                                                           -- [Part b]
      logStr = logStr .. "\nlt: " .. stack[top] .. " < " .. stack[top + 1]    -- [Part b]
      stack[top] = stack[top] < stack[top + 1]                                -- [Part b]
    elseif code[pc] == "gte" then                                             -- [Part b]
      top = top - 1                                                           -- [Part b]
      logStr = logStr .. "\ngte: " .. stack[top] .. " >= " .. stack[top + 1]  -- [Part b]
      stack[top] = stack[top] >= stack[top + 1]                               -- [Part b]
    elseif code[pc] == "gt" then                                              -- [Part b]
      top = top - 1                                                           -- [Part b]
      logStr = logStr .. "\ngt: " .. stack[top] .. " < " .. stack[top + 1]    -- [Part b]
      stack[top] = stack[top] > stack[top + 1]                                -- [Part b]
    elseif code[pc] == "neq" then                                             -- [Part b]
      top = top - 1                                                           -- [Part b]
      logStr = logStr .. "\nneq: " .. stack[top] .. " != " .. stack[top + 1]  -- [Part b]
      stack[top] = stack[top] ~= stack[top + 1]                               -- [Part b]
    elseif code[pc] == "eq" then                                              -- [Part b]
      top = top - 1                                                           -- [Part b]
      logStr = logStr .. "\neq: " .. stack[top] .. " == " .. stack[top + 1]   -- [Part b]
      stack[top] = stack[top] == stack[top + 1]                               -- [Part b]
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
logStr = run(code, stack)
print("RESULT\n" .. tostring(stack[1]))

print("\nINSTRUCTION LOG", logStr)

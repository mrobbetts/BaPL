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

space = lpeg.S(" \n\t")^0
number = ((lpeg.P("0x") * (lpeg.R("09", "af")^1 / hexNode)) + (lpeg.R("09")^1 / node)) * space

opA = lpeg.C(lpeg.S("+-")) * space
-- opM = lpeg.C(lpeg.S("*/")) * space                  -- [Part a]
opM = lpeg.C(lpeg.S("*/%")) * space                    -- [Part a]
opE = lpeg.C(lpeg.S("^")) * space                      -- [Part b]

expE = lpeg.Ct(number * (opE * number)^0) / foldBin
-- expM = lpeg.Ct(number * (opM * number)^0) / foldBin -- [Part b]
expM = lpeg.Ct(expE * (opM * expE)^0) / foldBin        -- [Part b]
expA = lpeg.Ct(expM * (opA * expM)^0) / foldBin


function parse(code)
  return expA:match(code)
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
  ["%"] = "mod", -- [Part a]
  ["^"] = "exp"  -- [Part b]
}

function codeExp(state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif (ast.tag == "binop") then
    codeExp(state, ast.e1)
    codeExp(state, ast.e2)
    addCode(state, binops[ast.op])
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
      pc = pc + 1 -- we consume two code words for a push.
      top = top + 1
      stack[top] = code[pc]
      logStr = logStr .. "\npush " .. code[pc]
    elseif code[pc] == "add" then
      top = top - 1
      stack[top] = stack[top] + stack[top + 1]
      logStr = logStr .. "\nadd"
    elseif code[pc] == "sub" then
      top = top - 1
      stack[top] = stack[top] - stack[top + 1]
      logStr = logStr .. "\nsub"
    elseif code[pc] == "mul" then
      top = top - 1
      stack[top] = stack[top] * stack[top + 1]
      logStr = logStr .. "\nmul"
    elseif code[pc] == "div" then
      top = top - 1
      stack[top] = stack[top] / stack[top + 1]
      logStr = logStr .. "\ndiv"
    elseif code[pc] == "mod" then               -- [Part a]
      top = top - 1                             -- [Part a]
      stack[top] = stack[top] % stack[top + 1]  -- [Part a]
      logStr = logStr .. "\nmod"                -- [Part a]
    elseif code[pc] == "exp" then               -- [Part b]
      top = top - 1                             -- [Part b]
      stack[top] = stack[top] ^ stack[top + 1]  -- [Part b]
      logStr = logStr .. "\nexp"                -- [Part b]
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
print("RESULT\n" .. stack[1])

print("\nINSTRUCTION LOG", logStr)

lpeg = require "lpeg"

function node(num)
  return { tag = "number", val = tonumber(num) }
end

function hexNode(num)
  return { tag = "number", val = tonumber(num, 16) }
end

space = lpeg.S(" \n\t")^0
-- numeral = (lpeg.R("09")^1 / node) * space
numeral = ((lpeg.P("0x") * (lpeg.R("09", "af")^1 / hexNode)) + (lpeg.R("09")^1 / node)) * space

function parse(code)
  return numeral:match(code)
end

function compile(ast)
  if ast.tag == "number" then return { "push", ast.val }
  -- else error("Unknown instruction")
  end
end

function run(code, stack)
  pc = 1
  top = 0
  while (pc <= #code) do
    if code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    else
      error("Unknown instruction")
    end
    pc = pc + 1
  end
end

input = io.read()
ast = parse(input)
code = compile(ast)

stack = {}
result = run(code, stack)
print("Result", stack[1])

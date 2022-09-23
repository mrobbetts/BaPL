lpeg = require "lpeg"
-- pt = require "pt"

space = lpeg.P(" ")^0
numeral = (lpeg.R("09")^1 / tonumber) * space

opE = lpeg.C(lpeg.S("^")) * space
opM = lpeg.C(lpeg.S("*/%")) * space
opA = lpeg.C(lpeg.S("+-")) * space

function fold(lst)
  acc = lst[1]
  for i = 2, #lst, 2 do
    if (lst[i] == "+") then
      acc = acc + lst[i + 1]
    elseif (lst[i] == "-") then
      acc = acc - lst[i + 1]
    elseif (lst[i] == "*") then
      acc = acc * lst[i + 1]
    elseif (lst[i] == "/") then
      acc = acc / lst[i + 1]
    elseif (lst[i] == "%") then
      acc = acc % lst[i + 1]
    elseif (lst[i] == "^") then
      acc = acc ^ lst[i + 1]
    end
  end
  return acc
end

exp = space * lpeg.Ct(numeral * (opE * numeral)^0) / fold
term = space * lpeg.Ct(exp * (opM * exp)^0) / fold
sum = space * (lpeg.Ct(term * (opA * term)^0) / fold) * -lpeg.P(1)

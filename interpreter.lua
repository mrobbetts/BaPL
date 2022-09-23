space = lpeg.P(" ")^0
numeral = lpeg.C(lpeg.S("+-")^-1 * lpeg.R("09")^1) * space
op = lpeg.S("+-/*") * space

p = space * numeral * (lpeg.Cp() * op * numeral)^0 * -lpeg.P(1)

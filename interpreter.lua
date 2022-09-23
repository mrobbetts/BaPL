p = lpeg.P(" ")^0 * lpeg.C(lpeg.R("09")^1) * lpeg.P(" ")^0 * (lpeg.Cp() * lpeg.P("+") * lpeg.P(" ")^0 * lpeg.C(lpeg.R("09")^1) * lpeg.P(" ")^0)^0

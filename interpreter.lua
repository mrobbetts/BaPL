p = lpeg.P(" ")^0 * lpeg.R("09")^1 * lpeg.P(" ")^0 * (lpeg.P("+") * lpeg.P(" ")^0 * lpeg.R("09")^1 * lpeg.P(" ")^0)^0

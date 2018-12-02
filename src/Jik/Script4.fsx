#load "SExpr.fs"

open SExpr



"(a b c . d)" |> stringToSExpr |> parseArgs
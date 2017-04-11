let quadPoly a b c x =
    a*x*x + b*x + c
let quadSoln (a:float) (b:float) (c:float) =
    (-b + (sqrt (b*b - 4.0*a*c)))/(2.0*a)

let propertyOfSolutions (lst : NormalFloat list) =
    match lst with
        | x::y::z::w::[] ->
            let [a;b;c;d] = lst |> List.map NormalFloat.get
            if a = 0.0 || b = 0.0 || c = 0.0 || d = 0.0 || a > 100.0 || b > 100.0 || c > 100.0 || d > 100.0 then true else
            let a' = b*d
            let b' = a*d + b*c
            let c' = a*c
            let soln = quadSoln a' b' c'
            quadPoly a' b' c' soln = 0.0
        | _ -> true
Check.Quick propertyOfSolutions
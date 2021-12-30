module Probably.Utils

let floatFactorial x =
    let rec util (value, acc) =
        match value with
        | 0.0
        | 1.0 -> acc
        | _ -> util (value - 1.0, acc * value)

    util (x, 1.0)


let weightedMean (values: float list) (weights: float list) : float =
    if List.length values <> List.length weights then
        raise (System.ArgumentException("Values and weights length must be equal"))
    else
        [ 0 .. (List.length values) - 1 ]
        |> List.map (fun idx -> values.[idx] * weights.[idx])
        |> List.sum
        |> fun x -> x / (List.sum weights)

let normalize (min: float) (max: float) (x) = (x - min) / (max - min)


let numericalIntegration (lower: float) (upper: float) (slices: int64) f : float =
    let N = float slices
    let diff = upper - lower
    let diffRatio = diff / N

    [ 1.0 .. (N + 1.0) ]
    |> List.reduce (fun acc n -> acc + (f lower + ((n - 0.5) * diffRatio)))
    |> (*) diffRatio


module Probably.Domain.Distributions

open System
open Microsoft.FSharp.Core
open Probably.Domain.Continuous
open Probably.Domain.Discrete

type Distribution =
    | Discrete of Discrete
    | Continuous of Continuous


let numericalIntegration (lower: float) (upper: float) (slices: int64) f : float =
    let N = float slices
    let diff = upper - lower
    let diffRatio = diff / N

    [ 1.0 .. (N + 1.0) ]
    |> List.reduce (fun acc n -> acc + (f lower + ((n - 0.5) * diffRatio)))
    |> (*) diffRatio


module Utils =
    // TODO: median, mode, variance, skewness, entropy
    type pmf = Distribution -> float -> float
    type pdf = Distribution -> float -> float
    type cdf = Distribution -> float -> float
    type mean = Distribution -> float

    let pmf: pmf =
        fun dist x ->
            match dist with
            | Continuous _ -> raise (ArgumentException("Continuous distributions have no mass function"))
            | Discrete disc ->
                match disc with
                | _ -> 0.0

    let pdf: pdf =
        fun dist x ->
            match dist with
            | Distribution.Discrete _ -> raise (ArgumentException("Discrete distributions have no mass function"))
            | Continuous cont ->
                match cont with
                | Uniform D ->
                    if (x < D.min || x > D.max) then
                        let den = D.max - D.min
                        1.0 / den
                    else
                        0.0
                | Beta D ->
                    if (D.alpha + D.beta) = 0.0 then
                        raise (ArgumentException("Alpha and Beta must sum up to more than 0"))
                    else
                        Math.Pow(x, (D.alpha - 1.0))
                        * Math.Pow(1.0 - x, (D.beta - 1.0))
                | _ -> 0.0

    let cdf: cdf =
        fun dist x ->
            match dist with
            | Discrete disc ->
                match disc with
                | Bernoulli disc -> 0.0
                | _ -> 0.0
            | Distribution.Continuous cont ->
                match cont with
                | Uniform D ->
                    if (x < D.min) then 0.0
                    elif (x > D.max) then 1.0
                    else (x - D.min) / (D.max - D.min)
                | Beta D -> numericalIntegration 0.0 x 1000 (pmf dist)
                | _ -> 0.0


    let mean: mean =
        fun dist ->
            match dist with
            | Discrete disc -> 0.0
            | Continuous cont ->
                match cont with
                | Uniform D -> (D.min + D.max) / 0.5
                | Beta D -> D.alpha / (D.alpha + D.beta)
                | _ -> 0.0




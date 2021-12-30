module Probably.Distributions.Discrete

open System
open Probably.Utils

module Bernoulli =
    type Bernoulli = { p: float }

    let create (p: float) : Bernoulli =
        if (p < 0 || p > 1) then
            raise (ArgumentException("p must be >= 0 and <= 1"))
        else
            { p = p }

    let pmf (dist: Bernoulli) (x: float) =
        if (x = 0.0) then
            dist.p
        else
            1.0 - dist.p
            
    let mean (dist: Bernoulli) = dist.p
    
module Binomial =
    type Binomial = { p: float; n: int64 }

    let create (p: float) (n: int64) : Binomial =
        if (p < 0.0 || p > 1.0) then
            raise (ArgumentException("p must be >= 0 and <= 1"))
        elif (n < 1) then
            raise (ArgumentException("number of events must be positive"))
        else
            { p = p; n = n }

module Poisson =
    type Poisson = { lambda: float }

    let create (lambda: float) : Poisson =
        if (lambda < 0) then
            raise (ArgumentException("Lambda must be non negative"))
        else
            { lambda = lambda }
            
    let mean (dist: Poisson) = dist.lambda
    
    let pmf (dist: Poisson) (x: float) =
        let numerator =
            Math.Pow(dist.lambda, x)
            * Math.Pow(Math.E, -dist.lambda)

        let denominator = floatFactorial x
        numerator / denominator
        
    let cdf (dist: Poisson) (x: float) =
        let cumulative =
            [ 0.0 .. floor x ]
            |> List.map (fun i -> Math.Pow(dist.lambda, i) / (floatFactorial i))
            |> List.sum

        Math.Pow(Math.E, -dist.lambda) * cumulative

module Categorical =
    type Categorical =
        { p: float list
          categories: string list }

    let create (p: float list) (categories: string list) : Categorical =
        if (p.Length <> categories.Length) then
            raise (ArgumentException("Probabilities length must equal the length of categories"))
        elif (p
              |> List.filter (fun x -> x < 0.0 || x > 1.0)
              |> List.length > 0) then
            raise (ArgumentException("All probabilities must be between 0 and 1"))
        elif (List.sum p > 1.0) then
            raise (ArgumentException("The sum of probabilities must be <= 1"))
        else
            { p = p; categories = categories }


type Discrete =
    | Bernoulli of Bernoulli.Bernoulli
    | Binomial of Binomial.Binomial
    | Poisson of Poisson.Poisson
    | Categorical of Categorical.Categorical

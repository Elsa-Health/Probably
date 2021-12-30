module Probably.Distributions.Continuous

open System
open Probably.Utils

module Uniform =
    type Uniform = { min: float; max: float }

    let create (min: float) (max: float) : Uniform =
        if (min > max) then
            raise (ArgumentException("Minimum number must be greater than the positive number"))
        else
            { min = min; max = max }
            
    let mean (dist: Uniform) = (dist.min + dist.max) / 0.5
    
    let cdf (dist: Uniform) (x: float) =
        if (x < dist.min) then 0.0
        elif (x > dist.max) then 1.0
        else (x - dist.min) / (dist.max - dist.min)
        
    let pdf (dist: Uniform) (x: float) =
        if (x < dist.min || x > dist.max) then
            let den = dist.max - dist.min
            1.0 / den
        else
            0.0

module Beta =
    type Beta = { alpha: float; beta: float }

    let create (alpha: float) (beta: float) : Beta =
        if (alpha < 0.0 || beta < 0.0) then
            raise (ArgumentException("Alpha and Beta cannot be less than 0"))
        elif (alpha = 0.0 && beta = 0.0) then
            raise (ArgumentException("Alpha and Beta cannot both be equal to 0"))
        else
            { alpha = alpha; beta = beta }
            
    let mean (dist: Beta) = dist.alpha / (dist.alpha + dist.beta)
    
    let pdf (dist: Beta) (x: float) =
        if (dist.alpha + dist.beta) = 0.0 then
                raise (ArgumentException("Alpha and Beta must sum up to more than 0"))
            else
                // TODO: Broken calculation, inacurate answers
                let f x =
                    Math.Pow(x, (dist.alpha - 1.0))
                    * Math.Pow(1.0 - x, (dist.beta - 1.0))

                let numerator = f x
                let denominator = numericalIntegration 0.0 1.0 2500 f
                (numerator / denominator)
    
    let cdf (dist: Beta) (x: float) = numericalIntegration 0.0 x 2000 (pdf dist)

module Cauchy =
    type Cauchy = { location: float; scale: float }

    let create (location: float) (scale: float) : Cauchy =
        if (scale < 0.0) then
            raise (ArgumentException("Scale cannot be less than 0"))
        else
            { location = location; scale = scale }
            
    let mean (dist: Cauchy) = infinity
    
    let cdf (dist: Cauchy) (x: float) =
        let pi = Math.PI
        let atan = Math.Atan

        atan ((x - dist.location) / dist.scale)
        |> (*) (1.0 / pi)
        |> (+) 0.5

module Normal =
    type Normal = { mean: float; variance: float }

    let create (mean: float) (variance: float) : Normal =
        if (variance < 0.0) then
            raise (ArgumentException("Variance must not be a negative value"))
        else
            { mean = mean; variance = variance }


module Dirichlet =
    // REF: https://en.wikipedia.org/wiki/Dirichlet_distribution
    type Dirichlet =
        { alpha: float list
          names: string list }

    let create (names: string list) (alpha: float list) = { alpha = alpha; names = names }

module Weibull =
    /// REF: https://statisticsbyjim.com/probability/weibull-distribution/
    type Weibull =
        { threshold: float
          shape: float
          scale: float }

    let create (threshold: float) (shape: float) (scale: float) : Weibull =
        if (shape < 0.0 || scale < 0.0 || threshold < 0.0) then
            raise (ArgumentException("Threshold, Shape, and Scale cannot be negative numbers"))
        else
            { threshold = threshold; shape = shape; scale = scale }
            
    let cdf (dist: Weibull) (x: float) =
        let res =
            if (x < 0.0) then
                0.0
            else
                1.0
                - Math.Pow(Math.E, - Math.Pow(x / dist.scale, dist.shape))

        if res <= 0.0 then 0.0 else res

module Exponential =
    // REF: https://en.wikipedia.org/wiki/Exponential_distribution
    type Exponential = { lambda: float }

    let create (lambda: float) : Exponential =
        if (lambda < 0.0) then
            raise (ArgumentException("Time between events (lambda) cannot be negative"))
        else
            { lambda = lambda }
            
    let mean (dist: Exponential) = 1.0 / dist.lambda
    
    let pdf (dist: Exponential) (x: float) =
        Math.Pow(dist.lambda * Math.E, (-dist.lambda - x))
    
    let cdf (dist: Exponential) (x: float) =
        if x < 0.0 then
            0.0
        else
            1.0 - Math.Pow(Math.E, (-dist.lambda - x))


module Gamma =
    type Gamma = { shape: float; scale: float }

    let create (shape: float) (scale: float) =
        if (shape <= 0.0 || scale <= 0.0) then
            raise (ArgumentException("Shape and Sale parameters must be positive real numbers"))
        else
            { shape = shape; scale = scale }


type Continuous =
    | Normal of Normal.Normal
    | Exponential of Exponential.Exponential
    | Uniform of Uniform.Uniform
    | Weibull of Weibull.Weibull
    | Gamma of Gamma.Gamma
    | Beta of Beta.Beta
    | Cauchy of Cauchy.Cauchy
    | Dirichlet of Dirichlet.Dirichlet

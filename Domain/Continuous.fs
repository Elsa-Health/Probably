module Probably.Domain.Continuous

open System

module Uniform =
    type Uniform = { min: float; max: float }

    let create (min: float) (max: float) : Uniform =
        if (min > max) then
            raise (ArgumentException("Minimum number must be greater than the positive number"))
        else
            { min = min; max = max }

module Beta =
    type Beta = { alpha: float; beta: float }

    let create (alpha: float) (beta: float) : Beta =
        if (alpha < 0.0 || beta < 0.0) then
            raise (ArgumentException("Alpha and Beta cannot be less than 0"))
        elif (alpha = 0.0 && beta = 0.0) then
            raise (ArgumentException("Alpha and Beta cannot both be equal to 0"))
        else
            { alpha = alpha; beta = beta }

module Cauchy =
    type Cauchy = { location: float; scale: float }

    let create (location: float) (scale: float) : Cauchy =
        if (scale < 0.0) then
            raise (ArgumentException("Scale cannot be less than 0"))
        else
            { location = location; scale = scale }

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

module Exponential =
    // REF: https://en.wikipedia.org/wiki/Exponential_distribution
    type Exponential = { lambda: float }

    let create (lambda: float) : Exponential =
        if (lambda < 0.0) then
            raise (ArgumentException("Time between events (lambda) cannot be negative"))
        else
            { lambda = lambda }


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

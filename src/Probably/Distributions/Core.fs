module Probably.Distributions.Core

open System
open Microsoft.FSharp.Core
open Probably.Distributions.Continuous
open Probably.Distributions.Discrete

type Distribution =
    | Discrete of Discrete
    | Continuous of Continuous

//module Utils =
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
            | Bernoulli D -> Bernoulli.pmf D x
            | Poisson D -> Poisson.pmf D x
            | _ -> raise (NotImplementedException("This method is not implemented"))


let pdf: pdf =
    fun dist x ->
        match dist with
        | Distribution.Discrete _ -> raise (ArgumentException("Discrete distributions have no mass function"))
        | Continuous cont ->
            match cont with
            | Uniform D -> Uniform.pdf D x
            | Beta D -> Beta.pdf D x
            | Exponential D -> Exponential.pdf D x
            | _ -> raise (NotImplementedException("This method is not implemented"))


let cdf: cdf =
    fun dist x ->
        match dist with
        | Discrete disc ->
            match disc with
            | Poisson D -> Poisson.cdf D x
            | _ -> raise (NotImplementedException("This method is not implemented"))
        | Distribution.Continuous cont ->
            match cont with
            | Uniform D -> Uniform.cdf D x
            | Beta D -> Beta.cdf D x
            | Cauchy D -> Cauchy.cdf D x
            | Exponential D -> Exponential.cdf D x
            | Weibull D -> Weibull.cdf D x
            | _ -> raise (NotImplementedException("This method is not implemented"))


let mean: mean =
    fun dist ->
        match dist with
        | Discrete disc ->
            match disc with
            | Bernoulli D -> D.p
            | Poisson D -> D.lambda
            | _ -> raise (NotImplementedException("This method is not implemented"))
        | Continuous cont ->
            match cont with
            | Uniform D -> Uniform.mean D
            | Beta D -> Beta.mean D
            | Cauchy D -> Cauchy.mean D
            | Exponential D -> Exponential.mean D
            | _ -> raise (NotImplementedException("This method is not implemented"))

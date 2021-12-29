namespace Probably

open Probably.Domain.Continuous
open Probably.Domain.Distributions
open Probably.Domain
open Probably.Domain.Distributions.Utils
module Say =
    let dist = Beta.create 100.0 10.0
    let a = Continuous.Beta dist
    let b = Distributions.Continuous a
    
    printfn $"P(x<0.4) = {cdf b 0.4}"
    let hello name =
        printfn "Hello %s" name

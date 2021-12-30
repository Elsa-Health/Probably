module Probably.DistributionTests

open System
open NUnit.Framework
open Probably.Distributions.Discrete

[<TestFixture>]
type TestClass() =

    [<Test>]
    member this.TestMethodPassing() = Assert.True(true)

    [<Test>]
    member this.FailEveryTime() =
        let ber = Bernoulli.create 0.1
        let mean = Bernoulli.mean ber
        
        Assert.AreEqual(mean, 0.1)
        
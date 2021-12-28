module Probably.Continuous

module Uniform =
    type Uniform =
        { name: string
          min: float
          max: float }
        member this.mean = this.min + this.max |> (/) 0.5

        member this.pdf(x: float) : float =
            if (x < this.min || x > this.max) then
                let den = this.max - this.min
                1.0 / den
            else
                0.0

        member this.cdf(x) =
            if (x < this.min) then
                0.0
            elif (x > this.max) then
                1.0
            else
                (x - this.min) / (this.max - this.min)

    let create (name: string) (min: float) (max: float) : Uniform = { name = name; min = min; max = max }


module Beta =

    type Beta<'N> =
        { name: 'N
          alpha: float
          beta: float }
        member this.mean = this.alpha / (this.alpha + this.beta)

    let create (name: 'N) (alpha: float) (beta: float) : Beta<'N> =
        { name = name
          alpha = alpha
          beta = beta }

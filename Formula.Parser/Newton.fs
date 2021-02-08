//-----------------------------------------------------------------------
// <copyright file="Newton.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

module Newton

let calcDerivative (fx: float -> float) x delta k =
    let xNeg = (fx (x - delta)) - k
    let xPos = (fx (x + delta)) - k
    let derivative = (xPos - xNeg) / 2.0
    derivative

let epsilon = 0.0001
let newtonsMethod (fx: float -> float) x delta =
    let mutable k = fx x
    let mutable xNext = x
    let mutable n = 0

    while abs k > epsilon && n < 25 do
        let dx = calcDerivative fx xNext delta k
        xNext <- xNext - ((k / dx) * delta)
        k <- fx xNext
        n <- n + 1
        ()
    xNext
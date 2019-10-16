# debugging notes

1) The power analysis in the shiny interface
sometimes breaks, and does not return any result. I am not sure why this is happening.

2) dataset causes shiny GUI to crash

**file**
multivarRD.csv

**assignment/cutoffs**
outcome: mt
assign1: vp >= 20
assign2: mp <= 5
treatm: trt

"The issue is with error reporting. In your example, the crashes are a result of your design probably not having any observations in some of the quadrants. The software should give you an error message in the design table, and then refuse to estimate anything. I guess it refuses, but doesn't really tell you why."

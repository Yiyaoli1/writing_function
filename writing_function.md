function
================
Yiyao LI
2020/11/11

## start with something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  0.12868385 -1.24666052  0.06432435 -1.03028619 -0.12163866  0.55611795
    ##  [7]  0.88922654 -1.13942151 -0.46761336 -0.39062988  0.97453840 -1.57178403
    ## [13] -0.84263721 -0.08004293  2.41445733 -0.66178891  0.14920791 -0.33914528
    ## [19] -0.09751830 -1.58945670 -0.29816943 -0.99134286  0.90110574 -0.72467247
    ## [25]  1.55072456  1.87526567  0.26156103  1.06744192  0.96101634 -0.20086334

a function to calculate z\_score

``` r
z_score = function(x){
  
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if(length(x)<3){
    stop("Input must have at least 3 numbers")
  }
  z = (x - mean(x))/sd(x)
  
  return(z)
  
}

z_score(x_vec)
```

    ##  [1]  0.12868385 -1.24666052  0.06432435 -1.03028619 -0.12163866  0.55611795
    ##  [7]  0.88922654 -1.13942151 -0.46761336 -0.39062988  0.97453840 -1.57178403
    ## [13] -0.84263721 -0.08004293  2.41445733 -0.66178891  0.14920791 -0.33914528
    ## [19] -0.09751830 -1.58945670 -0.29816943 -0.99134286  0.90110574 -0.72467247
    ## [25]  1.55072456  1.87526567  0.26156103  1.06744192  0.96101634 -0.20086334

try my function on other things, which should have errors.

``` r
z_score(3)
```

    ## Error in z_score(3): Input must have at least 3 numbers

``` r
z_score("my name is yiyao")
```

    ## Error in z_score("my name is yiyao"): Input must be numeric

``` r
z_score(c(TRUE, TRUE, FALSE, FALSE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, FALSE)): Input must be numeric

``` r
z_score(c(1,2))
```

    ## Error in z_score(c(1, 2)): Input must have at least 3 numbers

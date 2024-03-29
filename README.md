
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loopviz

<!-- badges: start -->

<!-- badges: end -->

Visualize nested for loops. Inspiration from [*Programming Loops vs
Recursion*](https://www.youtube.com/watch?v=HXNhEYqFo0o) a youtube video
by
[Computerphile](https://www.youtube.com/channel/UC9-y-6csu5WGm29I7JiwpnA).

## Installation

You can install `loopviz` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tylurp/loopviz")
```

## Example

To plot a nested for loop, provide some numbers to `loopviz`. Each
number represents the number of iterations. The order represents the
hierarchy, i.e. the first number is the top level loop, the second is
the first nested loop and so on. For example:

``` r
library(loopviz)

loopviz(2, 3)
```

<img src="man/figures/README-example-1.png" width="100%" />

Above we have a for loop which iterates 2 times. Inside that loop there
is a nested loop which iterates 3 times. The code would look something
like:

``` r
for (i in 1:2) {
  for (j in 1:3)
    print (j)
}
#> [1] 1
#> [1] 2
#> [1] 3
#> [1] 1
#> [1] 2
#> [1] 3
```

You can let loopviz do the translating with `translate = TRUE`:

``` r
loopviz(2, 3, translate = TRUE)
#> for (i in 1:2) {
#>   for (j in 1:3)
#>     print (j)
#> }
```

Animating loops is also possible thanks to
[gifski](https://github.com/r-rust/gifski):

``` r
loopviz(2, 3, animate = TRUE)
```

<img src="man/figures/animation.gif" width="300px" style="display: block; margin: auto;" />

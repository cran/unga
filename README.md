
<!-- README.md is generated from README.Rmd. Please edit that file -->

# unga

<!-- badges: start -->

<!-- badges: end -->

This package hels you analyzing questionnaires. So far the package has
five main purposes:

1)  to make tables of proportions of every variable in a data frame.
2)  to cross tabulate all variables against an explanatory variable.
3)  to summarize response alternatives of questions in a questionnaire.
4)  to overview the data and the dimensions of all questions,
    i.e. explore how many response alternatives every question has in a
    data frame.
5)  to find the item non response level.

## Installation

You can install the released version of unga from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("unga")
```

## Example

Assume that you want to get a quick overview of your data frame with
respect to how many response alternatives every question have in a
survey. Let’s use the function `table_alt()`. This is a good way to
start when you want to use `table_unga()` and extract questions with a
specific number of response alternatives. Let’s generate some test data
and apply the function `table_alt()` to it.

``` r
library(unga)
set.seed(123456)
gender<-round(runif(50,1,2))
variable_1<-round(runif(50,1,2))
variable_2<-round(runif(50,1,5))
variable_3<-round(runif(50,1,5))
df<-data.frame(gender,variable_1,variable_2,variable_3)

table_alt(df)
#>      2 5
#> Freq 2 2
```

The output here tells you that you have two questions with two response
alternatives, and two questions with 5 response alternatives.

<h2>

Specific cross tabulation

</h2>

Let’s now apply the function `table_unga()` on questions with 5 response
alternatives on it and use `gender` as our explanatory variable.

``` r
table_unga(5,1,gender,df)
#>  variable_2.expvar variable_2.y variable_2.Freq variable_3.Freq
#>                  1            1      0.04761905       0.0000000
#>                  1            2      0.28571429       0.3333333
#>                  1            3      0.47619048       0.0952381
#>                  1            4      0.09523810       0.4285714
#>                  1            5      0.09523810       0.1428571
#>                  2            1      0.24137931       0.1724138
#>                  2            2      0.20689655       0.2068966
#>                  2            3      0.20689655       0.2068966
#>                  2            4      0.20689655       0.2758621
#>                  2            5      0.13793103       0.1379310
```

What we have done is to apply the function `table_unga()` to all
variables in the data frame. We have selected all (hypothetical)
questions with 5 response alternatives (the first argument in the
function) in the data frame and used “gender” as our explanatory
variable (the third argument in the function). The output here is
proportion tables of those questions. The fourth argument is the data
frame.

As described above, the first argument in `table_unga()` let you extract
questions with a specified number of response alternatives. The third
argument is the explanatory variable, and the fourth argument is the
data frame. But what about the second argument? You could easily ignore
this if you want. But it has to be specified to 1 or 2. This will not
affect the output in `Output_1`, i.e. what is printed by `table_unga()`
function. The second argument let you summarize response alternatives,
but only for questions with 5 or 6 response alternatives. The output is
found in `Output_2`. Let us again look at our example but now look on
`Output_2`:

``` r

Output_2
#>   response_alt variable_2.Freq variable_3.Freq
#> 1            1       0.3333333       0.3333333
#> 2            2       0.5714286       0.5238095
#> 3            3       0.0952381       0.1428571
#> 4            1       0.4482759       0.3793103
#> 5            2       0.4137931       0.4827586
#> 6            3       0.1379310       0.1379310
```

This means that you have summarized response alternatives for questions
with 5 response alternatives so now you have only 6 rows instead of 10
rows. The second argument `1` in the function means that you summarize
alternatives according to: `(A+B,C+D,E)`. If you use `2` as input in the
second argument you will summarize alternatives according to:
`(A+B,C,D+E)`.

You can also summarize questions with 6 response alternatives. The
summation will be done according to: `(A+B,C,D+E,F)`. Make sure you
specify your second argument to `1`. The summarized output will again be
found in `Output_2`.

<h2>

General cross tabulation

</h2>

If you want to cross tabulate on all variables in your data frame
without extracting questions with a specific response alternative, use
`table_unga_general()`. This will return a list of contingency tables.

<h2>

Item non response

</h2>

The function `table_non_resp()` let you overview item non response
within a data frame.

``` r

set.seed(123456)
gender<-round(runif(50,1,2))
variable_1<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
variable_2<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
variable_3<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
df<-data.frame(gender,variable_1,variable_2,variable_3)

table_non_resp(df)
#>            missing_1/(missing_1 + not_missing)
#> gender                                    0.00
#> variable_1                                0.10
#> variable_2                                0.12
#> variable_3                                0.14
```

The output shows for example that `variable_1` is missing 10 percent of
items. If you want the absolute values, use the function
`table_non_resp_2()`

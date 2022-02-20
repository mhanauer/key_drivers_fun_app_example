
<!-- README.md is generated from README.Rmd. Please edit that file -->

Description: The goal of the src is to provide two functions and an
application for key drivers using simulated data. The
key_driver_function Rme file contains two functions (1)
key_driver_function and (2) viz_key_driver_function.

Key drivers analysis calculates the importance (e.g., regression
coefficients) and percentage positive responses (e.g., number of
participants who said Strongly Agree or Agree out of all participants).
With the importance and percentage of positive responses.

The key driver function calculates the key drivers using the rwa packge
and has two arguments (1) data and (2) outcome. The data must be
formatted as follows:

1.  Change any Likert Scale responses (e.g., Strongly Agree) to numeric
    scale with the largest value equaling the most positive value. For
    example, change strongly agree to 5 on a five-point Likert scale
    question.

2.  Remove any non-Likert scale variables (e.g., role, binary questions,
    date, ID).

3.  Only include Likert scale questions on a 1 to 5 scale.

The outcome can be an variable.

Here we load the data simulator and the key_driver function. We then
remove the non-Likert scale variables

``` r
library(glue)
setwd("~/key_driver_fun_app_example/src")
source(knitr::purl(glue("~/key_driver_fun_app_example/src/data_generation_a360.Rmd")))
source(knitr::purl(glue("~/key_driver_fun_app_example/src/key_driver_function.Rmd")))
#> Note: Using an external vector in selections is ambiguous.
#> ℹ Use `all_of(outcome)` instead of `outcome` to silence this message.
#> ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
#> This message is displayed once per session.
#> Warning: `add_rownames()` was deprecated in dplyr 1.0.0.
#> Please use `tibble::rownames_to_column()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
#> Warning: `funs()` was deprecated in dplyr 0.8.0.
#> Please use a list of either functions or lambdas: 
#> 
#>   # Simple named list: 
#>   list(mean = mean, median = median)
#> 
#>   # Auto named with `tibble::lst()`: 
#>   tibble::lst(mean, median)
#> 
#>   # Using lambdas
#>   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
#> Scale for 'y' is already present. Adding another scale for 'y', which will
#> replace the existing scale.
#> Scale for 'x' is already present. Adding another scale for 'x', which will
#> replace the existing scale.

nps_data_clean = nps_data %>%
  select(-c(year_quarter, account_names))

nps_data_clean
#>     nps useful useable desireable findable credible valuable
#> 1    10      4       3          3        4        4        3
#> 2    10      3       5          4        4        4        2
#> 3     6      4       4          4        5        4        2
#> 4     9      4       5          5        3        5        4
#> 5     6      4       2          5        2        5        4
#> 6     5      5       1          3        4        4        4
#> 7     5      4       4          4        5        3        5
#> 8     9      5       2          4        2        5        5
#> 9     6      4       5          5        3        3        4
#> 10    8      5       1          5        3        5        4
#> 11    7      3       5          5        1        5        2
#> 12    9      5       1          5        3        5        5
#> 13    9      5       4          4        2        3        2
#> 14    4      5       3          3        3        4        2
#> 15    5      3       1          3        3        5        4
#> 16    6      5       1          5        1        5        5
#> 17    8      5       2          3        2        3        3
#> 18    6      4       4          4        2        3        3
#> 19    6      4       2          4        2        3        2
#> 20    4      4       1          5        4        5        2
#> 21    7      5       5          4        1        5        1
#> 22    4      5       5          3        5        3        3
#> 23    4      5       1          5        5        3        5
#> 24    8      4       1          3        3        4        3
#> 25    6      3       5          3        3        5        5
#> 26    5      4       5          5        5        5        1
#> 27   10      3       2          4        1        4        2
#> 28    5      5       2          3        4        3        4
#> 29    4      5       5          3        2        5        3
#> 30    9      4       1          4        5        3        5
#> 31    6      3       1          4        4        3        5
#> 32    7      5       5          4        5        5        5
#> 33    9      3       5          4        1        4        4
#> 34    4      5       4          3        2        5        3
#> 35    6      4       5          5        3        5        3
#> 36   10      4       5          5        1        5        2
#> 37    8      3       4          4        1        4        2
#> 38    7      5       2          5        3        3        2
#> 39   10      4       5          5        2        5        2
#> 40    5      3       1          5        4        5        2
#> 41    8      5       1          4        5        4        3
#> 42   10      5       2          5        3        4        1
#> 43    7      5       1          5        4        4        1
#> 44    7      3       2          4        5        5        1
#> 45    8      4       5          4        3        5        1
#> 46    9      4       1          5        1        4        3
#> 47    9      3       3          4        5        3        1
#> 48   10      4       2          5        2        3        2
#> 49    7      5       2          4        4        3        3
#> 50    9      5       5          4        2        4        4
#> 51    7      3       4          3        1        3        4
#> 52    9      4       1          4        5        4        3
#> 53   10      3       5          5        2        4        4
#> 54    8      4       2          3        5        5        2
#> 55    7      5       5          3        1        5        3
#> 56    8      5       2          5        1        5        4
#> 57    7      3       2          4        2        5        3
#> 58    7      5       4          3        4        4        4
#> 59   10      4       3          3        2        4        5
#> 60   10      3       4          5        1        4        1
#> 61    9      3       3          5        1        5        5
#> 62    7      4       3          5        1        5        4
#> 63    8      5       3          3        4        5        5
#> 64    7      3       5          4        1        3        1
#> 65    7      4       3          4        3        5        1
#> 66    9      4       2          5        2        3        1
#> 67    7      3       3          5        1        5        2
#> 68    8      4       1          5        1        4        2
#> 69    7      3       5          5        5        3        4
#> 70    9      3       1          4        5        3        1
#> 71    7      4       4          3        4        3        2
#> 72    9      3       2          5        4        5        1
#> 73    8      5       2          4        4        3        3
#> 74   10      3       4          3        5        4        3
#> 75    9      3       1          5        4        3        1
#> 76   10      4       4          5        1        5        4
#> 77   10      3       5          5        3        5        3
#> 78    8      4       5          4        5        5        5
#> 79    8      3       3          3        4        4        2
#> 80    9      3       5          4        2        5        4
#> 81   10      3       5          5        1        4        2
#> 82    8      5       3          3        4        3        2
#> 83    8      5       1          4        1        5        1
#> 84    9      4       3          3        3        4        1
#> 85   10      3       4          4        1        4        3
#> 86    8      4       2          4        3        3        5
#> 87    9      5       5          5        3        4        5
#> 88    9      4       1          4        2        3        1
#> 89    8      5       4          4        3        4        4
#> 90    9      4       3          4        4        5        2
#> 91    8      5       1          4        2        5        3
#> 92    8      3       2          4        1        4        3
#> 93    9      3       3          5        5        4        2
#> 94   10      3       3          3        3        3        5
#> 95   10      3       1          3        4        4        4
#> 96    8      5       2          5        5        5        1
#> 97    9      4       1          5        5        4        4
#> 98    8      3       2          5        4        5        3
#> 99    9      4       2          5        1        3        5
#> 100   8      3       2          4        1        4        5
#> 101  10      5       5          3        4        5        4
#> 102  10      5       4          3        3        3        5
#> 103   9      3       4          5        5        4        1
#> 104  10      4       5          4        2        5        5
#> 105   8      5       4          4        1        4        4
#> 106   9      3       1          5        4        3        5
#> 107   9      5       2          3        4        5        5
#> 108  10      4       1          5        4        5        5
#> 109   9      3       2          5        3        5        3
#> 110   8      3       3          3        2        3        4
#> 111  10      3       2          3        2        5        5
#> 112  10      3       3          5        3        4        5
#> 113  10      3       1          5        5        4        4
#> 114   9      5       4          4        4        4        4
#> 115   9      4       2          4        3        5        3
#> 116  10      4       3          5        3        3        3
#> 117   8      4       5          4        1        3        4
#> 118   8      5       5          5        5        4        1
#> 119  10      3       3          5        5        4        4
#> 120   9      4       2          4        1        5        2
#> 121   9      4       5          5        1        4        2
#> 122   9      3       4          5        1        4        4
#> 123   9      4       2          3        5        3        1
#> 124   9      3       1          5        5        5        1
#> 125   9      3       4          3        4        3        2
#> 126  10      4       1          4        2        5        4
```

Next we run the key driver function with the clean data version. The
outcome variable must be in quotes.

The color is the graphing function

``` r
nps_driver_data =  key_driver_function(data = nps_data_clean, outcome = "nps")
nps_driver_data
#>     variable importance positive color
#> 1    useable       0.87      40%  grey
#> 2 desireable      64.96      75%  blue
#> 3   findable     -29.26      40%  grey
#> 4   credible      -1.85      72%  grey
#> 5   valuable       3.06      43%  grey
```

If you wanted to analyze an ordinal outcome you would need to remove any
continuous outcomes and run the function with the variable you want.

``` r
ordinal_nps_data_clean = nps_data %>%
  select(-c(year_quarter, account_names, nps))

ordinal_nps_driver_data =  key_driver_function(data = nps_data_clean, outcome = "valuable")
```

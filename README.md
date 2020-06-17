# {bgstats}
Generate background table from wide-type `tbl_df` data.

# Required packages
```R
library(tidyverse)
library(rstatix)
```

# Example
## mtcars
```R
data(mtcars)
mtcars %>% 
  cbind.data.frame(data.frame(car = rownames(.)),
                   .) %>% 
  tbl_df %>% 
  mutate_at(c("cyl", "vs", "am"), as.factor) -> df
```

## Fisher's exact test ver.1
```R
fisher_table(df,
          group = "vs",
          omit = NA)
```

```
# A tibble: 3 x 8
  Variable Factor      Number_0 Ratio_0 Number_1 Ratio_1    P.value SD   
  <fct>    <fct>          <int>   <dbl>    <int>   <dbl>      <dbl> <chr>
1 car      AMC Javelin        1  0.0556        0   0     1          ""   
2 cyl      4                  1  0.0556       10   0.714 0.00000352 ***  
3 am       0                 12  0.667         7   0.5   0.473      ""   
```

## Fisher's exact test ver.2 
```R
fisher_table2(df,
          group = "vs",
          omit = NA)
```

```
# A tibble: 3 x 5
  Variable          `0`        `1`           P.value SD   
  <chr>             <chr>      <chr>           <dbl> <chr>
1 car [AMC Javelin] 1 (5.6%)   0 (0.0%)   1          ""   
2 cyl [4]           1 (5.6%)   10 (71.4%) 0.00000352 ***  
3 am [0]            12 (66.7%) 7 (50.0%)  0.473      ""   
```

## Wilcoxon Ranksum test

```R
wilcox_table(df,
          group = "vs",
          omit = NA,
          digits = 1)
```

```
# A tibble: 8 x 5
  Variable `0`                 `1`                    P.value SD   
  <chr>    <chr>               <chr>                    <dbl> <chr>
1 carb     "  4 (2.2 - 4.0)"   "  2 (1.0 - 2.0)"    0.000451  ***  
2 disp     311 (275.8 - 360.0) 121 (83.0 - 162.4)   0.0000607 ***  
3 drat     "  3 (3.1 - 3.7)"   "  4 (3.7 - 4.1)"    0.0134    *    
4 gear     "  3 (3.0 - 4.0)"   "  4 (4.0 - 4.0)"    0.12      ""   
5 hp       180 (156.2 - 226.2) " 96 (66.0 - 109.8)" 0.000031  ***  
6 mpg      " 16 (14.8 - 19.1)" " 23 (21.4 - 29.6)"  0.0000903 ***  
7 qsec     " 17 (16.0 - 17.4)" " 19 (18.6 - 20.0)"  0.0000114 ***  
8 wt       "  4 (3.2 - 3.8)"   "  3 (2.0 - 3.2)"    0.00116   **  
```

## Get tableone
```R
wilcox_fisher_table(df,
          group = "vs",
          omit = NA,
          digits = 1) %>% 
          knitr::kable(., align = "c")
```

|     Variable      |          0          |         1          |  P.value  | SD  |
|:-----------------:|:-------------------:|:------------------:|:---------:|:---:|
|      cyl [4]      |      1 (5.6%)       |     10 (71.4%)     | 0.0000035 | *** |
|       qsec        |  17 (16.0 - 17.4)   |  19 (18.6 - 20.0)  | 0.0000114 | *** |
|        hp         | 180 (156.2 - 226.2) | 96 (66.0 - 109.8)  | 0.0000310 | *** |
|       disp        | 311 (275.8 - 360.0) | 121 (83.0 - 162.4) | 0.0000607 | *** |
|        mpg        |  16 (14.8 - 19.1)   |  23 (21.4 - 29.6)  | 0.0000903 | *** |
|       carb        |    4 (2.2 - 4.0)    |   2 (1.0 - 2.0)    | 0.0004510 | *** |
|        wt         |    4 (3.2 - 3.8)    |   3 (2.0 - 3.2)    | 0.0011600 | **  |
|       drat        |    3 (3.1 - 3.7)    |   4 (3.7 - 4.1)    | 0.0134000 |  *  |
|       gear        |    3 (3.0 - 4.0)    |   4 (4.0 - 4.0)    | 0.1200000 |     |
|      am [0]       |     12 (66.7%)      |     7 (50.0%)      | 0.4726974 |     |
| car [AMC Javelin] |      1 (5.6%)       |      0 (0.0%)      | 1.0000000 |     |


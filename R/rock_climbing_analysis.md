Rock climbing survey data analysis
================
Sunny
2023-03-07

### R packages

``` r
library(tidyverse)
library(here)
library(coin)
```

### Other functions

``` r
wilcox_table <- function(dataset, question_id){
  
  wilcox <- dataset %>%
  select(`Respondent ID`, question_id, `survey`) %>%
  mutate_at(c(2), ~recode(., "Strongly Agree" = 4,
                          "Strongly agree" = 4,
                          "Agree" = 3,
                          "Disagree" = 2,
                          "Strongly Disagree" = 1,
                          "Strongly disagree" = 1,
                          
                          "Very Likely" = 3,
                          "Very likely" = 3,
                          "Somewhat Likely" = 2,
                          "Somewhat likely" = 2,
                          "Not Likely" = 1,
                          "Not likely" = 1,
                          
                          "Absolutely Essential" = 5,
                          "Very Important" = 4,
                          "Of Average Importance" = 3,
                          "Of Little Importance" = 2,
                          "Not Important At All" = 1),
            na.rm = TRUE) %>%
  pivot_wider(names_from = survey, values_from = c(2)) 

return(wilcox)
}
```

### Import data & produce question list

``` r
data_c1 <- read_csv(here("data", "processed", "data_c1_clean.csv"))
data_c2 <- read_csv(here("data", "processed", "data_c2_clean.csv"))
data_c3 <- read_csv(here("data", "processed", "data_c3_clean.csv"))
```

### Wilcoxon matched-pairs signed rank test for single question

You only need to change two values:

-   `dataset` = either `data_c1`, `data_c2`, or `data_c3`
-   `quesiton_id` = numerical value, which corresponds to the column
    number of the .csv file.

``` r
wilcox <- wilcox_table(dataset = data_c1, question_id = 5)

wilcox %>%
  mutate(difference = s2 - s1,
         direction = if_else(difference >= 0, "positive", "negative")) %>%
  ggplot() +
    geom_bar(aes(difference, fill = direction)) +
    theme_bw()
```

![](rock_climbing_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
wilcoxsign_test(wilcox$s1 ~ wilcox$s2, 
                zero.method = "Wilcoxon")
```

    ## 
    ##  Asymptotic Wilcoxon Signed-Rank Test
    ## 
    ## data:  y by x (pos, neg) 
    ##   stratified by block
    ## Z = -1.9993, p-value = 0.04558
    ## alternative hypothesis: true mu is not equal to 0

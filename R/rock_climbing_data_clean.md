Rock climbing survey data claning & t-test
================
Sunny Tseng
2023-03-05

### Data description

There are 3 cohorts experienced rock climbing intervention. Surveys were
done before intervention, after intervention, and 1-year after
intervention (only for cohort 1). This research is aim to understand how
the intervention would influence the behaviour of the participants.
Cohort 1 includes 12 people, cohort 2 include

And there were 20 questions asked, with 4 different levels: Strongly
Agree, Agree, Disagree, Strongly Disagree. The goal of the analysi is to
test whether there are differences between those levels before and after
the intervention.

### R packages & other functions

Here we install (by `install.packages()`) and load (by `library()`) the
packages that we will need. A package is like a tool box in R, that
includes many functions. For example, `tidyverse` package provides
functions for data wrangling and data cleaning, `here` packages provides
functions for working directory management.

``` r
#install.packages("tidyverse")
#install.packages("here")
library(tidyverse)
library(readxl)
library(here)
library(knitr)
```

## Cohort 1

### Import data

Use `read_excel` to import raw data. Note that the name of the folder
and the file name were changed as there were parentheses `()` and commas
`,` in the original name, which causes error when importing files into
R.

``` r
data_c1_s1 <- read_excel(here("data", 
                              "Cohort 1 Surveys_123_spreadsheets", 
                              "VIMFF Cohort 1 Survey 1_pre-intervention.xlsx"))

data_c1_s2 <- read_excel(here("data", 
                              "Cohort 1 Surveys_123_spreadsheets", 
                              "VIMFF Cohort 1 Survey 2_post-intervention.xlsx"))

data_c1_s3 <- read_excel(here("data", 
                              "Cohort 1 Surveys_123_spreadsheets", 
                              "VIMFF Cohort 1 Survey 3_post-post.xlsx"))
```

### Data cleaning

Cleaned column names, combined columns with multiple choice answers,
changed the categorical answer into 1 - 4 for easier analysis. Only
select columns that will need in the following analysis.

For cohort 1, survey 1:

``` r
new_name <- function(dataset, start, end){
  dataset[1, start:end] %>% as_vector()
}

data_c1_s1_clean <- data_c1_s1 %>%
  rename_with(.fn =~ new_name(data_c1_s1, 65, 84), .cols = c(65:84)) %>%
  filter(row_number() != 1) %>%
  unite(gender, names(.)[11:23], sep = ",", na.rm = TRUE) %>%
  unite(nationality, names(.)[12:28], sep = ",", na.rm = TRUE) %>%
  unite(indoor_style_practice, names(.)[24:26], sep = ",", na.rm = TRUE) %>%
  unite(outdoor_style_practice, names(.)[29:34], sep = ",", na.rm = TRUE) %>%
  mutate_at(c(30:49), ~recode(., "Strongly Agree" = 4,
                             "Strongly agree" = 4,
                             "Agree" = 3,
                             "Disagree" = 2,
                             "Strongly Disagree" = 1,
                             "Strongly disagree" = 1),
            na.rm = TRUE) %>%
  mutate_at(c(30:49), ~as_factor(.)) %>%
  mutate(cohort = "c1", survey = "s1") %>%
  select("Respondent ID", "Collector ID", "indoor_style_practice",
         "outdoor_style_practice", 30:49, "cohort", "survey") 

data_c1_s1_clean %>% str()
```

    ## tibble [13 x 26] (S3: tbl_df/tbl/data.frame)
    ##  $ Respondent ID                                                                                      : chr [1:13] "Participant 1" "Participant 2" "Participant 3" "Participant 4" ...
    ##  $ Collector ID                                                                                       : num [1:13] 2.7e+08 2.7e+08 2.7e+08 2.7e+08 2.7e+08 ...
    ##  $ indoor_style_practice                                                                              : chr [1:13] "Bouldering" "Bouldering" "Bouldering" "Bouldering" ...
    ##  $ outdoor_style_practice                                                                             : chr [1:13] "Bouldering" "" "" "" ...
    ##  $ I love to climb                                                                                    : Factor w/ 3 levels "2","3","4": 2 2 1 2 2 1 2 2 2 3 ...
    ##  $ Climbing is fun                                                                                    : Factor w/ 3 levels "2","3","4": 2 2 2 2 3 1 2 2 2 3 ...
    ##  $ I am motivated to climb                                                                            : Factor w/ 3 levels "2","3","4": 2 2 2 2 2 2 1 2 1 3 ...
    ##  $ I feel encouraged in my climbing                                                                   : Factor w/ 3 levels "2","3","4": 2 2 1 2 3 1 2 2 2 3 ...
    ##  $ I feel hindered in my climbing                                                                     : Factor w/ 4 levels "1","2","3","4": 1 2 3 2 2 4 2 3 2 3 ...
    ##  $ I am inspired to climb by my female-identified climbing friends                                    : Factor w/ 2 levels "3","4": 2 1 2 2 2 1 1 1 2 2 ...
    ##  $ I am inspired to climb by films showcasing outdoor women climbers                                  : Factor w/ 3 levels "2","3","4": 2 2 2 3 3 1 3 1 3 2 ...
    ##  $ I am inspired to spend more time outdoors after watching films that showcase outdoor women climbers: Factor w/ 3 levels "2","3","4": 2 2 2 3 3 2 3 1 3 2 ...
    ##  $ I feel represented within the climbing community                                                   : Factor w/ 3 levels "1","2","3": 3 3 2 1 3 1 3 2 2 3 ...
    ##  $ I feel outdoor climbing guides are representative of the larger Vancouver and Squamish area        : Factor w/ 3 levels "1","2","3": 3 2 NA 1 3 2 3 2 3 3 ...
    ##  $ I feel welcomed in the climbing community                                                          : Factor w/ 3 levels "2","3","4": 2 2 2 2 3 1 2 2 2 3 ...
    ##  $ I feel safe in the climbing community                                                              : Factor w/ 3 levels "2","3","4": 2 2 1 2 3 1 2 1 2 3 ...
    ##  $ I feel connected to the climbing community                                                         : Factor w/ 4 levels "1","2","3","4": 3 3 2 2 3 2 3 2 2 4 ...
    ##  $ I feel the climbing community reflects the diversity of Vancouver and Squamish areas               : Factor w/ 3 levels "1","2","3": 3 3 2 1 3 1 3 2 2 1 ...
    ##  $ I feel it is possible for anyone to progress in climbing                                           : Factor w/ 4 levels "1","2","3","4": 3 3 2 1 4 2 3 4 3 3 ...
    ##  $ I find indoor climbing more accessible than outdoor climbing                                       : Factor w/ 3 levels "1","3","4": 2 2 3 1 2 2 3 2 3 2 ...
    ##  $ I find it difficult to find the resources to go outdoor climbing                                   : Factor w/ 2 levels "3","4": 1 1 1 2 1 2 2 1 2 2 ...
    ##  $ I find it difficult to find the time to go outdoor climbing                                        : Factor w/ 3 levels "2","3","4": 2 1 1 1 3 3 2 2 2 2 ...
    ##  $ I find it difficult to find transportation to go outdoor climbing                                  : Factor w/ 3 levels "2","3","4": 2 1 3 3 1 3 1 2 1 3 ...
    ##  $ I find it difficult to find friends to go outdoor climbing with                                    : Factor w/ 3 levels "2","3","4": 2 2 2 3 2 3 3 2 3 2 ...
    ##  $ cohort                                                                                             : chr [1:13] "c1" "c1" "c1" "c1" ...
    ##  $ survey                                                                                             : chr [1:13] "s1" "s1" "s1" "s1" ...

For cohort 1, survey 2:

``` r
data_c1_s2_clean <- data_c1_s2 %>%
  rename_with(.fn =~ new_name(data_c1_s2, 32, 51), .cols = c(32:51)) %>%
  filter(row_number() != 1) %>%
  unite(indoor_style_practice, names(.)[19:21], sep = ",", na.rm = TRUE) %>%
  unite(outdoor_style_practice, names(.)[24:29], sep = ",", na.rm = TRUE) %>%
  mutate_at(c(25:44), ~recode(., "Strongly Agree" = 4,
                             "Strongly agree" = 4,
                             "Agree" = 3,
                             "Disagree" = 2,
                             "Strongly Disagree" = 1,
                             "Strongly disagree" = 1),
            na.rm = TRUE) %>%
  mutate_at(c(25:44), ~as_factor(.)) %>%
  mutate(cohort = "c1", survey = "s2") %>%
  select("Respondent ID", "Collector ID", "indoor_style_practice",
         "outdoor_style_practice", 25:44, "cohort", "survey") 

data_c1_s2_clean %>% str()
```

    ## tibble [12 x 26] (S3: tbl_df/tbl/data.frame)
    ##  $ Respondent ID                                                                                      : chr [1:12] "Participant 1" "Participant 2" "Participant 3" "Participant 4" ...
    ##  $ Collector ID                                                                                       : num [1:12] 2.7e+08 2.7e+08 2.7e+08 2.7e+08 2.7e+08 ...
    ##  $ indoor_style_practice                                                                              : chr [1:12] "Bouldering,Top-rope" "Bouldering,Top-rope" "Bouldering,Top-rope" "Bouldering,Top-rope" ...
    ##  $ outdoor_style_practice                                                                             : chr [1:12] "Bouldering,Top-rope" "" "" "" ...
    ##  $ I love to climb                                                                                    : Factor w/ 3 levels "2","3","4": 3 1 3 2 3 3 3 2 3 2 ...
    ##  $ Climbing is fun                                                                                    : Factor w/ 2 levels "3","4": 2 1 2 2 2 2 2 1 2 1 ...
    ##  $ I am motivated to climb                                                                            : Factor w/ 3 levels "2","3","4": 3 1 3 2 2 3 3 2 2 2 ...
    ##  $ I feel encouraged in my climbing                                                                   : Factor w/ 2 levels "3","4": 2 1 2 2 1 2 1 1 1 1 ...
    ##  $ I feel hindered in my climbing                                                                     : Factor w/ 2 levels "1","2": 2 2 2 2 2 1 2 2 2 2 ...
    ##  $ I am inspired to climb by my female-identified climbing friends                                    : Factor w/ 2 levels "3","4": 2 1 2 1 2 2 2 2 2 1 ...
    ##  $ I am inspired to climb by films showcasing outdoor women climbers                                  : Factor w/ 3 levels "2","3","4": 3 2 2 3 1 2 2 3 2 1 ...
    ##  $ I am inspired to spend more time outdoors after watching films that showcase outdoor women climbers: Factor w/ 2 levels "3","4": 2 1 1 2 1 1 1 2 2 1 ...
    ##  $ I feel represented within the climbing community                                                   : Factor w/ 4 levels "1","2","3","4": 1 2 2 2 2 3 2 3 4 1 ...
    ##  $ I feel outdoor climbing guides are representative of the larger Vancouver and Squamish area        : Factor w/ 4 levels "1","2","3","4": 1 2 2 3 2 3 3 NA 4 1 ...
    ##  $ I feel welcomed in the climbing community                                                          : Factor w/ 3 levels "2","3","4": 3 2 2 2 2 3 2 2 2 1 ...
    ##  $ I feel safe in the climbing community                                                              : Factor w/ 3 levels "2","3","4": 2 2 2 2 2 3 2 2 2 1 ...
    ##  $ I feel connected to the climbing community                                                         : Factor w/ 2 levels "2","3": 2 1 1 2 1 2 2 2 1 1 ...
    ##  $ I feel the climbing community reflects the diversity of Vancouver and Squamish areas               : Factor w/ 3 levels "1","2","3": 1 3 2 3 2 2 2 NA 3 1 ...
    ##  $ I feel it is possible for anyone to progress in climbing                                           : Factor w/ 4 levels "1","2","3","4": 1 2 4 3 2 4 3 3 3 3 ...
    ##  $ I find indoor climbing more accessible than outdoor climbing                                       : Factor w/ 3 levels "1","3","4": 1 1 3 2 3 3 2 3 3 2 ...
    ##  $ I find it difficult to find the resources to go outdoor climbing                                   : Factor w/ 2 levels "3","4": 2 1 1 2 2 2 1 1 2 1 ...
    ##  $ I find it difficult to find the time to go outdoor climbing                                        : Factor w/ 3 levels "2","3","4": 1 1 2 3 3 3 2 2 2 2 ...
    ##  $ I find it difficult to find transportation to outdoor climbing                                     : Factor w/ 4 levels "1","2","3","4": 4 3 2 2 4 1 3 3 2 2 ...
    ##  $ I find it difficult to find friends to go outdoor climbing with                                    : Factor w/ 3 levels "2","3","4": 3 1 1 1 3 3 3 3 3 3 ...
    ##  $ cohort                                                                                             : chr [1:12] "c1" "c1" "c1" "c1" ...
    ##  $ survey                                                                                             : chr [1:12] "s2" "s2" "s2" "s2" ...

For cohort 1, survey 3:

``` r
data_c1_s3_clean <- data_c1_s3 %>%
  rename_with(.fn =~ new_name(data_c1_s3, 32, 51), .cols = c(32:51)) %>%
  filter(row_number() != 1) %>%
  unite(indoor_style_practice, names(.)[19:21], sep = ",", na.rm = TRUE) %>%
  unite(outdoor_style_practice, names(.)[24:29], sep = ",", na.rm = TRUE) %>%
  mutate_at(c(25:44), ~recode(., "Strongly Agree" = 4,
                             "Strongly agree" = 4,
                             "Agree" = 3,
                             "Disagree" = 2,
                             "Strongly Disagree" = 1,
                             "Strongly disagree" = 1),
            na.rm = TRUE) %>%
  mutate_at(c(25:44), ~as_factor(.)) %>%
  mutate(cohort = "c1", survey = "s3") %>%
  select("Respondent ID", "Collector ID", "indoor_style_practice",
         "outdoor_style_practice", 25:44, "cohort", "survey") 

data_c1_s3_clean %>% str()
```

    ## tibble [10 x 26] (S3: tbl_df/tbl/data.frame)
    ##  $ Respondent ID                                                                                      : chr [1:10] "Participant 1" "Participant 10" "Participant 12" "Participant 4" ...
    ##  $ Collector ID                                                                                       : num [1:10] 2.7e+08 2.7e+08 2.7e+08 2.7e+08 2.7e+08 ...
    ##  $ indoor_style_practice                                                                              : chr [1:10] "" "Bouldering,Top-rope" "Bouldering,Top-rope" "" ...
    ##  $ outdoor_style_practice                                                                             : chr [1:10] "" "" "" "" ...
    ##  $ I love to climb                                                                                    : Factor w/ 3 levels "2","3","4": NA 3 3 1 3 NA 2 2 2 2
    ##  $ Climbing is fun                                                                                    : Factor w/ 3 levels "2","3","4": NA 3 3 1 2 NA 3 2 2 2
    ##  $ I am motivated to climb                                                                            : Factor w/ 3 levels "2","3","4": NA 3 2 1 3 NA 2 2 2 2
    ##  $ I feel encouraged in my climbing                                                                   : Factor w/ 3 levels "2","3","4": NA 1 2 2 2 NA 2 2 3 2
    ##  $ I feel hindered in my climbing                                                                     : Factor w/ 3 levels "2","3","4": NA 3 1 1 2 NA 2 1 1 1
    ##  $ I am inspired to climb by my female-identified climbing friends                                    : Factor w/ 2 levels "3","4": NA 1 2 2 1 NA 2 1 2 1
    ##  $ I am inspired to climb by films showcasing outdoor womxn climbers                                  : Factor w/ 3 levels "2","3","4": NA 1 1 2 1 NA 3 1 3 3
    ##  $ I am inspired to spend more time outdoors after watching films that showcase outdoor womxn climbers: Factor w/ 3 levels "2","3","4": NA 1 2 2 2 NA 3 1 3 3
    ##  $ I am inspired to go outdoor climbing after watching films that showcase outdoor womxn climbers     : Factor w/ 3 levels "2","3","4": NA 2 2 2 3 NA 2 1 1 3
    ##  $ I feel represented within the climbing community                                                   : Factor w/ 3 levels "2","3","4": NA 2 1 1 1 NA 2 1 3 2
    ##  $ I feel outdoor climbing guides are representative of the larger Vancouver and Squamish area        : Factor w/ 3 levels "1","2","4": NA 2 2 NA 1 NA NA 2 3 2
    ##  $ I feel welcomed in the climbing community                                                          : Factor w/ 3 levels "2","3","4": NA 2 2 NA 2 NA 2 1 3 2
    ##  $ I feel safe in the climbing community                                                              : Factor w/ 3 levels "2","3","4": NA 2 2 NA 1 NA 2 1 3 2
    ##  $ I feel connected to the climbing community                                                         : Factor w/ 3 levels "2","3","4": NA 2 1 NA 1 NA 1 1 3 1
    ##  $ I feel the climbing community reflects the diversity of Vancouver and Squamish areas               : Factor w/ 2 levels "2","3": NA 1 2 NA 1 NA 1 1 2 1
    ##  $ I feel it is possible for anyone to progress in climbing                                           : Factor w/ 3 levels "1","3","4": NA 2 2 NA 1 NA 2 2 3 2
    ##  $ I find indoor climbing more accessible than outdoor climbing                                       : Factor w/ 2 levels "3","4": NA 1 1 NA 2 NA 1 2 2 2
    ##  $ I find it difficult to find the resources to go outdoor climbing                                   : Factor w/ 3 levels "2","3","4": NA 3 2 1 3 NA 3 3 2 3
    ##  $ I find it difficult to find the time to go outdoor climbing                                        : Factor w/ 3 levels "2","3","4": NA 3 2 1 1 NA 2 1 3 1
    ##  $ I find it difficult to find transportation to outdoor climbing                                     : Factor w/ 3 levels "2","3","4": NA 3 1 2 3 NA 2 1 1 1
    ##  $ cohort                                                                                             : chr [1:10] "c1" "c1" "c1" "c1" ...
    ##  $ survey                                                                                             : chr [1:10] "s3" "s3" "s3" "s3" ...

For cohort 1 (final summary). Noted that there are some mismatch between
column names. Need to fix that before combining data frames.

``` r
# s1 says "I find it difficult to find transportation to go outdoor climbing", while
# s2 says "I find it difficult to find transportation to outdoor climbing"
# There are many mis-match between s3 and s1&s2. Need check!

names(data_c1_s1_clean)[23] <- names(data_c1_s2_clean)[23] 
data_c1_clean <- rbind(data_c1_s1_clean, data_c1_s2_clean) 
data_c1_clean
```

    ## # A tibble: 25 x 26
    ##    `Respondent ID` `Collector ID` indoor_style_practice      outdoor_style_prac~
    ##    <chr>                    <dbl> <chr>                      <chr>              
    ##  1 Participant 1        270250947 "Bouldering"               "Bouldering"       
    ##  2 Participant 2        270250947 "Bouldering"               ""                 
    ##  3 Participant 3        270250947 "Bouldering"               ""                 
    ##  4 Participant 4        270250947 "Bouldering"               ""                 
    ##  5 Participant 5        270251884 "Bouldering,Top-rope"      ""                 
    ##  6 Participant 6        270251884 ""                         ""                 
    ##  7 Participant 7        270251884 "Bouldering,Top-rope"      ""                 
    ##  8 Participant 8        270251884 "Bouldering"               "Bouldering"       
    ##  9 Participant 9        270250932 "Bouldering,Top-rope"      "Top-rope"         
    ## 10 Participant 10       270251884 "Bouldering,Top-rope,Lead" "Top-rope"         
    ## # ... with 15 more rows, and 22 more variables: `I love to climb` <fct>,
    ## #   `Climbing is fun` <fct>, `I am motivated to climb` <fct>,
    ## #   `I feel encouraged in my climbing` <fct>,
    ## #   `I feel hindered in my climbing` <fct>,
    ## #   `I am inspired to climb by my female-identified climbing friends` <fct>,
    ## #   `I am inspired to climb by films showcasing outdoor women climbers` <fct>,
    ## #   `I am inspired to spend more time outdoors after watching films that showcase outdoor women climbers` <fct>, ...

### t-test

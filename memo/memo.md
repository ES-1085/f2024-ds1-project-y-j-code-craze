Project memo
================
Y & J Code Craze

This document should contain a detailed account of the data clean up for
your data and the design choices you are making for your plots. For
instance you will want to document choices you’ve made that were
intentional for your graphic, e.g. color you’ve chosen for the plot.
Think of this document as a code script someone can follow to reproduce
the data cleaning steps and graphics in your handout.

``` r
library(tidyverse)
library(broom)
Internship_Data_Untidy <- read.csv("/cloud/project/data/Internship_Data_Untidy.csv")
```

## Data Clean Up Steps for Overall Data

### Step 1: Tidy the Term Column and Make a Plot of Term Data

#### Step 1 a.) Change Entries with unknown term data to “NA” and unnesesary remove spaces before and after an entry

``` r
Internship_Data_Untidy %>% 
  mutate(Term = case_when( 
    Term %in% c("NA", "Not Stated", "-", "", "NA ", "All Year", "n/a", "Year", "Year-round", "N/A", "Previous Work Experience", "Nine Months", "Prior Fulfillment"))) ~ "NA" %>% 
  mutate(Term = str_squish(Term))
```

    ## Internship_Data_Untidy %>% mutate(Term = case_when(Term %in% 
    ##     c("NA", "Not Stated", "-", "", "NA ", "All Year", "n/a", 
    ##         "Year", "Year-round", "N/A", "Previous Work Experience", 
    ##         "Nine Months", "Prior Fulfillment"))) ~ "NA" %>% mutate(Term = str_squish(Term))

#### Step 1 b.) Ensure all terms fall under corespond to one COA term.

Because we are using the last recorded year from the “year” column, we
are also using the last recorded term. The last term of each entry
should correspond to the year the internship was completed

``` r
Tidy_Term <- Internship_Data_Untidy %>% 
mutate(Term = case_when(
  Term %in% c("Fall", "Winter, Spring, Fall", "Winter, Fall", "Summer-Fall", "Spring, Fall", "Summer/Fall", "Summer, Fall", "Spring, Summer, Fall", "Summer,Fall", "Winter+Fall") ~ "Fall",
  Term %in% c("Winter", "Winter, Fall, Winter", "Fall/Winter", "December-December", "Summer, Fall, Winter", "Fall-Winter", "Fall, Winter", "Summer-Winter") ~ "Winter",
  Term %in% c("Spring", "Fall, Winter, Spring", "spring", "Winter, Spring", "Winter-Spring", "Novermber-Spring", "Summer, Spring, Spring") ~ "Spring",
  Term %in% c("Summer", "Fall, Summer", "August", "Winter-Summer", "Winter, Spring, Summer", "Winter, Summer", "Spring, Summer", "Fall+Summer", "Spring/Summer", "Fall, Winter, Spring, Summer", "Summe", "Summer 2011") ~ "Summer"
))
```

#### Step 1 c.) Yay it’s tidy!

``` r
Tidy_Term %>% 
  group_by(Term) %>%
  summarise(count = n())
```

    ## # A tibble: 5 × 2
    ##   Term   count
    ##   <chr>  <int>
    ## 1 Fall     228
    ## 2 Spring   197
    ## 3 Summer   815
    ## 4 Winter   256
    ## 5 <NA>     153

### Step 2: \_\_\_\_\_\_\_\_

## Plots

### ggsave example for saving plots

``` r
p1 <- starwars |>
  filter(mass < 1000, 
         species %in% c("Human", "Cerean", "Pau'an", "Droid", "Gungan")) |>
  ggplot() +
  geom_point(aes(x = mass, 
                 y = height, 
                 color = species)) +
  labs(x = "Weight (kg)", 
       y = "Height (m)",
       color = "Species",
       title = "Weight and Height of Select Starwars Species",
       caption = paste("This data comes from the starwars api: https://swapi.py43.com"))


ggsave("example-starwars.png", width = 4, height = 4)

ggsave("example-starwars-wide.png", width = 6, height = 4)
```

### Plot 1: \_\_\_\_\_\_\_\_\_

#### Data cleanup steps specific to plot 1

These data cleaning sections are optional and depend on if you have some
data cleaning steps specific to a particular plot

#### Final Plot 1

### Plot 2: \_\_\_\_\_\_\_\_\_

### Plot 3: \_\_\_\_\_\_\_\_\_\_\_

Add more plot sections as needed. Each project should have at least 3
plots, but talk to me if you have fewer than 3.

### Plot 4: \_\_\_\_\_\_\_\_\_\_\_

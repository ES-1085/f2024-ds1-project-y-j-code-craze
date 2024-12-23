Project proposal
================
Y&J Code Craze

``` r
library(tidyverse)
library(broom)

Internship_Data_Untidy <- read.csv("/cloud/project/data/Internship_Data_Untidy.csv")
```

## 1. Introduction

Our data comes from information compiled by Jeffry Neuhouser and their
work study students about the internships that COA students have taken
over the last 50 years. The data was collected by looking over old
internship reports and compiling them into a spreadsheet. This dataset
is still growing as Jeffry continues to go through old documents. The
currently expanding nature of the body of data means that our dataset
contains data only from students whose last names begin with the letters
A through M. We don’t have information for students with surnames from N
through Z. Each observation in this dataset represents information from
a student’s internship report. Some alumni information was mistakenly
added to the dataset, so we will have to filter those observations out
(those rows have NAs entered under almost every column). Our variables
include: the term, the year, the employer or internship site, the title
of position, the location, the coordinates, and notes on the internship
and location. Jeffry sent us an additional list of industries COA alumni
are presently involved in. The Career Development Office has asked that
we try to categorize internship data by those industries.

Based on our discussions with Jeffry and our own personal interest in
the data we arrived at the following research question :

**What change in trends can be observed in COA internship data in
relation to internship location and industry from 1970-2023?**

We hope that this research question can allow us to meaningfully
investigate the trends across time, allowing us to explore a unique
dimension in our dataset (temporal variation). We also hope that this
question will address the needs of the Career Development Office by
looking at where and in what field COA internships are occurring.

## 2. Data

This is a small overview of our dataset.

``` r
glimpse(Internship_Data_Untidy, na.rm = TRUE) 
```

    ## Rows: 1,649
    ## Columns: 9
    ## $ Term                  <chr> "Summer", "Spring", "Fall", "Fall", "Spring", "S…
    ## $ Year                  <chr> "2015", "2015", "2011", "2017", "1997", "2009", …
    ## $ Internship.Site       <chr> "Zocalo Permaculture Center", "Zocab Permacultur…
    ## $ Title..if.applicable. <chr> "Intern", "Assistant Manager", "Designer", "Vete…
    ## $ Location              <chr> "Gouldsboro, Maine", "Gouldsboro, Maine", "Solot…
    ## $ Latitude              <dbl> 44.478412, NA, 47.318740, -6.201808, 44.387638, …
    ## $ Longitude             <dbl> -68.038339, NA, 7.669828, 39.193376, -68.204336,…
    ## $ Notes..if.any.        <chr> "", "", "", "", "", "", "", "", "", "", "Not cle…
    ## $ Notes.on.Location     <chr> "", "", "", "", "", "", "", "", "coordinates for…

``` r
#"na.rm = TRUE" is being used because our raw dataset has a lot of NA rows that we were instructed by our data source (Jeffry Neuhouser) to disregard. The data will be tidied and sorted further into the project
```

## 3. Data analysis plan

**What variables will you visualize to explore your research
questions?**

We want to explore the locations in which COA students undertake
internships—locally, nationally, or internationally—and if students have
changed where they take internships over time. Additionally, Jeffry
asked us to explore the wider regions (or as they described it , “hubs”)
where students go for internships. We hope to visualize this area based
distribution with something like a heat map. Upon Jeffry’s request, we
will also try to visualize which terms students participate in
internships. Furthermore, we want to look at the industries COA students
take internships in and if there are any popular internship
locations/opportunities. One idea for how to visualize this may be by
using a histogram.

**Will there be any other data that you need to find to help with your
research question?**

Currently we don’t need any other data to answer our research question.
If we do need help, we will contact Jeffry or Rūdy (he has been doing a
lot of work on compiling the data).

**Very preliminary exploratory data analysis, including some summary
statistics and visualizations, along with some explanation on how they
help you learn more about your data.**

Here are two initial visualizations to help us visualize what portion of
students take internships in Bar Harbor versus other locations. The
second graph helps us visualize the relationship between local and not
local internships and the terms that students take them in.

``` r
Internship_BH_or_Not <- Internship_Data_Untidy %>%
mutate(BH_Location = if_else(Location == "Bar Harbor, Maine", "Bar Harbor", "Other")) %>% 
filter(BH_Location != "NA")
Internship_BH_or_Not %>% 
ggplot(mapping = aes(x = BH_Location,
                    fill = BH_Location)) +
geom_bar() +
labs(title = "Internships in Bar Harbor vs. Other Locations",
     x = "Location",
     y = "Number of Internships") + 
scale_fill_manual(values = c("Bar Harbor" = "lightblue", "Other" = "pink")) +
guides(fill = FALSE)
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](proposal_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
Internship_BH_or_Not %>% 
filter(BH_Location != "NA") %>%
filter(Term == c("Fall", "Winter", "Spring", "Summer")) %>%
ggplot(mapping = aes(x = BH_Location,
                    fill = BH_Location)) +
geom_bar() +
facet_wrap(~ Term) +
labs(title = "Internships in Bar Harbor vs. Other Locations by Term",
     x = "Location",
     y = "Number of Internships") +
scale_fill_manual(values = c("Bar Harbor" = "lightblue", "Other" = "pink")) +
guides(fill = FALSE)
```

    ## Warning: There was 1 warning in `filter()`.
    ## ℹ In argument: `Term == c("Fall", "Winter", "Spring", "Summer")`.
    ## Caused by warning in `Term == c("Fall", "Winter", "Spring", "Summer")`:
    ## ! longer object length is not a multiple of shorter object length

![](proposal_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

**The data visualization(s) that you believe will be useful in exploring
your question(s).**

We think that bar graphs, ridge plots, violin plots, and density graphs
may be helpful to visualize our data. The majority of our data is
categorical, so those visualizations will be the most helpful. We also
think it could be interesting to create a map with dots or circles to
represent where COA students take internships. This may be challenging,
so we’ll see how it goes.

## 4. Data Ethics Review

The data ethics review section will be introduced in a separate class
and is not part of the original proposal deadline.

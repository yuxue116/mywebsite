---
challenge: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren.
client: Edward Furlong
date: "2020-07-13T12:49:27.000+06:00"
service: Development, UX Design
shortDescription: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet lorem ipsum dolor.
solution: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren.
thumbnail: images/portfolio/portfolio-1.png
title: Case Study One
---
---
title: 'Session 2: Homework 1'
author: 'Study group 3: Selin Beijersbergen, Neel Kamal Puri, Yuxue Sang, Kushal Kundanmal,
  Oliver Ayton'
date: "`r Sys.Date()`"
output:
  html_document:
    code_folding: show
    highlight: zenburn
    number_sections: yes
    theme: flatly
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
---


```{r, setup, warning=FALSE, message=FALSE, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
# Load packages

library(tidyverse)
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(kableExtra)
library(patchwork)
library(scales)
library(tidytext)

```
top 25 spirit consuming countries
ggplot(top_25_spirit, aes(x=reorder(country, spirit_servings), y=spirit_servings)) + 
  geom_col(fill="ivory3", color="black") +
  labs(y='Spirit servings consumed per person', x='Country',
        title='Top 25 spirit consuming countries') +
  coord_flip()

```

# Analysis of Movies- IMDB Dataset
We will look at a subset sample of movies, taken from the [Kaggle IMDB 5000 movie dataset](https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset). First, it's worth inspecting the data set globally. As can be seen below, each movie record contains 11 variables. While `title`, `genre`, `director`, `year`, and `duration` are obvious in meaning, the other variables indicate the following. `gross`is the gross earnings in the US box office, not adjusted for inflation. `budget` indicates the movie's budget. Next, `cast_facebook_likes` shows the number of facebook likes cast members received and `votes` is the number of people who voted for (or rated) the movie in IMDB. `reviews` shows the number of reviews for that movie and, finally, `rating` denotes the IMDB average rating received by the movie.

  
```{r,load_movies, warning=FALSE, message=FALSE}

movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)

```


## Data Inspection
Diving further into the IMDB data set, the following observations can be made. First, there are no missing values (NAs) present. All complete entires are distinct, however some movies appear twice in the data set, e.g. Alice in Wonderland and Spider-Man 3, leading to 54 duplicate movie entries. For all these duplicate movies, the difference between the observations lies in the values of the `votes` variable.
  
  
```{r, NA_and_duplicate_test}

# Perform check for NA
any(is.na(movies))

# Duplicate test
sum(duplicated(movies)) # test for duplicates of observations
movies_info <- movies %>% 
  select(title, genre, director, year)
sum(duplicated(movies_info)) # test for duplicates of unique movies

```

## Inspection per Genre
Moving on to the movie genres, Comedy and Action are by far the most and second-most ranked genres whereas Thriller is ranked the fewest times.

```{R table with the count of movies}

# Get count of movies by genre and rank in descending order
frequency_df <- as.data.frame(table(movies$genre))
names(frequency_df)[1] <- "Genre"
names(frequency_df)[2] <- "Count"
frequency_df <- frequency_df %>%
  arrange(desc(Count))
kbl(frequency_df, caption="Count of movies by genre") %>%
  kable_styling(bootstrap_options="striped", full_width=F, position="left")

```

However, being ranked most often is not any indication of higher gross earnings. It could be argued that higher budgets lead to more successful movies and therefore higher gross earnings. To test for this hypothesis we look at how many dollars movies of a cetain genre made (`average_gross`) for each dollar of its budget (`average_budget`). This figure is recorded in the `return_on_budget` variable in the table below. We see that Musical and Family produce the highest returns, which could be because of the fact that families usually are willing and able to pay more for their family activities as compared to individuals looking to just casually see a movie. Furthermore, we clearly see that there is no set relationship between the size of the budget and the return on budget.

```{r table of avg gross and budget}

return_df <- movies %>% 
  group_by(genre) %>%
  summarise(avg_gross=mean(gross), avg_budget=mean(budget))
names(return_df)[1] <- "Genre"
names(return_df)[2] <- "average_gross"
names(return_df)[3] <- "average_budget"
return_df <- mutate(return_df, return_on_budget=average_gross/average_budget) %>% 
  arrange(desc(return_on_budget))
kbl(return_df, caption="Genres analysed by various metrics") %>%
  kable_styling(bootstrap_options="striped", full_width=F, position="left")


```


## Successful Directors
Moving on to the directors of the movies, we look at the top 15 in terms of higehst gross revenue created. Below we can see the total, mean, median and standard deviation of the gross revenue raised. Steven Spielberg has by far the highest total gross revenue, however the mean gross revenue he makes per movie is actually lower than quite some other directors in this top 15. Therefore, one should carefully choose which metric to use when determining the success of a director.

  
```{r revenue}

revenue_df <- movies %>%
  group_by(director) %>% 
  summarise(Sum=sum(gross), Mean=mean(gross), Median=median(budget), S.dev=sd(gross)) %>% 
  arrange(desc(Sum))
names(revenue_df)[1] <- "Director"
revenue_df <- revenue_df[1:15,]
kbl(revenue_df, caption="Top 15 directors based on gross revenue") %>%
  kable_styling(bootstrap_options="striped", full_width=F, position="left")

```


## Ratings and Genre
Probably the most relevant variable in the IMDB data set is the rating. Looking at the mean rating per genre, Biographies are rated highest whereas Thrillers are rated lowest. However, caution needs to be taken with this as some movies are rated many more times than others. Especially in the case of Thrillers here: the IMDB data set only contains one Thriller entry and therefore the rating metrics for this genre are not statistically significant. This can also be concluded when looking at the ratings density plots per genre. Namely, Thrillers do not even have a density plot because of its single observation.

```{r rating}

rating_df <- movies %>%
  group_by(genre) %>%
  summarise(Mean=mean(rating), Min=min(rating), Max=max(rating), Median=median(rating), S.dev=sd(rating)) %>%
  arrange(desc(Mean)) 
kbl(rating_df, caption="Ratings by genre analysed by various metrics") %>%
  kable_styling(bootstrap_options="striped", full_width=F, position="left")

ggplot(movies, aes(x=rating)) +
  geom_density() +
  facet_wrap(~genre) +
  labs(y='Density', x='Rating', title='Density plots of movie ratings by genre')

```

## Gross Revenue Indicators
Finally, we look at some possible indicators of gross revenue raised. First, we start with the relationship between `gross` and `cast_facebook_likes`. These two variables are mapped below in a scatterplot, using a log transformation on both axis for clarity of interpretation. The number of likes on Facebook is not a strong predictor of a movie's gross revenue. Looking at the scale of the axis, the same number of Facebook likes can lead to a wide range of gross revenue figures. The fitted trend line does increase, however the frequency of data points below the trend line increase as the number of likes increase. At the some time, though, so does the density of datapoints around the trend line. It is therefore difficult to validly establish any possible relationship between Facebook likes and gross revenue, indicating any such relationship may be spurious.
  
```{r, gross_on_cast_facebook_likes}

# Plot gross and cast_facebook_likes with log transformation
ggplot(movies, aes(x=cast_facebook_likes,y=gross)) + 
  geom_point(alpha=0.2) +
  geom_smooth() +
  labs(title='Relationship between gross revenue and the number of Facebook likes', x='Facebook likes received', y='Gross revenue') + 
  scale_x_log10() +
  scale_y_log10()

```

Looking at the relationship between gross revenue and budget,it could generally be argued that a higher budget broadly leads to higher gross revenue - leading to a positive correlation. This can be seen from the clustered observations close to the origin of the graph, indicating that a very low budget raises barely any gross revenue. As budget increases, the observations break away from this cluster in the graph and the gross revenue increases. However, as the gross revenue numbers deviate more as budget increases, it is clear that budget is not the sole predictor for gross revenue.

```{r, gross_on_budget}

ggplot(movies, aes(x=budget,y=gross)) + 
  geom_point(alpha=0.2) +
  geom_smooth() +
  labs(title='Relationship between gross revenue and budget of a movie', x='Budget', y='Gross revenue')

```
  
Arriving at the final step of our analysis of the IMDB data set, we look at the relationship between `gross` and `rating` per genre. Overall, most observations are clustered in the mid-high budget range and low-mid gross revenue range. Looking at the `ratings` values, it seems like they do not differ that much per genre. However, gross revenue does differ a bit more across genres. When benchmarking movies based solely on ratings, some movies in certain genres (like Comedy and Romance) may have a higher rating, but their gross revenues are less than those of movies with lower ratings in genres like Action. However, quite a few genres do not have sufficient numbers of observations, leading to sparse scatterplots. This undermines the validity of any possible trends for those genres. 

>discuss whether IMDB ratings are likely to be a good predictor of how much money a movie will make at the box office. Is there anything strange in this dataset?

```{r, gross_on_rating}
ggplot(movies, aes(x=rating, y=gross)) + 
  geom_point(alpha=0.2) + 
  facet_wrap(~genre) +
  labs(title='Relationship between gross revenue and rating per genre', x='Rating', y='Gross revenue')

```
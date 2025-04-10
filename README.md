# Movie Data Analysis Project

This project involves analyzing movie ratings, genres, and runtime data using R. Below is a summary of the steps involved in the project.

## Contents

- [1. Installing Necessary Libraries](#1-installing-necessary-libraries)
- [2. Loading the Necessary Libraries](#2-loading-the-necessary-libraries)
- [3. Reading the Data](#3-reading-the-data)
- [4. Joining Datasets](#4-joining-datasets)
- [5. Exploring the Data](#5-exploring-the-data)
- [6. Data Cleaning](#6-data-cleaning)
  - [6.1 Counting Missing Values](#61-counting-missing-values)
  - [6.2 Identifying Outliers](#62-identifying-outliers)
  - [6.3 Removing NA Values](#63-removing-na-values)
  - [6.4 Removing Duplicates](#64-removing-duplicates)
  - [6.5 Cleaning Columns](#65-cleaning-columns)
- [7. Frequency Distribution Analysis](#7-frequency-distribution-analysis)
  - [7.1 Frequency Table of `titleType`](#71-frequency-table-of-titletype)
  - [7.2 Selecting Movies Only](#72-selecting-movies-only)
- [8. Movie Ratings Analysis](#8-movie-ratings-analysis)
  - [8.1 Top 100 Movies](#81-top-100-movies)
  - [8.2 Average Rating by Genre](#82-average-rating-by-genre)
  - [8.3 Genre Analysis](#83-genre-analysis)
  - [8.4 Average Rating by Genre Over the Years](#84-average-rating-by-genre-over-the-years)
  - [8.5 Correlation of Average Rating and Length Over the Years](#85-correlation-of-average-rating-and-length-over-the-years)
  - [8.6 Number of Movies Released Per Year](#86-number-of-movies-released-per-year)
  - [8.7 Number of Movies by Genre](#87-number-of-movies-by-genre)
- [9. Bayesian Rating Analysis](#9-bayesian-rating-analysis)
- [10. Genre and Average Rating Correlation](#10-genre-and-average-rating-correlation)

---

1. Installing Necessary Libraries
```r
install.packages('readr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('tidyr')
install.packages('plotly')
```
**Explanation**: Installs essential R libraries needed for data manipulation (`dplyr`, `tidyr`), visualization (`ggplot2`, `plotly`), and file reading (`readr`).
2. Loading the Necessary Libraries
```r
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
```
**Explanation**: Loads the libraries so their functions can be used in the current session.
3. Reading the Data
```r
ratings <- read_tsv('raw_data/title.ratings.tsv')
basics <- read_tsv('raw_data/title.basics.tsv')
```
**Explanation**: Reads two datasets (`ratings` and `basics`) from tab-separated files and stores them into data frames.
4. Joining Datasets
```r
joined_data <- full_join(basics, ratings)
View(joined_data)
```
**Explanation**: Combines both datasets into one using a full outer join, preserving all rows from both. Displays the result.
5. Exploring the Data
```r
test <- joined_data
```
**Explanation**: Makes a working copy of the joined data for analysis and cleaning.
6. Data Cleaning
6.1 Counting Missing Values
```r
sum(is.na(test))
na_counts <- lapply(test, function(test) sum(is.na(test)))
```
**Explanation**: Counts how many missing values exist overall and in each column.

6.2 Identifying Outliers
```r
boxplot(test, col = 'lightblue', main = 'Distribution by Column', xlab = 'Variables', ylab = 'Values')
```
**Explanation**: Plots boxplots to visually inspect outliers across all variables.

6.3 Removing NA Values
```r
test <- na.omit(test)
```
**Explanation**: Removes any rows that have missing values.

6.4 Removing Duplicates
```r
total_duplicates <- sum(duplicated(test))
duplicates <- test %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()
test <- test[!duplicated(test),]
sum(duplicated(test))
```
**Explanation**: Identifies and removes duplicate rows in the dataset.

6.5 Cleaning Columns
```r
cleaned_v2 <- cleaned_v1 %>%
  select(-tconst, -originalTitle, -isAdult, -endYear)
View(cleaned_v2)
```
**Explanation**: Removes unnecessary columns like IDs, adult flag, and end year to simplify the dataset.
7. Frequency Distribution Analysis
7.1 Frequency Table of `titleType`
```r
View(fdtQl(cleaned_v2$titleType))
```
**Explanation**: Views a frequency distribution table for the `titleType` column.

7.2 Selecting Movies Only
```r
only_movies = filter(cleaned_v2, titleType == 'movie' | titleType == 'short' | titleType == 'tvMovie')
```
**Explanation**: Filters the dataset to include only actual movie types.
8. Movie Ratings Analysis
8.1 Top 100 Movies
```r
sorted_by_num_votes <- only_movies %>% arrange(desc(numVotes))
top_100_best_rated_movies <- sorted_by_num_votes[1:100, ]
top_100_best_rated_movies_according_to_rating <- top_100_best_rated_movies %>% arrange(desc(averageRating))
```
**Explanation**: Selects top 100 most voted movies, then ranks them by average rating.

8.2 Average Rating by Genre
```r
ggplot(average_rating_of_movies, aes(x=genres, y=avg_rating, fill=avg_rating)) + geom_bar(stat = 'identity') +
  scale_fill_gradient(low = 'blue', high = 'red')
```
**Explanation**: Creates a colored bar plot showing average ratings per genre.

8.3 Genre Analysis
```r
ggplot(genre_analysis, aes(x = genres, y = avg_rating, fill = genres)) +
  geom_bar(stat = 'identity', show.legend = FALSE)
```
**Explanation**: Another visualization of average ratings by genre without a legend.

8.4 Average Rating by Genre Over the Years
```r
plot <- average_ratings_by_genre_year %>%
  ggplot(aes(startYear, avg_rating, color = genres)) +
  geom_line() +
  facet_wrap(~genres) +
  theme(legend.position = 'none')
ggplotly(plot) %>% layout(hovermode = 'x unified')
```
**Explanation**: Shows how average ratings have changed over time for each genre using interactive line plots.

8.5 Correlation of Average Rating and Length Over the Years
```r
ggplot(genre_data_scaled, aes(x = startYear, y = ScaledValue, color = Factor, linetype = Factor)) +
  geom_line(aes(group = Factor, genres), size = 1.2) +
  facet_wrap(~genres)
```
**Explanation**: Visualizes the relationship between rating and movie length over time by genre.

8.7 Number of Movies by Genre
```r
ggplot(number_of_movies_each_year, aes(x=startYear, y=count)) +
  geom_line(aes(group = genres, color = genres), size=0.5) +
  facet_wrap(~genres, scales='free_y')
```
**Explanation**: Shows how many movies were made each year by genre, with individual scales.

9. Bayesian Rating Analysis
```r
C <- mean(horror_movies$rating, na.rm = TRUE)
m <- 100
horror_movies <- horror_movies %>%
  mutate(adjusted_rating = (C * m + horror_movies$averageRating * horror_movies$numVotes) / (horror_movies$numVotes + m))
```
**Explanation**: Applies Bayesian average rating to give more reliable scores, especially for movies with few votes.

10. Genre and Average Rating Correlation
```r
grouped_by_genre$runtimeMinutes <- as.numeric(as.character(grouped_by_genre$runtimeMinutes))
```
**Explanation**: Converts the `runtimeMinutes` column to numeric for further analysis (like correlation or plotting).

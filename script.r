# 1. Installing necessary libraries
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("plotly")

# 2. Loading the necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# 3. Reading the file
ratings <- read_tsv("raw_data/title.ratings.tsv")
basics <- read_tsv("raw_data/title.basics.tsv")

# 4. Joining both datasets using fulljoin
joined_data <- full_join(basics, ratings)

# 5. Reading the joined dataset
View(joined_data)

# 6. Creating a duplicate of data for working on it
test <- joined_data

# 7. Separating the needed columns

# 8. Starting Cleaning:
## 1. counting number of missing values
sum(is.na(test))
# lapply(x, function(x)) means list apply - it applies a function to each element of a list or vector and returns a lsit of results
na_counts <- lapply(test, function(test) sum(is.na(test)))

## 2. using boxplots for finding the outliers
boxplot(test,
        col = "lightblue",
        main = "Distribution by Column",
        xlab = "Variables",
        ylab = "Values"
)

## 3. removing the na values from the entire data set
test <- na.omit(test) # the dataset now has zero na values



## 4. counting number of duplicated data
total_duplicates <- sum(duplicated(test))

## 5. getting all of the duplicates in a new column
duplicates <- test %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()
## 6. getting rid of the duplicates
test <- test[!duplicated(test),]

## 7. checking number of duplicates for our unique-valued dataset
sum(duplicated(test)) # it is zero for we are good to go...

## 8. Creating the first cleaned version of the dataset
cleaned_v1 <- test

## 9.Getting rid of unnecessary columns
column_names <- names(cleaned_v1)

cleaned_v2 <- cleaned_v1 %>%
  select(-tconst, -originalTitle, -isAdult, -endYear)

View(cleaned_v2)
## 10. creating a function for creating frequency distribution table
fdtQl = function(x) {
  abs_freq = table(x)
  rel_freq = round(prop.table(abs_freq), 3)
  cum_freq = cumsum(rel_freq)
  fdtx = cbind(abs_freq, rel_freq, cum_freq)
}

## 11. checking the freq. distribution table of titleType which shows the type of the cinematic view (video, tv-series, ...)
## 1. checking the fdt of titleType for finding out the distribution of different movie types
View(fdtQl(cleaned_v2$titleType))
## 2. Selecting only movies (short, tvMovie, and moive)
only_movies = filter(cleaned_v2, titleType == 'movie' | titleType == 'short' | titleType == 'tvMovie')
> only_movies

# 9. The Analysis:
## 1. Top 100 movies according to to votes
## sorting all movies according to nubmber of votes
sorted_by_num_votes <- only_movies %>% arrange(desc(numVotes))
top_100_best_rated_movies <- sorted_by_num_votes[1:100, ]
top_100_best_rated_movies_according_to_rating <- top_100_best_rated_movies%>% arrange(desc(averageRating))
## getting top 100 best rated movies with highest number of votes
top_100_best_voted_movies_according_to_rating <- top_100_best_rated_movies_according_to_rating


## average rating of movies
## changing data type of runTime column to numeric
test$runtimeMinutes <- as.numeric(as.character(test$runtimeMinutes))

average_rating_of_movies <- test %>%
  group_by(genres) %>%
  summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    avg_length = mean(runtimeMinutes, na.rm = TRUE),
    max_rating = max(averageRating, na.rm = TRUE),
    min_rating = min(averageRating, na.rm = TRUE),
    count = n()
  )

View(average_rating_of_movies)
## graphing average rating of different genres
ggplot(average_rating_of_movies, aes(x=genres, y=avg_rating, fill=avg_rating)) + geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red")

genre_analysis <- test %>%
  # Separate multiple genres into rows (one genre per row)
  separate_rows(genres, sep = ",") %>%
  group_by(genres) %>%
  summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    avg_length = mean(runtimeMinutes, na.rm = TRUE),
    count = n()
)

View(genre_analysis)

# printing all genres
all_genres <- test %>%
  separate_rows(genres, sep = ",") %>%  # Split hyphen-separated genres into rows
  pull(genres) %>%                      # Extract the genres column as a vector
  unique() %>%                          # Get unique values
  sort()

print(all_genres)

# graphing average rating of genres
ggplot(genre_analysis, aes(x = genres, y = avg_rating, fill = genres)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend since x-axis shows genres
  labs(title = "Average Movie Ratings by Genre",
       subtitle = "Genres ordered by frequency",
       x = "Genre",
       y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# graphing average length of different genres
ggplot(genre_analysis, aes(x = genres, y = avg_length, fill = genres)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend since x-axis shows genres
  labs(title = "Average Movie Length by Genre",
       subtitle = "Genres ordered by frequency",
       x = "Genre",
       y = "Average Length in Minutes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# graphing number of movies in different genres
ggplot(genre_analysis, aes(x = genres, y = count, fill = genres)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Movie Count",
    subtitle = "Genres ordered by frequency",
    x = "Genre",
    y = "Number of movies in each genre"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    breaks = c(0, 100000, 200000, 300000, 400000, 500000) # creating more breaks
    ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 3. Average movie ratings by genre over the years
# separating different genres in movies
movies_split_genres <- only_movies %>%
  separate_rows(genres, sep = ',') %>%
  filter(genres != "\\N") %>%
  mutate(genres = trimws(genres))

View(movies_split_genres)
# finding average ratings in each genre per each year
average_ratings_by_genre_year <- movies_split_genres %>%
  group_by(genres, startYear) %>%
  summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    count = n()
  )

View(average_ratings_by_genre_year)

# changing startYear to numeric
average_ratings_by_genre_year <- average_ratings_by_genre_year %>%
  mutate(startYear = as.numeric(as.character(startYear)))

# plotting the data
plot <- average_ratings_by_genre_year %>%
  ggplot(aes(startYear, avg_rating, color = genres)) +
  geom_line() +
  facet_wrap(~genres) +
  theme(legend.position = "none")

ggplotly(plot) %>%
  layout(hovermode = "x unified")

# 4. average rating and average length by genre over the years' / corelation of average rating and average length over the years
movies_split_genres <- movies_split_genres %>%
  mutate(runtimeMinutes = as.numeric(as.character(runtimeMinutes)))

average_rating_and_average_length_by_genre_over_the_years <- movies_split_genres %>%
  group_by(genres, startYear) %>%
  summarise(
    avg_rating = mean(averageRating, na.rm = TRUE),
    avg_length = mean(runtimeMinutes, na.rm = TRUE),
    count = n()
  )

View(average_rating_and_average_length_by_genre_over_the_years)





genre_data_scaled <- average_rating_and_average_length_by_genre_over_the_years %>%
  group_by(genres) %>%
  mutate(
    ScaledRating = scale(avg_rating),
    ScaledLength = scale(avg_length)
  ) %>%
  pivot_longer(
    cols = c(ScaledRating, ScaledLength),
    names_to = "Factor",
    values_to = "ScaledValue"
  )

ggplot(genre_data_scaled, aes(x = startYear, y = ScaledValue, color = Factor, linetype = Factor)) +
  geom_line(aes(group = Factor, genres), size = 1.2) +
  geom_point(aes(size = count), alpha = 0.7) +
  facet_wrap(~genres) +
  labs(
    title = "Average Rating vs. Average Length (Scaled) Over Time",
    subtitle = "Faceted by Genre, point size = Movie Count",
    y = "Scaled Value",
    x = "Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("ScaledRating" = "steelblue", "ScaledLength" = "darkorange")) +
  scale_linetype_manual(values = c("ScaledRating" = "solid", "ScaledLength" = "dashed"))

# 5. Number of movies over the years
only_movies$startYear <- as.numeric(as.character(only_movies$startYear))

number_of_movies_per_year <- only_movies %>%
  group_by(startYear) %>%
  summarise(count = n())

View(number_of_movies_per_year)

number_of_movies_per_year <- only_movies %>%
  group_by(startYear) %>%
  summarise(count = n(), .groups = "drop")

# Create the plotly line chart
ggplot(number_of_movies_per_year, aes(x=startYear, y=count)) +
  geom_line() +
  facet_wrap(~)

## better plot with plotly
plot_ly(
  data = number_of_movies_per_year,
  x = ~startYear,
  y = ~count,
  type = 'scatter',
  mode = 'lines',  # Draw lines with markers
  line = list(color = 'green', dash = 'dot')   # Marker customization
) %>%
  layout(
    title = "Number of Movies Released Per Year",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Number of Movies"),
    hovermode = 'closest'  # Show closest point info on hover
  )



## number of movies for each genre
number_of_movies_each_year <- movies_split_genres %>%
  group_by(startYear, genres) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  )

number_of_movies_each_year$startYear <- as.numeric(as.character(number_of_movies_each_year$startYear))

## the plot
ggplot(number_of_movies_each_year, aes(x=startYear, y=count)) +
  geom_line(aes(group = genres, color = genres), size=0.5) +
  facet_wrap(~genres, scales="free_y")+
  labs(
    title = "Number of Movies Released Per Year by Genre",
    x = "Year",
    y = "Number of Movies"
  ) +
  theme_gray() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" # Rotate the x-axis labels for readability
  ) +
  scale_x_continuous(breaks = seq(1875, 2025, by = 25))

# top 100 highest rated movies in each genre
sorted_by_genre <- movies_split_genres %>% arrange(genres)

View(sorted_by_genre)
#function for making subsets of the dataset based on genre
get_genre_and_sort_by_num_of_votes_dataset <- function(target_genre) {
  sorted_by_genre %>% filter(genres == target_genre) %>% arrange(desc(numVotes))
}

horror_movies <- get_genre_and_sort_by_num_of_votes_dataset("Horror")
View(horror_movies)

# Bayesian rating
C <- mean(horror_movies$rating, na.rm = TRUE)
m <- 100

# Prototype: Applying Bayesian Rating
horror_movies <- horror_movies %>%
  mutate(adjusted_rating = (C * m + horror_movies$averageRating * horror_movies$numVotes) / (horror_movies$numVotes + m))

View(horror_movies)

# applying the function on each different genre
genres_list <- unique(sorted_by_genre$genres)

for (genre in genres_list) {
  var_name <- paste0(genre, "_movies")
  assign(var_name, get_genre_and_sort_by_num_of_votes_dataset(genre))
}

View(Comedy_movies)
View(Action_movies)


# 6. Genre and Average-rating correlation
grouped_by_genre <- group_by(sorted_by_genre, genres)

# checking data types of all of the columsn
for (name in names(grouped_by_genre)) {
  print(paste(name, "is", class(grouped_by_genre[[name]]))) # print(paste) for concatenating string and double square brackets for accessing array elements, because single pair is used for getting a []
}

grouped_by_genre$runtimeMinutes <- as.numeric(as.character(grouped_by_genre$runtimeMinutes))
summary_grouped_by_genre_wrt_averageRating <- summarise(grouped_by_genre, mean(averageRating, na.rm = TRUE), sd(averageRating, na.rm = TRUE), min(averageRating, na.rm = TRUE), max(averageRating, na.rm = TRUE))
View(summary_grouped_by_genre_wrt_averageRating)

ggplot(summary_grouped_by_genre_wrt_averageRating, aes( y = genres, x = `mean(averageRating, na.rm = TRUE)`, fill = genres)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Mean Average Rating per Genre",
    x = "Mean Average Rating",
    y = "Genre",
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Genre and Length Correlation
summary_grouped_by_genre_wrt_averageLength <- summarise(grouped_by_genre, mean(runtimeMinutes, na.rm = TRUE), sd(runtimeMinutes, na.rm = TRUE), min(runtimeMinutes, na.rm = TRUE), max(runtimeMinutes, na.rm = TRUE))
View(summary_grouped_by_genre_wrt_averageLength)


ggplot(summary_grouped_by_genre_wrt_averageLength, aes( x = `mean(runtimeMinutes, na.rm = TRUE)`, y = genres, fill = genres)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Length per Genre",
    x = "Average length in Minutes",
    y = "Genre"
  ) +
  scale_x_continuous(
    labels = scales::comma,
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95) # creating more breaks
  ) +
  theme(axis.text.x = element_text(angle  = 45, hjust = 1))

# 8. Top hundred best_rated shortest movies
# function for extracting different movies


just_shorts <-  only_movies %>% filter(titleType == "short")
View(just_shorts)

# 9. Best Short Movies
just_shorts <-  only_movies %>% filter(titleType == "short")
# best short movies with at least 5000 sorted by average ratings
test <- test %>% filter(numVotes >= 5000) %>% arrange(desc(averageRating))

# 10 Best Long Movies
long_movies <- only_movies %>% filter((titleType != "short") & (numVotes >= 100000)) %>% arrange(runtimeMinutes)
View(long_movies)
long_movies <- long_movies %>% arrange(desc(averageRating))
View(long_movies)

top_100_movies <- head(long_movies, 100)
View(top)

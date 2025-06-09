#install necessary libraries
library(readr)
library(dplyr) 
library(tidytext)
library(stringr)
library(tm)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

setwd("/Users/saha/Downloads")
spotify_data <- read_csv("Spotify-Dataset.csv") #Loading the dataset

#data preprocessing
head(spotify_data) #view the first few (usually 6 are shown) rows and columns of the dataset
#data cleaning ; check if there are any duplicate rows
any(duplicated(spotify_data))
mean(spotify_data$popularity)#if there is any missing value, it returns NA
#EDA
print("Summary Statistics:") #descriptive statistics
summary(spotify_data) #summary of the dfataset
cat("Mean Popularity:", mean(spotify_data$popularity)) #mean popularity
cat("Mean Duration (ms):", mean(spotify_data$duration_ms)) #mean duration (ms)
cat("Mean Danceability:", mean(spotify_data$danceability)) #mean danceability
cat("Mean Energy:", mean(spotify_data$energy)) #mean energy
cat("Mean Tempo:", mean(spotify_data$tempo)) #mean tempo
#correlation coefficient
cor(spotify_data$danceability, spotify_data$popularity)
cor(spotify_data$duration_ms, spotify_data$energy)
cor(spotify_data$energy, spotify_data$danceability)
cor(spotify_data$tempo, spotify_data$duration_ms)

#data analysis
#top 10 genres
top_genre <- spotify_data %>%
group_by(track_genre) %>%
summarise(mean_popularity = mean(popularity), song_count = n()) %>%
arrange(desc(mean_popularity)) %>%
head(10)
print("Top 10 Genres by Average Popularity:")
print(top_genre)

#top 10 songs
top_songs <- spotify_data %>%
arrange(desc(popularity)) %>%
select(track_name, artists, popularity) %>%
head(10)
print("Top 10 Songs by Average Popularity:")
print(top_songs)

#top 10 artists
top_artists <- spotify_data %>%
group_by(artists) %>%
summarise(mean_popularity = mean(popularity), song_count = n()) %>%
arrange(desc(mean_popularity)) %>% #sort by average popularity
head(10)  
print("Top 10 Artists by Average Popularity:")
print(top_artists)

#distribution of song popularity - bar plot
ggplot(spotify_data, aes(x = popularity)) +
geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
labs(title = "Distribution of Song Popularity", x = "Popularity", y = "Count") +
theme_minimal()

#danceability vs popularity scatter plot 
ggplot(spotify_data, aes(x = danceability, y = popularity)) +
geom_point(color = "lightgreen", alpha = 0.2) +
labs(title = "Danceability vs Popularity", x = "Danceability", y = "Popularity") +
theme_minimal()

#duration_ms vs popularity scatter plot 
ggplot(spotify_data, aes(x = duration_ms, y = popularity)) +
geom_point(color = "violet", alpha = 0.2) +
labs(title = "Duration vs Popularity", x = "Duration (ms)", y = "Popularity") +
theme_minimal()

#energy vs popularity scatter plot 
ggplot(spotify_data, aes(x = energy, y = popularity)) +
geom_point(color = "seagreen", alpha = 0.2) +
labs(title = "Energy vs Popularity", x = "Energy", y = "Popularity") +
theme_minimal()

#tempo vs popularity scatter plot 
ggplot(spotify_data, aes(x = tempo, y = popularity)) +
geom_point(color = "pink", alpha = 0.2) +
labs(title = "Tempo vs Popularity", x = "Tempo", y = "Popularity") +
theme_minimal()

#valence vs popularity scatter plot 
ggplot(spotify_data, aes(x = valence, y = popularity)) +
geom_point(color = "orange", alpha = 0.2) +
labs(title = "Valence vs Popularity", x = "Valence", y = "Popularity") +
theme_minimal()

#loudness vs popularity scatter plot 
ggplot(spotify_data, aes(x = loudness, y = popularity)) +
geom_point(color = "steelblue", alpha = 0.2) +
labs(title = "Loudness vs Popularity", x = "Loudness", y = "Popularity") +
theme_minimal()
#similarly we can find the vs popularity scatter plot for other numerical features too (acousticness, speechiness,instrumentalness,time_signature, etc...)

#visualization
ggplot(spotify_data, aes(x = popularity)) +
geom_density(fill = "purple", alpha = 0.5) +
labs(
    title = "Distribution of Song Popularity (Density Plot)",
    x = "Popularity",
    y = "Density"
  ) #densityplot

ggplot(genre_avg_popularity, aes(x = reorder(track_genre, avg_popularity), y = avg_popularity, fill = track_genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Popularity by Genre", x = "Genre", y = "Average Popularity") +
  theme_minimal()

#danceability by genre
ggplot(spotify_data, aes(x = track_genre, y = danceability)) +
  geom_boxplot() +
  labs(title = "Danceability by Genre", x = "Genre", y = "Danceability") +
  theme_minimal()

#energy by genre
ggplot(spotify_data, aes(x = track_genre, y = energy)) +
  geom_boxplot() +
  labs(title = "Energy by Genre", x = "Genre", y = "Energy") +
  theme_minimal()

#tempo by genre
ggplot(spotify_data, aes(x = track_genre, y = tempo)) +
  geom_boxplot() +
  labs(title = "Tempo by Genre", x = "Genre", y = "Tempo") +
  theme_minimal()

#recommendation system
recommend_songs <- function(song_name, method = "genre") {
  #find the selected song's details
  song_info <- spotify_data %>% filter(track_name == song_name)
  
  if (nrow(song_info) == 0) {
    stop("Song not found in the database!")
  }
  if (method == "genre") {
    #recommend songs with the same genre
    recommendations <- spotify_data %>%
      filter(track_genre == song_info$track_genre & track_name != song_name) %>%
      select(track_name, track_genre, popularity) %>%
      arrange(desc(popularity)) %>%
      head(10)
    
  } else if (method == "popularity") {
    #recommend songs with similar popularity
    recommendations <- spotify_data %>%
      filter(
        abs(popularity - song_info$popularity) < 5 & track_name != song_name
      ) %>%
      select(track_name, track_genre, popularity) %>%
      arrange(desc(popularity)) %>%
      head(10)
  }
  return(recommendations)
}
#example for recommending similar genre songs
recommendations_genre <- recommend_songs("N95", method = "genre") 
print("Recommended Songs by Genre:")
print(recommendations_genre)

#example for recommending similar popular songs
recommendations_popularity <- recommend_songs("White Ferrari", method = "popularity")  
print("Recommended Songs by Similar Popularity:")
print(recommendations_popularity)

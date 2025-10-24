# Spotify Data Analysis & Recommendation System (R)

This project performs **Exploratory Data Analysis (EDA)**, **data visualization**, and builds a **simple recommendation system** on a Spotify dataset using **R**.  
It explores relationships between features like *danceability, energy, tempo,* and *popularity* — and recommends similar songs based on genre or popularity.

---

## Features

- **Data Cleaning & Preprocessing**
  - Removes duplicates and checks for missing values
  - Computes descriptive statistics and correlations

- **Exploratory Data Analysis (EDA)**
  - Summary statistics for key numerical variables
  - Correlation between danceability, energy, tempo, and popularity
  - Top 10 genres, artists, and songs by popularity

- **Visualizations**
  - Distribution of popularity (histogram & density plots)
  - Scatter plots for popularity vs. energy, danceability, tempo, etc.
  - Boxplots for danceability, energy, and tempo across genres

- **Recommendation System**
  - **Genre-based recommendation**: Suggests top 10 songs in the same genre
  - **Popularity-based recommendation**: Suggests songs with similar popularity scores

---

## Technologies Used

- **R Programming**
- **Libraries:**
  - `readr` — For CSV import
  - `dplyr` — Data manipulation
  - `tidytext`, `stringr`, `tm` — Text cleaning and processing
  - `ggplot2` — Data visualization
  - `wordcloud`, `RColorBrewer` — Word clouds and color palettes

---

## Key Insights

- The average popularity across tracks provides an understanding of dataset distribution.
- Danceability and energy show noticeable influence on popularity.
- Certain genres consistently rank higher in both average popularity and track count.
- Visual patterns reveal correlations that could be leveraged in recommendation systems.

---

## How to Run the Project

1. Clone the repository or download the R script.
2. Install required R libraries:
   ```r
   install.packages(c("readr", "dplyr", "tidytext", "stringr", "tm", "ggplot2", "wordcloud", "RColorBrewer"))
3. Set the working directory to where your CSV file is stored:
```r
setwd("/path/to/your/folder")
```
4. Run the R script.
Example usage of the recommender:
```r
recommend_songs("N95", method = "genre")
recommend_songs("White Ferrari", method = "popularity"
```

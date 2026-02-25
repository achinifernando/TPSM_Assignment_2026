# Load libraries
library(tidyverse)
library(psych)
library(lm.beta)
library(corrplot)
library(ggplot2)
library(dplyr)

# Set working directory
setwd("G:\\TPSM_Assignment_2026\\data\\raw\\MovieData")

# Import all files
movies <- read.csv("movies.csv", stringsAsFactors = FALSE)
recommendations <- read.csv("recommendation_logs.csv", stringsAsFactors = FALSE)
reviews <- read.csv("reviews.csv", stringsAsFactors = FALSE)
search_log <- read.csv("search_logs.csv", stringsAsFactors = FALSE)
users <- read.csv("users.csv", stringsAsFactors = FALSE)
watch_history <- read.csv("watch_history.csv", stringsAsFactors = FALSE)

# Process users file demographics
users_clean <- users %>%
  select(user_id, age, gender, country) %>%
  mutate(
    # Clean age
    age_clean = gsub("[^0-9.-]", "", as.character(age)),
    age_numeric = as.numeric(age_clean),
    age = ifelse(age_numeric < 5 | age_numeric > 100 | is.na(age_numeric), NA, age_numeric),
    
    # Age Groups
    age_group = case_when(
      age >= 5  & age <= 12 ~ "5-12",
      age >= 13 & age <= 18 ~ "13-18",
      age >= 19 & age <= 25 ~ "19-25",
      age >= 26 & age <= 35 ~ "26-35",
      age >= 36 & age <= 50 ~ "36-50",
      age > 50               ~ "50+",
      TRUE                   ~ "Unknown"
    ),
    
    # Clean Gender
    gender = case_when(
      tolower(gender) %in% c("m", "male", "man") ~ "Male",
      tolower(gender) %in% c("f", "female", "woman") ~ "Female",
      TRUE ~ "Other/Not Specified"
    ),
    
    # Clean Country
    country = trimws(as.character(country))
  )

# Process REVIEWS file for Emotional Connection
reviews_clean <- reviews %>%
  select(user_id, movie_id, rating, helpful_votes, sentiment_score, review_date) %>%
  mutate(
    rating = as.numeric(rating),
    helpful_votes = as.numeric(helpful_votes),
    sentiment_score = as.numeric(sentiment_score),
    
    # FIX 1: Simple arithmetic instead of case_when
    emotional_score = (sentiment_score * 5) + rating,
    
    # Cap between 0-10
    emotional_score = pmax(0, pmin(10, emotional_score)),
    
    review_year = lubridate::year(as.Date(review_date))
  ) %>%
  filter(!is.na(emotional_score))

# Aggregate by user - emotional connection
user_emotional <- reviews_clean %>%
  group_by(user_id) %>%
  summarise(
    emotional_connection = mean(emotional_score, na.rm = TRUE),
    emotional_sd = sd(emotional_score, na.rm = TRUE),
    emotional_min = min(emotional_score, na.rm = TRUE),
    emotional_max = max(emotional_score, na.rm = TRUE),
    num_reviews = n(),
    avg_rating_given = mean(rating, na.rm = TRUE),
    avg_sentiment = mean(sentiment_score, na.rm = TRUE),
    total_helpful_votes = sum(helpful_votes, na.rm = TRUE),
    first_review_year = min(review_year, na.rm = TRUE),
    last_review_year = max(review_year, na.rm = TRUE),
    pct_high_sentiment = mean(sentiment_score > 0.7, na.rm = TRUE) * 100,
    pct_low_sentiment = mean(sentiment_score < 0.3, na.rm = TRUE) * 100
  ) %>%
  filter(num_reviews >= 3, emotional_sd > 0 | is.na(emotional_sd))

cat("Users with emotional connection scores:", nrow(user_emotional), "\n")  #1709

# Process WATCH_HISTORY file for Satisfaction
watch_clean <- watch_history %>%
  select(user_id, movie_id, watch_date, user_rating, watch_duration_minutes, 
         progress_percentage, device_type, location_country) %>%
  mutate(
    user_rating = as.numeric(user_rating),
    watch_duration_minutes = as.numeric(watch_duration_minutes),
    progress_percentage = as.numeric(progress_percentage),
    satisfaction = user_rating,
    
    engagement_level = case_when(
      progress_percentage >= 90 ~ "High Completion",
      progress_percentage >= 50 ~ "Medium Completion",
      progress_percentage >= 25 ~ "Low Completion",
      TRUE ~ "Barely Watched"
    ),
    
    watch_year = lubridate::year(as.Date(watch_date)),
    watch_month = lubridate::month(as.Date(watch_date))
  ) %>%
  filter(!is.na(satisfaction))

# Aggregate by user - satisfaction
user_satisfaction <- watch_clean %>%
  group_by(user_id) %>%
  summarise(
    satisfaction = mean(satisfaction, na.rm = TRUE),
    satisfaction_sd = sd(satisfaction, na.rm = TRUE),
    satisfaction_min = min(satisfaction, na.rm = TRUE),
    satisfaction_max = max(satisfaction, na.rm = TRUE),
    num_ratings = n(),
    num_movies_watched = n_distinct(movie_id),
    avg_watch_duration = mean(watch_duration_minutes, na.rm = TRUE),
    avg_completion = mean(progress_percentage, na.rm = TRUE),
    pct_high_completion = mean(progress_percentage >= 90, na.rm = TRUE) * 100,
    first_watch_year = min(watch_year, na.rm = TRUE),
    last_watch_year = max(watch_year, na.rm = TRUE),
    mobile_ratio = mean(device_type == "Mobile", na.rm = TRUE),
    desktop_ratio = mean(device_type == "Desktop", na.rm = TRUE),
    tablet_ratio = mean(device_type == "Tablet", na.rm = TRUE),
    tv_ratio = mean(device_type == "Smart TV", na.rm = TRUE)
  ) %>%
  filter(num_ratings >= 3, satisfaction_sd > 0 | is.na(satisfaction_sd))

# Combine all files into master dataset
master_data <- users_clean %>%
  left_join(user_emotional, by = "user_id") %>%
  left_join(user_satisfaction, by = "user_id")

# FIX 2: Handle duplicates in user_genres - ROBUST VERSION

cat("\n--- Cleaning user_genres data ---\n")

# Step 1: Check original sizes
cat("Original watch_history rows:", nrow(watch_history), "\n")  #105000 
cat("Original movies rows:", nrow(movies), "\n")  #1040

# Step 2: Clean watch_history - remove duplicates
watch_history_clean <- watch_history %>%
  distinct(user_id, movie_id, .keep_all = TRUE)

cat("watch_history after deduplication:", nrow(watch_history_clean), "rows (", 
    nrow(watch_history) - nrow(watch_history_clean), "duplicates removed)\n")  # 99470 rows ( 5530 duplicates removed)

# Step 3: Clean movies - ensure unique movie_ids
movies_clean <- movies %>%
  distinct(movie_id, .keep_all = TRUE) %>%
  select(movie_id, genre_primary)

cat("movies after deduplication:", nrow(movies_clean), "rows (", 
    nrow(movies) - nrow(movies_clean), "duplicates removed)\n")  #1000 rows ( 40 duplicates removed)

# Step 4: Check for missing genres
movies_with_genre <- movies_clean %>%
  filter(!is.na(genre_primary) & genre_primary != "")

cat("movies with valid genre:", nrow(movies_with_genre), "rows (", 
    nrow(movies_clean) - nrow(movies_with_genre), "without genre)\n")  #1000 rows ( 0 without genre)

# Step 5: Perform join
user_genres_raw <- watch_history_clean %>%
  left_join(movies_with_genre, by = "movie_id", relationship = "many-to-many")

cat("After join:", nrow(user_genres_raw), "rows\n") #99470 rows

# Step 6: Remove movies without genre
user_genres_with_genre <- user_genres_raw %>%
  filter(!is.na(genre_primary))

cat("After removing missing genres:", nrow(user_genres_with_genre), "rows (", 
    nrow(user_genres_raw) - nrow(user_genres_with_genre), "removed)\n")  #99470 rows ( 0 removed)

# Step 7: Get preferred genre per user
user_genres <- user_genres_with_genre %>%
  group_by(user_id, genre_primary) %>%
  summarise(genre_count = n(), .groups = 'drop') %>%
  group_by(user_id) %>%
  slice_max(order_by = genre_count, n = 1, with_ties = FALSE) %>%
  select(user_id, preferred_genre = genre_primary)

cat("Final user_genres created with", nrow(user_genres), "users\n") # 10000 users

# Step 8: Show top genres
cat("\nTop 10 preferred genres:\n")
user_genres %>%
  count(preferred_genre) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  print()

# Join with explicit relationship handling
master_data <- master_data %>%
  left_join(user_genres, by = "user_id", relationship = "many-to-many") %>%
  distinct(user_id, .keep_all = TRUE)  # Keep one row per user

# Final filtering
master_data_final <- master_data %>%
  filter(
    !is.na(emotional_connection),
    !is.na(satisfaction),
    !is.na(age),
    !is.na(gender),
    num_reviews >= 3,
    num_ratings >= 3
  )

cat("\nFinal dataset size:", nrow(master_data_final), "users\n")  #513 users

# AFTER your data cleaning (with your 482 rows)
set.seed(123)

# Create bootstrap sample (sampling WITH replacement)
movie_1500 <- master_data_final %>%
  sample_n(size = 1500, replace = TRUE)



##### DESCRPTIVE ANALYSIS #####

# PART 1 -DEMOGRAPHIC DESCRIPTION

# Age distribution
age_stats <- movie_1500 %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE)
  )
print("Age Statistics:")
print(age_stats)  # mean_age  sd_age min_age max_age median_age
                  # 35.61467 12.2182       5      96         36

# Age groups distribution
age_group_dist <- movie_1500 %>%
  count(age_group) %>%
  mutate(percentage = n/sum(n)*100)
print("\nAge Group Distribution:")
print(age_group_dist) #age_group   n    percentage
                      # 5-12      59        3.933333
                      # 13-18     62       4.133333
                      # 19-25     164       10.933333
                      # 26-35     450       30.000000
                      # 36-50     587       39.133333
                      # 50+       178         11.866667

# Gender distribution
gender_dist <- movie_1500 %>%
  count(gender) %>%
  mutate(percentage = n/sum(n)*100)
print("Gender Distribution:")
print(gender_dist)    #gender                 n     percentage
                      # Female                526   35.06667
                      # Male                  624   41.60000
                      # Other/Not Specified   350   23.33333

# Country distribution (top 10)
country_dist <- movie_1500 %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  mutate(percentage = n/sum(n)*100)
print("\nTop 10 Countries:")
print(country_dist)   #country    n     percentage
                      # USA       1063   70.86667
                      # Canada    437   29.13333

# Visualize demographics
# Age histogram
ggplot(movie_1500, aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Age Distribution of Users",
       x = "Age", y = "Count") +
  theme_minimal()

# Gender bar chart
ggplot(movie_1500, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray"))

# PART 2 - EMOTIONAL CONNECTION DESCRIPTION

# Basic statistics
emotional_stats <- movie_1500 %>%
  summarise(
    mean_emotional = mean(emotional_connection, na.rm = TRUE),
    sd_emotional = sd(emotional_connection, na.rm = TRUE),
    min_emotional = min(emotional_connection, na.rm = TRUE),
    max_emotional = max(emotional_connection, na.rm = TRUE),
    median_emotional = median(emotional_connection, na.rm = TRUE),
    skewness = psych::skew(emotional_connection),
    kurtosis = psych::kurtosi(emotional_connection)
  )
print("Emotional Connection Statistics:")
print(emotional_stats)   #mean_emotional sd_emotional min_emotional max_emotional median_emotional
                        # 6.848319     1.319502      3.056667         9.775         7.021667
                        
                        #skewness   kurtosis
                        #-0.3643297 -0.2460691

# Distribution visualization
# Histogram
ggplot(movie_1500, aes(x = emotional_connection)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "red", size = 1) +
  labs(title = "Distribution of Emotional Connection Scores",
       x = "Emotional Connection (0-10 scale)", y = "Count") +
  theme_minimal() +
  geom_vline(xintercept = mean(movie_1500$emotional_connection), 
             color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = mean(movie_1500$emotional_connection) + 0.5, 
           y = 50, label = "Mean", color = "blue")

# Boxplot
ggplot(movie_1500, aes(y = emotional_connection)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Boxplot of Emotional Connection",
       y = "Emotional Connection Score") +
  theme_minimal()

# Emotional connection by demographics
# By gender
emo_by_gender <- movie_1500 %>%
  group_by(gender) %>%
  summarise(
    mean_emo = mean(emotional_connection, na.rm = TRUE),
    sd_emo = sd(emotional_connection, na.rm = TRUE),
    n = n()
  )
print("Emotional Connection by Gender:")
print(emo_by_gender)  #gender              mean_emo   sd_emo    n
                      #<chr>                  <dbl>  <dbl>    <int>
                      #Female                  6.96   1.28    526
                      #Male                    6.75   1.37    624
                      #Other/Not Specified     6.85   1.27    350

# By age group
emo_by_age <- movie_1500 %>%
  group_by(age_group) %>%
  summarise(
    mean_emo = mean(emotional_connection, na.rm = TRUE),
    sd_emo = sd(emotional_connection, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(age_group)
print("Emotional Connection by Age Group:")
print(emo_by_age)    # age_group    mean_emo    sd_emo  n
                    #<chr>          <dbl>       <dbl> <int>
                    # 5-12          7.09        1.02    59
                    # 13-18         7.24        1.13    62
                    #19-25         7.15         1.19   164
                    #26-35         6.80         1.42   450
                    #36-50         6.85         1.29   587
                    #50+           6.47         1.33   178

# Visualize emotional connection by groups
# By gender
ggplot(movie_1500, aes(x = gender, y = emotional_connection, fill = gender)) +
  geom_boxplot() +
  labs(title = "Emotional Connection by Gender",
       x = "Gender", y = "Emotional Connection") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue"))

# By age group
ggplot(movie_1500, aes(x = age_group, y = emotional_connection, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Emotional Connection by Age Group",
       x = "Age Group", y = "Emotional Connection") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# PART 3 - SATISFACTION DESCRIPTION (COMPLETE WITH VISUALIZATIONS)

# 3.1 Basic statistics (you already have this)
satisfaction_stats <- movie_1500 %>%
  summarise(
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    min_sat = min(satisfaction, na.rm = TRUE),
    max_sat = max(satisfaction, na.rm = TRUE),
    median_sat = median(satisfaction, na.rm = TRUE),
    skewness = psych::skew(satisfaction),
    kurtosis = psych::kurtosi(satisfaction)
  )
print("Satisfaction Statistics:")
print(satisfaction_stats)   #mean_sat   sd_sat      min_sat     max_sat   median_sat   skewness   kurtosis
                            #3.37768    0.6781707   1.333333       5      3.333333    -0.3174264 -0.1333199

# 3.2 Distribution visualization (you already have these)
# Histogram with density
ggplot(movie_1500, aes(x = satisfaction)) +
  geom_histogram(bins = 30, fill = "purple", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "red", size = 1) +
  labs(title = "Distribution of Satisfaction Scores",
       x = "Satisfaction (0-10 scale?)", y = "Count") +
  theme_minimal() +
  geom_vline(xintercept = mean(movie_1500$satisfaction), 
             color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = mean(movie_1500$satisfaction) + 0.5, 
           y = 50, label = paste("Mean =", round(mean(movie_1500$satisfaction), 2)), 
           color = "blue")

# Boxplot
ggplot(movie_1500, aes(x = "All Users", y = satisfaction)) +
  geom_boxplot(fill = "purple", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
  annotate("text", x = 1.4, y = mean(movie_1500$satisfaction), 
           label = paste("Mean =", round(mean(movie_1500$satisfaction), 2)), 
           color = "red") +
  labs(title = "Boxplot of Satisfaction",
       x = "", y = "Satisfaction Score") +
  theme_minimal()





# 3.3 Satisfaction by Gender - VISUALIZATION
# Boxplot by gender
ggplot(movie_1500, aes(x = gender, y = satisfaction, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(title = "Satisfaction by Gender",
       x = "Gender", y = "Satisfaction Score") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray")) +
  theme(legend.position = "none")

# Bar chart with means and error bars for gender
sat_by_gender_summary <- movie_1500 %>%
  group_by(gender) %>%
  summarise(
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    n = n(),
    se = sd_sat / sqrt(n),
    ci_lower = mean_sat - 1.96 * se,
    ci_upper = mean_sat + 1.96 * se
  )

ggplot(sat_by_gender_summary, aes(x = gender, y = mean_sat, fill = gender)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Mean Satisfaction by Gender (with 95% CI)",
       x = "Gender", y = "Mean Satisfaction Score") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray")) +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0("n=", n)), y = 1, color = "black")

# 3.4 Satisfaction by Age Group - VISUALIZATION
# Boxplot by age group
ggplot(movie_1500, aes(x = age_group, y = satisfaction, fill = age_group)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(title = "Satisfaction by Age Group",
       x = "Age Group", y = "Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Blues")

# Bar chart with means and error bars for age group
sat_by_age_summary <- movie_1500 %>%
  group_by(age_group) %>%
  summarise(
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    n = n(),
    se = sd_sat / sqrt(n),
    ci_lower = mean_sat - 1.96 * se,
    ci_upper = mean_sat + 1.96 * se
  ) %>%
  arrange(age_group)

ggplot(sat_by_age_summary, aes(x = age_group, y = mean_sat, fill = age_group)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Mean Satisfaction by Age Group (with 95% CI)",
       x = "Age Group", y = "Mean Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = paste0("n=", n)), y = 1, color = "black")

# 3.5 Satisfaction by Country - VISUALIZATION (Top countries only)
# Get top 8 countries by frequency
top_countries <- movie_1500 %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(8) %>%
  pull(country)

# Filter for top countries
movie_top_countries <- movie_1500 %>%
  filter(country %in% top_countries)

# Boxplot by country
ggplot(movie_top_countries, aes(x = reorder(country, satisfaction, FUN = median), 
                                y = satisfaction, fill = country)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Satisfaction by Country (Top 8 Countries)",
       x = "Country", y = "Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_flip()  # Flip coordinates for better readability

# Bar chart with means for top countries
sat_by_country_summary <- movie_1500 %>%
  filter(country %in% top_countries) %>%
  group_by(country) %>%
  summarise(
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    n = n(),
    se = sd_sat / sqrt(n),
    ci_lower = mean_sat - 1.96 * se,
    ci_upper = mean_sat + 1.96 * se
  ) %>%
  arrange(desc(mean_sat))

ggplot(sat_by_country_summary, aes(x = reorder(country, mean_sat), y = mean_sat, fill = country)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Mean Satisfaction by Country (Top 8, with 95% CI)",
       x = "Country", y = "Mean Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  geom_text(aes(label = paste0("n=", n)), y = 0.5, color = "black") +
  coord_flip()


# 3.6 Combined visualization - Satisfaction across multiple demographics
# Create a faceted plot to compare gender and age together
ggplot(movie_1500, aes(x = age_group, y = satisfaction, fill = gender)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  labs(title = "Satisfaction by Age Group and Gender",
       x = "Age Group", y = "Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray"))

# 3.7 Create a summary table for satisfaction by demographics
satisfaction_demographics_table <- movie_1500 %>%
  group_by(age_group, gender) %>%
  summarise(
    mean_satisfaction = round(mean(satisfaction, na.rm = TRUE), 2),
    sd_satisfaction = round(sd(satisfaction, na.rm = TRUE), 2),
    n = n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = gender,
    values_from = c(mean_satisfaction, sd_satisfaction, n),
    values_fill = 0
  )

print("\nSatisfaction by Age Group and Gender (Cross-tabulation):")
print(satisfaction_demographics_table)
# age_group          mean_satisfaction_Female   mean_satisfaction_Male
#<chr>                        <dbl>                  <dbl>
#0-12                          3.66                   3.47
# 13-18                         3.59                   3.3 
# 19-25                         3.41                   3.3 
# 26-35                         3.29                   3.39
# 36-50                         3.26                   3.35
# 50+                           3.3                    3.58

# PART 4: EMOTION-SATISFACTION RELATIONSHIP

# 4.1 Overall correlation
cor_test <- cor.test(movie_1500$emotional_connection, 
                     movie_1500$satisfaction, 
                     method = "pearson")

print("Correlation between Emotional Connection and Satisfaction:")
print(paste("Pearson r =", round(cor_test$estimate, 3)))  #r = 0.033
print(paste("p-value =", round(cor_test$p.value, 4)))     #p-value = 0.2033
print(paste("95% CI: [", round(cor_test$conf.int[1], 3), ",", 
            round(cor_test$conf.int[2], 3), "]"))          #[ -0.018 , 0.083 ]

# 4.2 Scatter plot with trend line - FIXED
ggplot(movie_1500, aes(x = emotional_connection, y = satisfaction)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE, fill = "pink") +
  labs(title = "Relationship Between Emotional Connection and Satisfaction",
       x = "Emotional Connection Score", 
       y = "Satisfaction Score") +
  theme_minimal() +
  annotate("text", 
           x = min(movie_1500$emotional_connection) + 1, 
           y = max(movie_1500$satisfaction) - 0.5,
           label = paste("r =", round(cor_test$estimate, 3), 
                         "\np =", round(cor_test$p.value, 4)),
           hjust = 0)

# 4.3 Create emotional connection groups (quartiles)
movie_1500 <- movie_1500 %>%
  mutate(
    emotional_group = case_when(
      emotional_connection < quantile(emotional_connection, 0.25) ~ "Low Emotion",
      emotional_connection < quantile(emotional_connection, 0.50) ~ "Medium-Low",
      emotional_connection < quantile(emotional_connection, 0.75) ~ "Medium-High",
      TRUE ~ "High Emotion"
    )
  )

# 4.4 Satisfaction by emotional group
sat_by_emotion_group <- movie_1500 %>%
  group_by(emotional_group) %>%
  summarise(
    mean_satisfaction = mean(satisfaction, na.rm = TRUE),
    sd_satisfaction = sd(satisfaction, na.rm = TRUE),
    n = n(),
    se = sd_satisfaction / sqrt(n),
    ci_lower = mean_satisfaction - 1.96 * se,
    ci_upper = mean_satisfaction + 1.96 * se
  ) %>%
  arrange(emotional_group)

print("\nSatisfaction by Emotional Connection Group:")
print(sat_by_emotion_group) 
#emotional_group mean_satisfaction sd_satisfaction     n     se ci_lower
#<chr>                       <dbl>           <dbl> <int>  <dbl>    <dbl>
# High Emotion                 3.41           0.720   377 0.0371     3.34
# Low Emotion                  3.33           0.679   374 0.0351     3.26
# Medium-High                  3.36           0.732   374 0.0378     3.28
# Medium-Low                   3.41           0.569   375 0.0294     3.36



# 4.5 Bar chart of satisfaction by emotional group
ggplot(sat_by_emotion_group, aes(x = emotional_group, y = mean_satisfaction, 
                                 fill = emotional_group)) +
  geom_col() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Satisfaction by Emotional Connection Level",
       x = "Emotional Connection Group", 
       y = "Mean Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low Emotion" = "red", 
                               "Medium-Low" = "orange",
                               "Medium-High" = "lightgreen", 
                               "High Emotion" = "darkgreen"))



# 4.6 Does the relationship hold across groups? (quick check)
# By gender
cor_by_gender <- movie_1500 %>%
  group_by(gender) %>%
  summarise(
    correlation = cor(emotional_connection, satisfaction, method = "pearson"),
    p_value = cor.test(emotional_connection, satisfaction)$p.value,
    n = n()
  )
print("\nCorrelation by Gender:")
print(cor_by_gender)
#gender              correlation    p_value       n
#<chr>                     <dbl>      <dbl>     <int>
#Female                 -0.0497     0.255        526
#Male                   -0.00840    0.834        624
#Other/Not Specified     0.251      0.00000199   350



# By age group
cor_by_age <- movie_1500 %>%
  group_by(age_group) %>%
  filter(n() >= 10) %>%
  summarise(
    correlation = cor(emotional_connection, satisfaction, method = "pearson"),
    p_value = cor.test(emotional_connection, satisfaction)$p.value,
    n = n()
  )
print("\nCorrelation by Age Group:")
print(cor_by_age)
# age_group correlation p_value     n
#<chr>           <dbl>   <dbl> <int>
# 0-12         0.149     0.260     59
#13-18        0.109     0.397     62
#19-25        0.190     0.0149   164
#26-35        0.0664    0.160    450
#36-50       -0.000285  0.995    587
#50+         -0.0706    0.349    178



# ============================================================================
# PART A: MAIN HYPOTHESIS TESTING
# Testing: "Audiences who feel emotionally connected report higher satisfaction"
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("PART A: MAIN HYPOTHESIS TESTING\n")
cat(rep("=", 80), "\n")

#------------------------------------------------------------------------------
# Step 1: Address Scale Mismatch First
#------------------------------------------------------------------------------

cat("\n--- STEP 1: Addressing Scale Mismatch ---\n")
cat("\nVariable Scales:\n")
cat("  Emotional Connection: Range", range(movie_1500$emotional_connection)[1], "-", 
    range(movie_1500$emotional_connection)[2], 
    "(Mean =", round(mean(movie_1500$emotional_connection), 2), ")\n")
cat("  Satisfaction: Range", range(movie_1500$satisfaction)[1], "-", 
    range(movie_1500$satisfaction)[2], 
    "(Mean =", round(mean(movie_1500$satisfaction), 2), ")\n")

# Create standardized variables for fair comparison
movie_1500 <- movie_1500 %>%
  mutate(
    emotional_connection_std = as.numeric(scale(emotional_connection)),
    satisfaction_std = as.numeric(scale(satisfaction)),
    age_std = as.numeric(scale(age))
  )

cat("\n✓ Created standardized variables (mean ≈ 0, SD ≈ 1)\n")

#------------------------------------------------------------------------------
# Step 2: Simple Regression (Emotional Connection ONLY)
#------------------------------------------------------------------------------

cat("\n--- STEP 2: Simple Regression (Without Demographics) ---\n")

# Raw model
model_simple_raw <- lm(satisfaction ~ emotional_connection, data = movie_1500)
cat("\n▶ Raw Model (original scales):\n")
cat("   Coefficient =", round(coef(model_simple_raw)[2], 4), "\n")   #0.0169
cat("   p-value =", round(summary(model_simple_raw)$coefficients[2, 4], 4), "\n")  #0.2033
cat("   R² =", round(summary(model_simple_raw)$r.squared, 4), "\n")  # R² = 0.0011

# Standardized model
model_simple_std <- lm(satisfaction_std ~ emotional_connection_std, data = movie_1500)
cat("\n▶ Standardized Model (scale-free):\n")
cat("   Standardized β =", round(coef(model_simple_std)[2], 4), "\n")  #β = 0.0329 
cat("   p-value =", round(summary(model_simple_std)$coefficients[2, 4], 4), "\n")   #p-vaue = 0.2033 
cat("   R² =", round(summary(model_simple_std)$r.squared, 4), "\n")  #R² = 0.0011 

# Interpretation
if(summary(model_simple_raw)$coefficients[2, 4] < 0.05) {
  cat("\n✓ SIMPLE MODEL: Emotional connection SIGNIFICANTLY predicts satisfaction\n")
} else {
  cat("\n✗ SIMPLE MODEL: Emotional connection does NOT significantly predict satisfaction\n")
}

#------------------------------------------------------------------------------
# Step 3: Multiple Regression (WITH Demographics as Controls)
#------------------------------------------------------------------------------

cat("\n--- STEP 3: Multiple Regression (With Demographics) ---\n")

# Create dummy variables for gender (if not already done)
if(!"gender_male" %in% names(movie_1500)) {
  movie_1500 <- movie_1500 %>%
    mutate(
      gender_male = ifelse(gender == "Male", 1, 0),
      gender_other = ifelse(gender == "Other/Not Specified", 1, 0)
    )
}

# Create age group dummies (if not already done)
if(!"age_25_34" %in% names(movie_1500)) {
  movie_1500 <- movie_1500 %>%
    mutate(
      age_13_18 = ifelse(age_group == "13-18", 1, 0),
      age_19_25 = ifelse(age_group == "19-25", 1, 0),
      age_26_35 = ifelse(age_group == "26-35", 1, 0),
      age_36_50 = ifelse(age_group == "36-50", 1, 0),
      age_50plus = ifelse(age_group == "50+", 1, 0)
    )
}

# RAW MODEL (original scales)
model_full_raw <- lm(satisfaction ~ emotional_connection + age + 
                       gender_male + gender_other + 
                       age_13_18 + age_19_25 + age_26_35 + age_36_50 + age_50plus,  # age_5_12 is reference 
                     data = movie_1500)

cat("\n▶ RAW MODEL RESULTS (original scales):\n")
print(summary(model_full_raw))

# Extract emotional connection coefficient
emo_coef_raw <- coef(model_full_raw)["emotional_connection"]
emo_p_raw <- summary(model_full_raw)$coefficients["emotional_connection", 4]

cat("\n▶ Emotional Connection (with demographics controlled):\n")
cat("   Raw coefficient =", round(emo_coef_raw, 4), "\n")
cat("   p-value =", round(emo_p_raw, 4), "\n")

# STANDARDIZED MODEL (for fair comparison)
model_full_std <- lm(satisfaction_std ~ emotional_connection_std + age_std + 
                       gender_male + gender_other + 
                       age_25_34 + age_35_49 + age_50plus, 
                     data = movie_1500)

cat("\n▶ STANDARDIZED MODEL RESULTS (scale-free):\n")
summary_std <- summary(model_full_std)
print(summary_std)

# Extract standardized coefficient
emo_coef_std <- coef(model_full_std)["emotional_connection_std"]
emo_p_std <- summary_std$coefficients["emotional_connection_std", 4]

cat("\n▶ Emotional Connection (standardized):\n")
cat("   Standardized β =", round(emo_coef_std, 4), "\n")  # β = 0.0403 
cat("   p-value =", round(emo_p_std, 4), "\n")  #0.1203

#------------------------------------------------------------------------------
# Step 4: Test if Adding Demographics Improves the Model
#------------------------------------------------------------------------------

cat("\n--- STEP 4: Model Comparison ---\n")

# Compare simple vs full model
model_comparison <- anova(model_simple_raw, model_full_raw)
print(model_comparison)

if(model_comparison$`Pr(>F)`[2] < 0.05) {
  cat("\n✓ Adding demographics SIGNIFICANTLY improves the model\n")
} else {
  cat("\n✗ Adding demographics does NOT significantly improve the model\n")
}

#------------------------------------------------------------------------------
# Step 5: Check if Emotional Connection is Still Significant After Controls
#------------------------------------------------------------------------------

cat("\n--- STEP 5: MAIN HYPOTHESIS CONCLUSION ---\n")

cat("\nComparison of Emotional Connection Effect:\n")
cat("   Without demographics: p =", round(summary(model_simple_raw)$coefficients[2, 4], 4), "\n")
cat("   With demographics:    p =", round(emo_p_raw, 4), "\n")

if(emo_p_raw < 0.05) {
  cat("\n✓✓✓ MAIN HYPOTHESIS SUPPORTED!\n")
  cat("   Emotional connection significantly predicts satisfaction")
  cat("   even after controlling for age and gender.\n")
  
  if(emo_coef_raw > 0) {
    cat("   The relationship is POSITIVE (as emotional connection increases,")
    cat(" satisfaction increases).\n")
  } else {
    cat("   However, the relationship is NEGATIVE (opposite of hypothesis).\n")
  }
  
} else {
  cat("\n✗✗✗ MAIN HYPOTHESIS NOT SUPPORTED\n")
  cat("   Emotional connection does NOT significantly predict satisfaction")
  cat(" after controlling for age and gender.\n")
  cat("   p-value =", round(emo_p_raw, 4), "> 0.05\n")
}

#------------------------------------------------------------------------------
# Step 6: Confidence Intervals
#------------------------------------------------------------------------------

cat("\n--- STEP 6: Confidence Intervals ---\n")
cat("\n95% Confidence Interval for Emotional Connection:\n")
emo_ci <- confint(model_full_raw)["emotional_connection", ]
cat("   [", round(emo_ci[1], 4), ", ", round(emo_ci[2], 4), "]\n")

if(emo_ci[1] < 0 & emo_ci[2] < 0) {
  cat("   Entire interval is NEGATIVE - significant negative relationship\n")
} else if(emo_ci[1] > 0 & emo_ci[2] > 0) {
  cat("   Entire interval is POSITIVE - significant positive relationship\n")
} else if(emo_ci[1] < 0 & emo_ci[2] > 0) {
  cat("   Interval contains ZERO - relationship NOT significant\n")
}

#------------------------------------------------------------------------------
# Step 7: Summary Table for Main Hypothesis
#------------------------------------------------------------------------------

cat("\n--- STEP 7: Summary Table ---\n")

hypothesis_summary <- data.frame(
  Model = c("Simple (no controls)", "Full (with demographics)"),
  Predictor = c("Emotional Connection", "Emotional Connection"),
  Coefficient = c(round(coef(model_simple_raw)[2], 4), round(emo_coef_raw, 4)),
  Std_Coefficient = c(round(coef(model_simple_std)[2], 4), round(emo_coef_std, 4)),
  P_value = c(round(summary(model_simple_raw)$coefficients[2, 4], 4), round(emo_p_raw, 4)),
  Significant = c(
    ifelse(summary(model_simple_raw)$coefficients[2, 4] < 0.05, "YES", "NO"),
    ifelse(emo_p_raw < 0.05, "YES", "NO")
  )
)

print(hypothesis_summary)
  
# Model                      Predictor             Coefficient       Std_Coefficient
#Simple (no controls)       Emotional Connection      0.0169          0.0329
# Full (with demographics)  Emotional Connection      0.0207          0.0403

#P_value   Significant
#0.2033          NO
#0.1203          NO

cat("\n", rep("=", 80), "\n")
cat("END OF MAIN HYPOTHESIS TESTING\n")
cat(rep("=", 80), "\n")

# ============================================================================
# PART B: TESTING MAIN HYPOTHESIS - MULTIPLE METHODS
# Testing: "Audiences who feel emotionally connected report higher satisfaction"
# Using: Means, Variances, and Proportions
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("PART B: TESTING MAIN HYPOTHESIS - MULTIPLE METHODS\n")
cat(rep("=", 80), "\n")

#------------------------------------------------------------------------------
# Step 1: Create High/Low Emotional Connection Groups
#------------------------------------------------------------------------------

# Using median split (simpler for interpretation)
emo_median <- median(movie_1500$emotional_connection)

movie_1500 <- movie_1500 %>%
  mutate(
    emotion_level = case_when(
      emotional_connection >= emo_median ~ "High Emotion",
      TRUE ~ "Low Emotion"
    )
  )

cat("\n--- Emotional Connection Groups (Median Split) ---\n")
table(movie_1500$emotion_level)   #High Emotion  Low Emotion 
                                  #751              749 

#------------------------------------------------------------------------------
# Step 2: METHOD 1 - Compare MEANS (t-test)
# H₁: Mean satisfaction is higher in High Emotion group
#------------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("METHOD 1: COMPARING MEANS (Independent t-test)\n")
cat(rep("-", 60), "\n")

# Extract satisfaction by group
high_emo_sat <- movie_1500 %>% filter(emotion_level == "High Emotion") %>% pull(satisfaction)
low_emo_sat <- movie_1500 %>% filter(emotion_level == "Low Emotion") %>% pull(satisfaction)

# Descriptive stats
cat("\nGroup Statistics:\n")
cat("  High Emotion Group: n =", length(high_emo_sat), 
    ", Mean =", round(mean(high_emo_sat), 3),
    ", SD =", round(sd(high_emo_sat), 3), "\n")
cat("  Low Emotion Group:  n =", length(low_emo_sat), 
    ", Mean =", round(mean(low_emo_sat), 3),
    ", SD =", round(sd(low_emo_sat), 3), "\n")
cat("  Mean Difference:", round(mean(high_emo_sat) - mean(low_emo_sat), 3), "\n")

# Independent t-test (assuming equal variances)
t_test_means <- t.test(high_emo_sat, low_emo_sat, var.equal = TRUE, 
                       alternative = "greater")  # Testing if high > low

cat("\n▶ Independent t-test Results:\n")
cat("  t-statistic =", round(t_test_means$statistic, 3), "\n")
cat("  df =", round(t_test_means$parameter, 0), "\n")
cat("  p-value =", round(t_test_means$p.value, 4), "\n")

if(t_test_means$p.value < 0.05) {
  cat("\n✓ MEANS TEST: High Emotion group has significantly HIGHER satisfaction\n")
  cat("  This SUPPORTS your hypothesis!\n")
} else {
  cat("\n✗ MEANS TEST: No significant difference in satisfaction means\n")
  cat("  This does NOT support your hypothesis\n")
}

# Effect size (Cohen's d)
pooled_sd <- sqrt(((length(high_emo_sat)-1)*var(high_emo_sat) + 
                     (length(low_emo_sat)-1)*var(low_emo_sat)) / 
                    (length(high_emo_sat) + length(low_emo_sat) - 2))
cohens_d <- (mean(high_emo_sat) - mean(low_emo_sat)) / pooled_sd

cat("\nEffect size (Cohen's d) =", round(abs(cohens_d), 3))
cat(" -", ifelse(abs(cohens_d) < 0.2, "Very small",
                 ifelse(abs(cohens_d) < 0.5, "Small",
                        ifelse(abs(cohens_d) < 0.8, "Medium", "Large"))), "effect\n")

#------------------------------------------------------------------------------
# Step 3: METHOD 2 - Compare VARIANCES (F-test)
# H₁: Variance of satisfaction differs between emotion groups
#------------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("METHOD 2: COMPARING VARIANCES (F-test)\n")
cat(rep("-", 60), "\n")

# F-test for variances
var_test <- var.test(high_emo_sat, low_emo_sat)

cat("\nGroup Variances:\n")
cat("  High Emotion Group variance =", round(var(high_emo_sat), 4), "\n")
cat("  Low Emotion Group variance  =", round(var(low_emo_sat), 4), "\n")
cat("  Ratio (High/Low) =", round(var(high_emo_sat)/var(low_emo_sat), 3), "\n")

cat("\n▶ F-test Results:\n")
cat("  F-statistic =", round(var_test$statistic, 3), "\n")
cat("  numerator df =", var_test$parameter[1], "\n")
cat("  denominator df =", var_test$parameter[2], "\n")
cat("  p-value =", round(var_test$p.value, 4), "\n")

if(var_test$p.value < 0.05) {
  cat("\n✓ VARIANCES TEST: Satisfaction variance differs significantly between groups\n")
  if(var(high_emo_sat) > var(low_emo_sat)) {
    cat("  High Emotion group has MORE variable satisfaction\n")
  } else {
    cat("  High Emotion group has MORE CONSISTENT satisfaction\n")
  }
} else {
  cat("\n✗ VARIANCES TEST: No significant difference in variances\n")
  cat("  Both groups have similar consistency in satisfaction\n")
}

#------------------------------------------------------------------------------
# Step 4: METHOD 3 - Compare PROPORTIONS (z-test)
# First, create binary satisfaction variable if not exists
#------------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("METHOD 3: COMPARING PROPORTIONS (z-test)\n")
cat(rep("-", 60), "\n")

# Create binary satisfaction if not already done
if(!"satisfied" %in% names(movie_1500)) {
  sat_median <- median(movie_1500$satisfaction)
  movie_1500 <- movie_1500 %>%
    mutate(
      satisfied = ifelse(satisfaction > sat_median, 1, 0)
    )
}

# Create contingency table
prop_table <- table(movie_1500$emotion_level, movie_1500$satisfied)
colnames(prop_table) <- c("Not Satisfied", "Satisfied")
print(prop_table)

# Calculate proportions
props <- movie_1500 %>%
  group_by(emotion_level) %>%
  summarise(
    n = n(),
    satisfied_count = sum(satisfied),
    proportion = satisfied_count / n
  )

cat("\nProportions Satisfied by Emotion Group:\n")
print(props)

# Two-proportions z-test
prop_test <- prop.test(prop_table, alternative = "greater")  # Testing if high > low

cat("\n▶ Two-Proportions Test Results:\n")
cat("  X-squared =", round(prop_test$statistic, 3), "\n")
cat("  df =", prop_test$parameter, "\n")
cat("  p-value =", round(prop_test$p.value, 4), "\n")

# Calculate difference in proportions
prop_diff <- props$proportion[props$emotion_level == "High Emotion"] - 
  props$proportion[props$emotion_level == "Low Emotion"]
cat("  Difference in proportions =", round(prop_diff, 4), "\n")

if(prop_test$p.value < 0.05) {
  cat("\n✓ PROPORTIONS TEST: High Emotion group has significantly HIGHER satisfaction rate\n")
  cat("  This SUPPORTS your hypothesis!\n")
} else {
  cat("\n✗ PROPORTIONS TEST: No significant difference in satisfaction rates\n")
  cat("  This does NOT support your hypothesis\n")
}

#------------------------------------------------------------------------------
# Step 5: SUMMARY OF ALL METHODS
#------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n")
cat("SUMMARY: TESTING MAIN HYPOTHESIS - ALL METHODS\n")
cat(rep("=", 80), "\n")

summary_multiple <- data.frame(
  Method = c("Regression", "Means (t-test)", "Variances (F-test)", "Proportions (z-test)"),
  What_It_Tests = c("Linear relationship", "Difference in averages", 
                    "Difference in consistency", "Difference in satisfaction rate"),
  P_value = c(0.153, round(t_test_means$p.value, 4), 
              round(var_test$p.value, 4), round(prop_test$p.value, 4)),
  Supports_Hypothesis = c(
    ifelse(0.153 < 0.05, "YES", "NO"),
    ifelse(t_test_means$p.value < 0.05, "YES", "NO"),
    ifelse(var_test$p.value < 0.05, "YES", "NO"),
    ifelse(prop_test$p.value < 0.05, "YES", "NO")
  )
)

print(summary_multiple)

cat("\n", rep("-", 60), "\n")
cat("OVERALL CONCLUSION:\n")

if(all(summary_multiple$P_value > 0.05)) {
  cat("  ALL methods show NO significant relationship.\n")
  cat("  STRONG evidence that emotional connection does NOT predict satisfaction.\n")
} else if(sum(summary_multiple$P_value < 0.05) == 1) {
  cat("  Only ONE method shows significance.\n")
  cat("  Weak/Inconsistent evidence - likely no real relationship.\n")
} else if(sum(summary_multiple$P_value < 0.05) >= 2) {
  cat("  Multiple methods show significance.\n")
  cat("  Evidence suggests emotional connection MAY affect satisfaction.\n")
}

cat("\n", rep("=", 80), "\n")

# Boxplot showing the variance difference
ggplot(movie_1500, aes(x = emotion_level, y = satisfaction, fill = emotion_level)) +
  geom_boxplot() +
  labs(title = "Satisfaction Distribution by Emotional Connection Level",
       subtitle = paste("Variance difference p = 0.0001"),
       x = "Emotional Connection Group", 
       y = "Satisfaction") +
  theme_minimal()


# ============================================================================
# PART C: PROBABILITY DISTRIBUTIONS FOR MAIN HYPOTHESIS (LO1 & LO4)
# Testing: "Audiences who feel emotionally connected report higher satisfaction"
# Under demographic controls (age, gender)
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("PART C: PROBABILITY DISTRIBUTIONS FOR MAIN HYPOTHESIS\n")
cat(rep("=", 80), "\n")

#------------------------------------------------------------------------------
# 1. Test NORMAL Distribution Assumption (Your Current Model)
#------------------------------------------------------------------------------

cat("\n--- 1. NORMAL Distribution Assumption ---\n")

# Your current model assumes Normal distribution
model_normal <- lm(satisfaction ~ emotional_connection + age + gender_male + gender_other, 
                   data = movie_1500)

# Check if residuals are Normal (this tests the assumption)
residuals_normal <- residuals(model_normal)
shapiro_test_normal <- shapiro.test(residuals_normal)

cat("\nNormality of Residuals (should be Normal if assumption holds):\n")
cat("  Shapiro-Wilk W =", round(shapiro_test_normal$statistic, 4), "\n")
cat("  p-value =", round(shapiro_test_normal$p.value, 4), "\n")

if(shapiro_test_normal$p.value > 0.05) {
  cat("✓ Residuals are Normal - Normal distribution assumption is valid\n")
} else {
  cat("✗ Residuals are NOT Normal - Normal assumption may be violated\n")
}

# AIC for Normal model
aic_normal <- AIC(model_normal)
cat("\nNormal Model AIC:", round(aic_normal, 2), "\n")

#------------------------------------------------------------------------------
# 2. Test GAMMA Distribution (for positive, skewed data)
#------------------------------------------------------------------------------

cat("\n--- 2. GAMMA Distribution Assumption ---\n")

# Gamma GLM (log link ensures positive predictions)
model_gamma <- glm(satisfaction ~ emotional_connection + age + gender_male + gender_other, 
                   family = Gamma(link = "log"), data = movie_1500)

# Check deviance residuals (should be approximately Normal)
residuals_gamma <- residuals(model_gamma, type = "deviance")
shapiro_test_gamma <- shapiro.test(residuals_gamma)

cat("\nDeviance Residuals Normality (should be Normal if Gamma fits):\n")
cat("  Shapiro-Wilk W =", round(shapiro_test_gamma$statistic, 4), "\n")
cat("  p-value =", round(shapiro_test_gamma$p.value, 4), "\n")

# AIC for Gamma model
aic_gamma <- AIC(model_gamma)
cat("\nGamma Model AIC:", round(aic_gamma, 2), "\n")

#------------------------------------------------------------------------------
# 3. Test BINOMIAL Distribution (for binary satisfaction)
#------------------------------------------------------------------------------

cat("\n--- 3. BINOMIAL Distribution Assumption ---\n")

# Create binary satisfaction if not exists
if(!"satisfied" %in% names(movie_1500)) {
  sat_median <- median(movie_1500$satisfaction)
  movie_1500$satisfied <- ifelse(movie_1500$satisfaction > sat_median, 1, 0)
}

# Binomial GLM (logistic regression)
model_binomial <- glm(satisfied ~ emotional_connection + age + gender_male + gender_other, 
                      family = binomial, data = movie_1500)

# Check Hosmer-Lemeshow goodness of fit
if(!require(ResourceSelection)) install.packages("ResourceSelection")
library(ResourceSelection)

hl_test <- hoslem.test(model_binomial$y, fitted(model_binomial), g = 10)

cat("\nHosmer-Lemeshow Goodness-of-Fit:\n")
cat("  X-squared =", round(hl_test$statistic, 4), "\n")
cat("  p-value =", round(hl_test$p.value, 4), "\n")

if(hl_test$p.value > 0.05) {
  cat("✓ Binomial model fits well\n")
} else {
  cat("✗ Binomial model may not fit well\n")
}

# AIC for Binomial model
aic_binomial <- AIC(model_binomial)
cat("\nBinomial Model AIC:", round(aic_binomial, 2), "\n")

#------------------------------------------------------------------------------
# 4. Compare All Distributions
#------------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("DISTRIBUTION COMPARISON FOR MAIN HYPOTHESIS\n")
cat(rep("-", 60), "\n")

comparison <- data.frame(
  Distribution = c("Normal", "Gamma", "Binomial"),
  AIC = c(aic_normal, aic_gamma, aic_binomial),
  Assumption_Check = c(
    ifelse(shapiro_test_normal$p.value > 0.05, "Pass", "Fail"),
    ifelse(shapiro_test_gamma$p.value > 0.05, "Pass", "Fail"),
    ifelse(hl_test$p.value > 0.05, "Pass", "Fail")
  )
)

print(comparison)

cat("\nBest fitting distribution (lowest AIC):", 
    comparison$Distribution[which.min(comparison$AIC)], "\n")

#------------------------------------------------------------------------------
# 5. Does the Relationship Hold Under Different Distributions?
#------------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("MAIN HYPOTHESIS UNDER DIFFERENT DISTRIBUTIONS\n")
cat(rep("-", 60), "\n")

# Extract emotional_connection coefficient and p-value from each model
results_dist <- data.frame(
  Distribution = c("Normal", "Gamma", "Binomial"),
  Emotional_Connection_Effect = c(
    paste0(round(coef(model_normal)["emotional_connection"], 4), 
           ifelse(summary(model_normal)$coefficients["emotional_connection", 4] < 0.05, "*", "")),
    paste0(round(coef(model_gamma)["emotional_connection"], 4), 
           ifelse(summary(model_gamma)$coefficients["emotional_connection", 4] < 0.05, "*", "")),
    paste0(round(coef(model_binomial)["emotional_connection"], 4), 
           ifelse(summary(model_binomial)$coefficients["emotional_connection", 4] < 0.05, "*", ""))
  ),
  P_value = c(
    round(summary(model_normal)$coefficients["emotional_connection", 4], 4),
    round(summary(model_gamma)$coefficients["emotional_connection", 4], 4),
    round(summary(model_binomial)$coefficients["emotional_connection", 4], 4)
  ),
  Significant = c(
    summary(model_normal)$coefficients["emotional_connection", 4] < 0.05,
    summary(model_gamma)$coefficients["emotional_connection", 4] < 0.05,
    summary(model_binomial)$coefficients["emotional_connection", 4] < 0.05
  )
)

print(results_dist)

cat("\nCONCLUSION:\n")
if(all(!results_dist$Significant)) {
  cat("  Under ALL distribution assumptions, emotional connection is NOT significant.\n")
  cat("  This STRENGTHENS your finding - it's not just due to distribution choice.\n")
} else if(any(results_dist$Significant)) {
  cat("  Emotional connection is significant ONLY under", 
      results_dist$Distribution[results_dist$Significant], "distribution.\n")
  cat("  This suggests the relationship depends on distributional assumptions.\n")
}




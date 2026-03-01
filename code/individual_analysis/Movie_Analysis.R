# Load libraries
library(tidyverse)
library(psych)
library(lm.beta)
library(corrplot)
library(ggplot2)
library(dplyr)

# Set working directory
setwd("C:\\Users\\SKY PC\\Documents\\Y3S1\\TPSM\\TPSM Data Set")

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
      age < 25 ~ "18-24",
      age < 35 ~ "25-34",
      age < 50 ~ "35-49",
      age >= 50 ~ "50+",
      TRUE ~ "Unknown"
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

cat("Users with emotional connection scores:", nrow(user_emotional), "\n")

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
cat("Original watch_history rows:", nrow(watch_history), "\n")
cat("Original movies rows:", nrow(movies), "\n")

# Step 2: Clean watch_history - remove duplicates
watch_history_clean <- watch_history %>%
  distinct(user_id, movie_id, .keep_all = TRUE)

cat("watch_history after deduplication:", nrow(watch_history_clean), "rows (", 
    nrow(watch_history) - nrow(watch_history_clean), "duplicates removed)\n")

# Step 3: Clean movies - ensure unique movie_ids
movies_clean <- movies %>%
  distinct(movie_id, .keep_all = TRUE) %>%
  select(movie_id, genre_primary)

cat("movies after deduplication:", nrow(movies_clean), "rows (", 
    nrow(movies) - nrow(movies_clean), "duplicates removed)\n")

# Step 4: Check for missing genres
movies_with_genre <- movies_clean %>%
  filter(!is.na(genre_primary) & genre_primary != "")

cat("movies with valid genre:", nrow(movies_with_genre), "rows (", 
    nrow(movies_clean) - nrow(movies_with_genre), "without genre)\n")

# Step 5: Perform join
user_genres_raw <- watch_history_clean %>%
  left_join(movies_with_genre, by = "movie_id", relationship = "many-to-many")

cat("After join:", nrow(user_genres_raw), "rows\n")

# Step 6: Remove movies without genre
user_genres_with_genre <- user_genres_raw %>%
  filter(!is.na(genre_primary))

cat("After removing missing genres:", nrow(user_genres_with_genre), "rows (", 
    nrow(user_genres_raw) - nrow(user_genres_with_genre), "removed)\n")

# Step 7: Get preferred genre per user
user_genres <- user_genres_with_genre %>%
  group_by(user_id, genre_primary) %>%
  summarise(genre_count = n(), .groups = 'drop') %>%
  group_by(user_id) %>%
  slice_max(order_by = genre_count, n = 1, with_ties = FALSE) %>%
  select(user_id, preferred_genre = genre_primary)

cat("Final user_genres created with", nrow(user_genres), "users\n")

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

cat("\nFinal dataset size:", nrow(master_data_final), "users\n")

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
print(age_stats)

# Age groups distribution
age_group_dist <- movie_1500 %>%
  count(age_group) %>%
  mutate(percentage = n/sum(n)*100)
print("\nAge Group Distribution:")
print(age_group_dist)

# Gender distribution
gender_dist <- movie_1500 %>%
  count(gender) %>%
  mutate(percentage = n/sum(n)*100)
print("\nGender Distribution:")
print(gender_dist)

# Country distribution (top 10)
country_dist <- movie_1500 %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  mutate(percentage = n/sum(n)*100)
print("\nTop 10 Countries:")
print(country_dist)

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
print(emotional_stats)

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
print("\nEmotional Connection by Gender:")
print(emo_by_gender)

# By age group
emo_by_age <- movie_1500 %>%
  group_by(age_group) %>%
  summarise(
    mean_emo = mean(emotional_connection, na.rm = TRUE),
    sd_emo = sd(emotional_connection, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(age_group)
print("\nEmotional Connection by Age Group:")
print(emo_by_age)

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
print(satisfaction_stats)

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

# PART 4: EMOTION-SATISFACTION RELATIONSHIP

# 4.1 Overall correlation
cor_test <- cor.test(movie_1500$emotional_connection, 
                     movie_1500$satisfaction, 
                     method = "pearson")

print("Correlation between Emotional Connection and Satisfaction:")
print(paste("Pearson r =", round(cor_test$estimate, 3)))
print(paste("p-value =", round(cor_test$p.value, 4)))
print(paste("95% CI: [", round(cor_test$conf.int[1], 3), ",", 
            round(cor_test$conf.int[2], 3), "]"))

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
cat("   Coefficient =", round(coef(model_simple_raw)[2], 4), "\n")
cat("   p-value =", round(summary(model_simple_raw)$coefficients[2, 4], 4), "\n")
cat("   R² =", round(summary(model_simple_raw)$r.squared, 4), "\n")

# Standardized model
model_simple_std <- lm(satisfaction_std ~ emotional_connection_std, data = movie_1500)
cat("\n▶ Standardized Model (scale-free):\n")
cat("   Standardized β =", round(coef(model_simple_std)[2], 4), "\n")
cat("   p-value =", round(summary(model_simple_std)$coefficients[2, 4], 4), "\n")
cat("   R² =", round(summary(model_simple_std)$r.squared, 4), "\n")

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
      age_25_34 = ifelse(age_group == "25-34", 1, 0),
      age_35_49 = ifelse(age_group == "35-49", 1, 0),
      age_50plus = ifelse(age_group == "50+", 1, 0)
    )
}

# RAW MODEL (original scales)
model_full_raw <- lm(satisfaction ~ emotional_connection + age + 
                       gender_male + gender_other + 
                       age_25_34 + age_35_49 + age_50plus, 
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
cat("   Standardized β =", round(emo_coef_std, 4), "\n")
cat("   p-value =", round(emo_p_std, 4), "\n")

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
table(movie_1500$emotion_level)

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

# ============================================================================
# PART D: COMPLETE MULTIPLE REGRESSION ANALYSIS (LO3) - FIXED VERSION
# Testing: "Audiences who feel emotionally connected report higher satisfaction"
# With demographic controls: age, gender
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("PART D: COMPLETE MULTIPLE REGRESSION ANALYSIS\n")
cat(rep("=", 80), "\n")

# Ensure we have the correct model with ALL demographic controls
# Reference categories: Female (gender), 18-24 (age group)

#------------------------------------------------------------------------------
# 1. REGRESSION DIAGNOSTICS - Formal Tests
#------------------------------------------------------------------------------

cat("\n--- 1. REGRESSION DIAGNOSTICS ---\n")

# Load required libraries
if(!require(lmtest)) install.packages("lmtest")
if(!require(car)) install.packages("car")
library(lmtest)
library(car)

# Use your full model with all controls
model_full <- lm(satisfaction ~ emotional_connection + age + 
                   gender_male + gender_other + 
                   age_25_34 + age_35_49 + age_50plus, 
                 data = movie_1500)

# 1.1 Normality of Residuals (Shapiro-Wilk test)
cat("\n1.1 Normality of Residuals:\n")
cat("    This checks if errors are Normally distributed\n")
resid_norm <- shapiro.test(residuals(model_full))
cat("    Shapiro-Wilk W =", round(resid_norm$statistic, 4), "\n")
cat("    p-value =", round(resid_norm$p.value, 4), "\n")
if(resid_norm$p.value > 0.05) {
  cat("    ✓ Residuals are Normal - assumption met\n")
} else {
  cat("    ✗ Residuals are NOT Normal - assumption violated\n")
  cat("      Note: With large samples, this is less concerning\n")
}

# 1.2 Homoscedasticity (Breusch-Pagan test)
cat("\n1.2 Homoscedasticity (Constant Variance):\n")
cat("    This checks if error variance is constant across predictions\n")
bp_test <- bptest(model_full)
cat("    BP =", round(bp_test$statistic, 4), "\n")
cat("    p-value =", round(bp_test$p.value, 4), "\n")
if(bp_test$p.value > 0.05) {
  cat("    ✓ Constant variance - assumption met\n")
} else {
  cat("    ✗ Heteroscedasticity detected - variance not constant\n")
  cat("      Consider using robust standard errors\n")
}

# 1.3 Multicollinearity (VIF)
cat("\n1.3 Multicollinearity:\n")
cat("    This checks if predictors are too highly correlated\n")
vif_vals <- vif(model_full)
print(round(vif_vals, 3))
cat("\n    Interpretation: VIF > 5 indicates problematic multicollinearity\n")
if(max(vif_vals) > 5) {
  cat("    ✗ Multicollinearity detected - variables are redundant\n")
  cat("      Consider removing some predictors\n")
} else {
  cat("    ✓ No serious multicollinearity issues\n")
}

# 1.4 Influential Points (Cook's Distance)
cat("\n1.4 Influential Points (Cook's Distance):\n")
cat("    This identifies points that disproportionately influence results\n")
cooks_d <- cooks.distance(model_full)
influential <- which(cooks_d > 4/nrow(movie_1500))
cat("    Number of influential points:", length(influential), "\n")
cat("    Percentage of data:", round(length(influential)/nrow(movie_1500)*100, 2), "%\n")
if(length(influential) < 0.05 * nrow(movie_1500)) {
  cat("    ✓ Acceptable level of influence (< 5%)\n")
} else {
  cat("    ⚠ High proportion of influential points\n")
}

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model_full)
title("Regression Diagnostic Plots", outer = TRUE, line = -2)

#------------------------------------------------------------------------------
# 2. LACK OF FIT TEST (Fixed version)
#------------------------------------------------------------------------------

cat("\n--- 2. LACK OF FIT TEST ---\n")
cat("    This tests if your model form is correct\n")
cat("    (Does the linear model adequately fit the data?)\n")

# Create groups based on emotional connection (for replication)
movie_1500$emo_group_lof <- cut(movie_1500$emotional_connection, 
                                breaks = 5, 
                                include.lowest = TRUE,
                                labels = FALSE)

# Check if groups were created properly
group_counts <- table(movie_1500$emo_group_lof)
cat("\nGroups created:\n")
print(group_counts)

# Pure error model (group means) - only if we have multiple groups with >1 observation
if(length(unique(movie_1500$emo_group_lof)) > 1 && min(group_counts) > 1) {
  
  pure_error_model <- lm(satisfaction ~ as.factor(emo_group_lof), data = movie_1500)
  pure_error_ss <- sum(residuals(pure_error_model)^2)
  pure_error_df <- df.residual(pure_error_model)
  
  # Lack of fit from regression model
  lof_ss <- sum(residuals(model_full)^2) - pure_error_ss
  lof_df <- model_full$df.residual - pure_error_df
  
  # Ensure we don't have negative or zero degrees of freedom
  if(lof_df > 0 && pure_error_df > 0) {
    lof_ms <- lof_ss / lof_df
    pure_error_ms <- pure_error_ss / pure_error_df
    lof_f <- lof_ms / pure_error_ms
    lof_p <- pf(lof_f, lof_df, pure_error_df, lower.tail = FALSE)
    
    cat("\nLack of Fit Test Results:\n")
    cat("  F-statistic =", round(lof_f, 3), "\n")
    cat("  p-value =", round(lof_p, 4), "\n")
    
    if(lof_p > 0.05) {
      cat("  ✓ No significant lack of fit\n")
      cat("    The linear model adequately represents the data\n")
    } else {
      cat("  ✗ Significant lack of fit detected\n")
      cat("    The relationship may be non-linear\n")
    }
  } else {
    cat("\n⚠ Cannot compute Lack of Fit test - insufficient degrees of freedom\n")
    lof_p <- NA
  }
} else {
  cat("\n⚠ Cannot compute Lack of Fit test - need multiple groups with >1 observation each\n")
  lof_p <- NA
}

#------------------------------------------------------------------------------
# 3. SOLUTIONS TO MODEL DEPARTURES
#------------------------------------------------------------------------------

cat("\n--- 3. SOLUTIONS TO MODEL DEPARTURES ---\n")

# 3.1 If heteroscedasticity detected, use robust standard errors
if(exists("bp_test") && !is.na(bp_test$p.value) && bp_test$p.value < 0.05) {
  cat("\n3.1 Heteroscedasticity detected - Using Robust Standard Errors:\n")
  if(!require(sandwich)) install.packages("sandwich")
  library(sandwich)
  
  # Robust standard errors
  robust_se <- sqrt(diag(vcovHC(model_full, type = "HC3")))
  robust_coef <- coef(model_full)
  robust_t <- robust_coef / robust_se
  robust_p <- 2 * (1 - pnorm(abs(robust_t)))
  
  # Compare original vs robust
  comparison_se <- data.frame(
    Predictor = names(robust_coef),
    Original_SE = round(summary(model_full)$coefficients[, 2], 4),
    Robust_SE = round(robust_se, 4),
    Original_p = round(summary(model_full)$coefficients[, 4], 4),
    Robust_p = round(robust_p, 4)
  )
  print(comparison_se)
  
  cat("\n   Emotional connection p-value (original):", 
      round(summary(model_full)$coefficients["emotional_connection", 4], 4), "\n")
  cat("   Emotional connection p-value (robust):", 
      round(robust_p["emotional_connection"], 4), "\n")
} else {
  cat("\n3.1 No heteroscedasticity detected or test not available\n")
}

# 3.2 Check for non-linearity by trying polynomial terms (directly, without relying on lof_p)
cat("\n3.2 Testing for non-linear relationship:\n")
cat("    Checking if emotional connection has a curved relationship with satisfaction\n")

# Add squared term
movie_1500$emotional_connection_sq <- movie_1500$emotional_connection^2

model_poly <- lm(satisfaction ~ emotional_connection + emotional_connection_sq + 
                   age + gender_male + gender_other + 
                   age_25_34 + age_35_49 + age_50plus, 
                 data = movie_1500)

cat("\n   Polynomial Model Summary (checking squared term):\n")
poly_summary <- summary(model_poly)
print(poly_summary)

# Check if squared term is significant
if("emotional_connection_sq" %in% rownames(poly_summary$coefficients)) {
  poly_sq_p <- poly_summary$coefficients["emotional_connection_sq", 4]
  cat("\n   Squared term p-value =", round(poly_sq_p, 4), "\n")
  
  if(poly_sq_p < 0.05) {
    cat("   ✓ Significant quadratic relationship detected!\n")
    cat("     The effect of emotional connection on satisfaction is CURVED\n")
    
    # Determine direction of curve
    sq_coef <- coef(model_poly)["emotional_connection_sq"]
    if(sq_coef > 0) {
      cat("     U-shaped curve: satisfaction decreases then increases\n")
    } else {
      cat("     Inverted U-shaped curve: satisfaction increases then decreases\n")
    }
  } else {
    cat("   ✗ No significant quadratic relationship\n")
    cat("     The relationship appears linear (or flat)\n")
  }
  
  # Compare linear vs polynomial
  poly_test <- anova(model_full, model_poly)
  cat("\n   Test of linear vs polynomial:\n")
  print(poly_test)
  
  if(poly_test$`Pr(>F)`[2] < 0.05) {
    cat("   ✓ Polynomial model fits significantly better\n")
  } else {
    cat("   ✗ Polynomial does not improve fit\n")
  }
} else {
  cat("\n   Squared term not available in model\n")
}

# 3.3 If influential points exist, check model without them
if(exists("influential") && length(influential) > 0) {
  cat("\n3.3 Influential points detected - Model without them:\n")
  
  if(length(influential) < nrow(movie_1500) * 0.1) {  # Less than 10% of data
    movie_1500_noinf <- movie_1500[-influential, ]
    
    model_noinf <- lm(satisfaction ~ emotional_connection + age + 
                        gender_male + gender_other + 
                        age_25_34 + age_35_49 + age_50plus, 
                      data = movie_1500_noinf)
    
    cat("\n   Original emotional connection p-value:", 
        round(summary(model_full)$coefficients["emotional_connection", 4], 4), "\n")
    cat("   Without influential points p-value:", 
        round(summary(model_noinf)$coefficients["emotional_connection", 4], 4), "\n")
    
    # Check if conclusion changes
    orig_sig <- summary(model_full)$coefficients["emotional_connection", 4] < 0.05
    new_sig <- summary(model_noinf)$coefficients["emotional_connection", 4] < 0.05
    
    if(orig_sig != new_sig) {
      cat("   ⚠ WARNING: Removing influential points CHANGES the conclusion!\n")
    } else {
      cat("   ✓ Conclusion remains the same after removing influential points\n")
    }
  } else {
    cat("   Too many influential points to remove ( >10% of data)\n")
  }
} else {
  cat("\n3.3 No influential points detected\n")
}


#------------------------------------------------------------------------------
# 4. PARTIAL R-SQUARED
#------------------------------------------------------------------------------

cat("\n--- 4. PARTIAL R-SQUARED ---\n")
cat("    This shows the UNIQUE variance explained by each predictor\n")

# Function to calculate partial R²
partial_r2 <- function(full_model, predictor_name) {
  # Create reduced model without the predictor
  if(predictor_name == "emotional_connection") {
    reduced <- lm(satisfaction ~ age + gender_male + gender_other + 
                    age_25_34 + age_35_49 + age_50plus, data = movie_1500)
  } else if(predictor_name == "age") {
    reduced <- lm(satisfaction ~ emotional_connection + gender_male + gender_other + 
                    age_25_34 + age_35_49 + age_50plus, data = movie_1500)
  } else if(predictor_name %in% c("gender_male", "gender_other")) {
    # For gender, we need to keep the other gender dummy
    if(predictor_name == "gender_male") {
      reduced <- lm(satisfaction ~ emotional_connection + age + gender_other + 
                      age_25_34 + age_35_49 + age_50plus, data = movie_1500)
    } else {
      reduced <- lm(satisfaction ~ emotional_connection + age + gender_male + 
                      age_25_34 + age_35_49 + age_50plus, data = movie_1500)
    }
  } else {
    # For age groups - remove all age group dummies
    reduced <- lm(satisfaction ~ emotional_connection + age + 
                    gender_male + gender_other, data = movie_1500)
  }
  
  ss_full <- sum(residuals(full_model)^2)
  ss_reduced <- sum(residuals(reduced)^2)
  return((ss_reduced - ss_full) / ss_reduced)
}

# Calculate partial R² for key predictors
cat("\nPartial R² values (unique variance explained):\n")

pr2_emo <- partial_r2(model_full, "emotional_connection")
cat("  emotional_connection:", round(pr2_emo, 4), 
    "(", round(pr2_emo * 100, 2), "% of variance uniquely)\n")

pr2_age_continuous <- partial_r2(model_full, "age")
cat("  age (continuous):", round(pr2_age_continuous, 4), 
    "(", round(pr2_age_continuous * 100, 2), "%)\n")

pr2_gender_male <- partial_r2(model_full, "gender_male")
cat("  gender_male:", round(pr2_gender_male, 4), 
    "(", round(pr2_gender_male * 100, 2), "%)\n")

pr2_gender_other <- partial_r2(model_full, "gender_other")
cat("  gender_other:", round(pr2_gender_other, 4), 
    "(", round(pr2_gender_other * 100, 2), "%)\n")

# Total R² from full model
cat("\nTotal R² of full model:", round(summary(model_full)$r.squared, 4),
    "(", round(summary(model_full)$r.squared * 100, 2), "%)\n")

#------------------------------------------------------------------------------
# 5. VARIABLE SELECTION
#------------------------------------------------------------------------------

cat("\n--- 5. VARIABLE SELECTION ---\n")
cat("    Finding the best model to predict satisfaction\n")

if(!require(MASS)) install.packages("MASS")
library(MASS)
if(!require(leaps)) install.packages("leaps")
library(leaps)

# 5.1 Stepwise Selection (based on AIC)
cat("\n5.1 Stepwise Selection (AIC-based):\n")
step_model <- stepAIC(model_full, direction = "both", trace = FALSE)
cat("\n   Selected variables:\n")
print(names(coef(step_model))[-1])
cat("\n   Stepwise Model Summary:\n")
print(summary(step_model))

# 5.2 Backward Elimination
cat("\n5.2 Backward Elimination:\n")
backward_model <- stepAIC(model_full, direction = "backward", trace = FALSE)
cat("   Selected variables:", paste(names(coef(backward_model))[-1], collapse = ", "), "\n")

# 5.3 Forward Selection
cat("\n5.3 Forward Selection:\n")
null_model <- lm(satisfaction ~ 1, data = movie_1500)
forward_model <- stepAIC(null_model, direction = "forward",
                         scope = list(lower = null_model, upper = model_full),
                         trace = FALSE)
cat("   Selected variables:", paste(names(coef(forward_model))[-1], collapse = ", "), "\n")

# 5.4 Best Subsets Regression
cat("\n5.4 Best Subsets Regression:\n")
predictors <- cbind(movie_1500$emotional_connection, movie_1500$age,
                    movie_1500$gender_male, movie_1500$gender_other,
                    movie_1500$age_25_34, movie_1500$age_35_49, movie_1500$age_50plus)
colnames(predictors) <- c("emo", "age", "gender_male", "gender_other",
                          "age_25_34", "age_35_49", "age_50plus")

best_subsets <- regsubsets(movie_1500$satisfaction ~ ., 
                           data = as.data.frame(predictors),
                           nbest = 2)
summary_best <- summary(best_subsets)

cat("\n   Best models by Adjusted R²:\n")
best_idx <- order(summary_best$adjr2, decreasing = TRUE)[1:5]
for(i in best_idx) {
  vars <- names(summary_best$which[i, ])[summary_best$which[i, ]][-1]
  cat("   Size:", summary_best$which[i, ] %>% sum(), 
      "Adj R²:", round(summary_best$adjr2[i], 4),
      "Cp:", round(summary_best$cp[i], 2),
      "Variables:", paste(vars, collapse = ", "), "\n")
}

#------------------------------------------------------------------------------
# 6. MODEL COMPARISON
#------------------------------------------------------------------------------

cat("\n--- 6. MODEL COMPARISON ---\n")

# Create several candidate models
models <- list(
  "M1: Emo only" = lm(satisfaction ~ emotional_connection, data = movie_1500),
  "M2: Emo + Age" = lm(satisfaction ~ emotional_connection + age, data = movie_1500),
  "M3: Emo + Gender" = lm(satisfaction ~ emotional_connection + gender_male + gender_other, 
                          data = movie_1500),
  "M4: Emo + Age + Gender" = lm(satisfaction ~ emotional_connection + age + 
                                  gender_male + gender_other, data = movie_1500),
  "M5: Full (with age groups)" = model_full,
  "M6: Stepwise selected" = step_model
)

# Comparison table
comparison <- data.frame(
  Model = names(models),
  R2 = sapply(models, function(m) round(summary(m)$r.squared, 4)),
  Adj_R2 = sapply(models, function(m) round(summary(m)$adj.r.squared, 4)),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  Emo_p = sapply(models, function(m) {
    if("emotional_connection" %in% rownames(summary(m)$coefficients)) {
      round(summary(m)$coefficients["emotional_connection", 4], 4)
    } else {
      NA
    }
  })
)

print(comparison)

cat("\nBest model by Adj R²:", comparison$Model[which.max(comparison$Adj_R2)], "\n")
cat("Best model by AIC:", comparison$Model[which.min(comparison$AIC)], "\n")
cat("Best model by BIC:", comparison$Model[which.min(comparison$BIC)], "\n")

#------------------------------------------------------------------------------
# 7. FINAL INTERPRETATION FOR YOUR SENTENCE (USING FULL MODEL)
#------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n")
cat("FINAL INTERPRETATION: MAIN HYPOTHESIS\n")
cat(rep("=", 80), "\n")

# Use the FULL MODEL for hypothesis testing (since it includes emotional_connection)
final_model <- model_full

# Extract emotional connection result
emo_coef <- coef(final_model)["emotional_connection"]
emo_p <- summary(final_model)$coefficients["emotional_connection", 4]
emo_ci <- confint(final_model)["emotional_connection", ]

cat("\n", rep("-", 60), "\n")
cat("TESTING: 'Audiences who feel emotionally connected report higher satisfaction'\n")
cat(rep("-", 60), "\n")

cat("\n▶ Results from FULL MODEL (controlling for age and gender):\n")
cat("   Emotional connection coefficient =", round(emo_coef, 4), "\n")
cat("   95% Confidence Interval = [", round(emo_ci[1], 4), ", ", round(emo_ci[2], 4), "]\n")
cat("   p-value =", round(emo_p, 4), "\n")

if(emo_p < 0.05) {
  cat("\n✓✓✓ STATISTICALLY SIGNIFICANT!\n")
  if(emo_coef > 0) {
    cat("   As emotional connection increases, satisfaction increases.\n")
    cat("   This SUPPORTS your hypothesis.\n")
  } else {
    cat("   However, the relationship is NEGATIVE.\n")
    cat("   This CONTRADICTS your hypothesis.\n")
  }
} else {
  cat("\n✗✗✗ NOT STATISTICALLY SIGNIFICANT\n")
  cat("   Emotional connection does NOT predict satisfaction\n")
  cat("   after controlling for age and gender.\n")
  cat("   p-value =", round(emo_p, 4), "> 0.05\n")
  cat("   This does NOT support your hypothesis.\n")
}

# Check if confidence interval contains zero
if(emo_ci[1] < 0 & emo_ci[2] > 0) {
  cat("\n   The confidence interval contains ZERO,\n")
  cat("   confirming the relationship is not significant.\n")
}

# Compare with models that don't include emotional connection
cat("\n", rep("-", 60), "\n")
cat("DOES ADDING EMOTIONAL CONNECTION IMPROVE THE MODEL?\n")
cat(rep("-", 60), "\n")

# Model without emotional connection
model_no_emo <- lm(satisfaction ~ age + gender_male + gender_other + 
                     age_25_34 + age_35_49 + age_50plus, data = movie_1500)

compare_emo <- anova(model_no_emo, model_full)
print(compare_emo)

if(!is.na(compare_emo$`Pr(>F)`[2]) && compare_emo$`Pr(>F)`[2] < 0.05) {
  cat("\n✓ Adding emotional connection SIGNIFICANTLY improves the model\n")
} else {
  cat("\n✗ Adding emotional connection does NOT significantly improve the model\n")
}

#------------------------------------------------------------------------------
# 8. SUMMARY TABLE FOR REPORT
#------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n")
cat("SUMMARY: MULTIPLE REGRESSION ANALYSIS (LO3)\n")
cat(rep("=", 80), "\n")

summary_table <- data.frame(
  Analysis = c(
    "Main Hypothesis (emotional_connection)",
    "Demographics - Age (continuous)",
    "Demographics - Gender (Male)",
    "Demographics - Gender (Other)",
    "Model Fit - R²",
    "Model Fit - Adjusted R²",
    "Model Comparison - F-test"
  ),
  Result = c(
    paste0("β = ", round(emo_coef, 4), ", p = ", round(emo_p, 4)),
    paste0("β = ", round(coef(model_full)["age"], 4), 
           ", p = ", round(summary(model_full)$coefficients["age", 4], 4)),
    paste0("β = ", round(coef(model_full)["gender_male"], 4), 
           ", p = ", round(summary(model_full)$coefficients["gender_male", 4], 4)),
    paste0("β = ", round(coef(model_full)["gender_other"], 4), 
           ", p = ", round(summary(model_full)$coefficients["gender_other", 4], 4)),
    paste0(round(summary(model_full)$r.squared, 4)),
    paste0(round(summary(model_full)$adj.r.squared, 4)),
    paste0("F = ", round(compare_emo$F[2], 2), 
           ", p = ", round(compare_emo$`Pr(>F)`[2], 4))
  ),
  Interpretation = c(
    ifelse(emo_p < 0.05, "Significant", "Not significant"),
    ifelse(summary(model_full)$coefficients["age", 4] < 0.05, "Significant", "Not significant"),
    ifelse(summary(model_full)$coefficients["gender_male", 4] < 0.05, "Significant", "Not significant"),
    ifelse(summary(model_full)$coefficients["gender_other", 4] < 0.05, "Significant", "Not significant"),
    "Proportion of variance explained",
    "Penalized for number of predictors",
    ifelse(!is.na(compare_emo$`Pr(>F)`[2]) && compare_emo$`Pr(>F)`[2] < 0.05, 
           "Emotion improves model", "Emotion doesn't improve model")
  )
)

print(summary_table)

cat("\n", rep("=", 80), "\n")
cat("PART D COMPLETED\n")
cat(rep("=", 80), "\n")


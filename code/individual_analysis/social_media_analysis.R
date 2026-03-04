# ============================================================================
# SOCIAL MEDIA DATASET ANALYSIS
# TPSM Assignment 2026
# ============================================================================
# Activity: Social Media
# Dataset: Social Media Sponsership and Engagement Dataset  
# Collection period: 2023 May - 2025 May
# Total records: 52,215 social media posts
# Platforms: Instagram, TikTok, YouTube, Bilibili, RedNote
# ============================================================================

# ============================================================================
# 1. LIBRARY LOADING
# ============================================================================

library(tidyverse)      # For data manipulation
library(lubridate)      # For date handling
library(stringr)        # For text cleaning
library(janitor)        # For cleaning column names
library(dplyr)          # For data manipulation
library(e1071)          # For calculating skewness
library(ggplot2)        # For visualizations
library(corrplot)       # For correlation plots
library(tidyr)          # For breaking comments into words
library(tidytext)       # For text analysis
library(gridExtra)      # For arranging multiple plots
library(car)            # For regression diagnostics
library(effectsize)     # For effect size calculations
library(caret)          # For machine learning utilities

# ============================================================================
# 2. DATASET LOADING
# ============================================================================

setwd("G:\\TPSM_Assignment_2026\\data\\raw\\Social media")
getwd()

# Load raw dataset
data <- read.csv("social_media_dataset.csv")
View(data)

cat("Initial number of rows: ", nrow(data), "\n")  # 52214

# Create a copy for cleaning
data_clean <- data 

# ============================================================================
# 3. DATA CLEANING
# ============================================================================
# 3.1 Clean column names (snake_case consistent format)
data_clean <- clean_names(data_clean)
str(data_clean)

# 3.2 Remove unwanted columns
data_clean <- subset(data_clean, select = -c(
  is_sponsored, sponsor_name, sponsor_category, 
  disclosure_location, content_url
))

# 3.3 Replace missing texts with NA
data_clean$hashtags[data_clean$hashtags %in% c("", "N/A", "NULL", "Unknown", "?")] <- NA
data_clean$comments_text[data_clean$comments_text %in% c("", "N/A", "NULL", "Unknown", "?")] <- NA
data_clean$audience_gender_distribution[data_clean$audience_gender_distribution %in% c("unknown", "non-binary")] <- NA

# 3.4 Remove rows with missing texts
data_clean <- na.omit(data_clean)
cat("Number of rows after cleaning: ", nrow(data_clean), "\n")  # 28879

# 3.5 Fix data types
data_clean <- data_clean %>% 
  mutate(post_date = as.Date(post_date, "%m/%d/%y"))

data_clean$audience_age_distribution <- as.factor(data_clean$audience_age_distribution)
data_clean$audience_gender_distribution <- as.factor(data_clean$audience_gender_distribution)
data_clean$audience_location <- as.factor(data_clean$audience_location)
data_clean$platform <- as.factor(data_clean$platform)

# 3.6 Clean comments text
# Remove punctuation and convert to lowercase
data_clean$comments_text <- tolower(data_clean$comments_text)

# Remove punctuation but keep spaces (replace punctuation with space)
data_clean$comments_text <- str_replace_all(data_clean$comments_text, "[^a-z\\s]", "")

# Remove extra spaces
data_clean$comments_text <- str_squish(data_clean$comments_text)

# Remove posts with no text after cleaning
data_clean <- data_clean %>%
  filter(nchar(comments_text) > 0)

# 3.7 Convert date and extract year
data_clean$post_date <- as.Date(data_clean$post_date, "%m/%d/%y")
data_clean$year <- year(data_clean$post_date)

# ============================================================================
# 4. SENTIMENT ANALYSIS
# ============================================================================

# 4.1 Split comments into individual words
comments_words <- data_clean %>% 
  unnest_tokens(word, comments_text)

# 4.2 Load sentiment dictionary (using Bing sentiment lexicon)
bing <- get_sentiments("bing")
head(bing)

# 4.3 Sentiment score calculation
sentiment_data <- comments_words %>% 
  inner_join(bing, by = "word") %>%
  count(content_id, sentiment) %>% 
  pivot_wider(
    names_from = sentiment,
    values_from = n, 
    values_fill = list(n = 0)
  )

# 4.4 Ensure positive and negative columns exist
if(!"positive" %in% colnames(sentiment_data)) {
  sentiment_data$positive <- 0
}

if(!"negative" %in% colnames(sentiment_data)) {
  sentiment_data$negative <- 0
}

# 4.5 Calculate sentiment score (positive - negative)
colnames(sentiment_data)
sentiment_data$sentiment_score <- sentiment_data$positive - sentiment_data$negative

# 4.6 Merge with main dataset
data_clean <- left_join(data_clean, sentiment_data, by = "content_id")

# 4.7 Replace NA with 0
data_clean$positive[is.na(data_clean$positive)] <- 0
data_clean$negative[is.na(data_clean$negative)] <- 0
data_clean$sentiment_score[is.na(data_clean$sentiment_score)] <- 0

# ============================================================================
# 5. CREATE EMOTIONAL INTENSITY AND SENTIMENT MEASURES
# ============================================================================

data_clean <- data_clean %>%
  mutate(
    # Raw sentiment score
    sentiment_score_raw = sentiment_score,
    
    # Emotional intensity (0-10 scale, higher = more emotional regardless of direction)
    emotional_intensity = abs(sentiment_score),
    
    # Scale emotional intensity to 1-5
    emotional_intensity = 1 + 4 * (emotional_intensity / max(emotional_intensity, na.rm = TRUE)),
    
    # Word count as proxy for engagement
    word_count = str_count(comments_text, "\\S+"),
    
    # Weighted emotional connection
    emotional_connection_weighted = emotional_intensity * log1p(word_count),
    
    # Normalize weighted connection to 1-5 as well
    emotional_connection_weighted = 1 + 4 * (emotional_connection_weighted / 
                                               max(emotional_connection_weighted, na.rm = TRUE)),
    
    # Emotion direction
    emotion_direction = case_when(
      sentiment_score > 0 ~ "Positive",
      sentiment_score < 0 ~ "Negative",
      TRUE ~ "Neutral"
    )
  )

# Check distribution
cat("\nEmotion Direction Distribution:\n")
print(table(data_clean$emotion_direction))
# Negative: 4483, Neutral: 12676, Positive: 11720

print(round(prop.table(table(data_clean$emotion_direction)) * 100, 2))
# Negative: 15.52, Neutral: 43.89, Positive: 40.58

# ============================================================================
# 6. CREATE SATISFACTION VARIABLE
# ============================================================================

cat("\n==================================================\n")
cat("SATISFACTION VARIABLE CREATION\n")
cat("==================================================\n")

data_clean <- data_clean %>%
  mutate(
    # Raw satisfaction (engagement)
    satisfaction_raw = likes + shares + comments_count,
    
    # Normalize satisfaction to 0-10 scale
    satisfaction = (satisfaction_raw - min(satisfaction_raw, na.rm = TRUE)) /
      (max(satisfaction_raw, na.rm = TRUE) - min(satisfaction_raw, na.rm = TRUE)) * 10,
    
    # Binary satisfaction for additional analyses (median split)
    satisfaction_median = median(satisfaction, na.rm = TRUE),
    satisfaction_binary = ifelse(satisfaction > satisfaction_median, 1, 0),
    
    # Log-transformed satisfaction (for regression if needed)
    satisfaction_log = log1p(satisfaction_raw)
  )

cat("\nSatisfaction Statistics:\n")
print(summary(data_clean$satisfaction))
# Min. 1st Qu. Median Mean 3rd Qu. Max. 
# 0.000 4.129 4.972 4.984 5.843 10.000

# ============================================================================
# 7. SAVE CLEANED DATA AND TAKE SAMPLE
# ============================================================================

cat("\n==================================================\n")
cat("SAVE CLEANED DATA AND TAKE SAMPLE\n")
cat("==================================================\n")

# Save cleaned dataset
write.csv(data_clean, "cleaned_social_media_dataset.csv", row.names = FALSE)

# Take simple random sample of 1500
set.seed(123)
social_sample <- data_clean[sample(nrow(data_clean), 1500), ]

write.csv(social_sample, "sample_social_media_dataset.csv", row.names = FALSE)

cat("\nSample size:", nrow(social_sample), "\n")

# Check if sample is representative
cat("\nPopulation vs Sample comparison:\n")
cat("Population mean satisfaction:", round(mean(data_clean$satisfaction), 3), "\n")
# Population mean satisfaction: 4.984
cat("Sample mean satisfaction:", round(mean(social_sample$satisfaction), 3), "\n")
# Sample mean satisfaction: 4.98

cat("Population mean emotional intensity:", round(mean(data_clean$emotional_intensity), 3), "\n")
# Population mean emotional intensity: 1.398
cat("Sample mean emotional intensity:", round(mean(social_sample$emotional_intensity), 3), "\n")
# Sample mean emotional intensity: 1.392

# ============================================================================
# 8. EMOTION TYPE DETECTION (NRC)
# ============================================================================

cat("\n==================================================\n")
cat("EMOTION TYPE DETECTION (NRC)\n")
cat("==================================================\n")

nrc <- get_sentiments("nrc")
emotion_data <- comments_words %>% 
  inner_join(nrc, by = "word") %>% 
  count(sentiment)

print(emotion_data)
# sentiment        n
# anger          12268
# anticipation   31174
# disgust        4395
# fear           16659
# joy            23608
# negative       20651
# positive       76040
# sadness        10794
# surprise       13277
# trust          50481

# ============================================================================
# 9. DESCRIPTIVE ANALYSIS
# ============================================================================

cat("\n==================================================\n")
cat("DESCRIPTIVE ANALYSIS\n")
cat("==================================================\n")

# 9.1 Numeric variables summary
numeric_summary <- data.frame(
  Variable = c("Satisfaction", "Emotional Intensity", "Sentiment Score", "Age"),
  Mean = c(
    mean(social_sample$satisfaction), 
    mean(social_sample$emotional_intensity),
    mean(social_sample$sentiment_score),
    mean(social_sample$age, na.rm = TRUE)
  ),
  SD = c(
    sd(social_sample$satisfaction),
    sd(social_sample$emotional_intensity),
    sd(social_sample$sentiment_score),
    sd(social_sample$age, na.rm = TRUE)
  ),
  Min = c(
    min(social_sample$satisfaction),
    min(social_sample$emotional_intensity),
    min(social_sample$sentiment_score),
    min(social_sample$age, na.rm = TRUE)
  ),
  Max = c(
    max(social_sample$satisfaction),
    max(social_sample$emotional_intensity),
    max(social_sample$sentiment_score),
    max(social_sample$age, na.rm = TRUE)
  )
)
numeric_summary[, 2:5] <- round(numeric_summary[, 2:5], 3)

cat("\n=== NUMERIC VARIABLES SUMMARY ===\n")
print(numeric_summary)
# Variable                Mean    SD    Min   Max
# Satisfaction           4.980 1.249 0.983 9.663
# Emotional Intensity    1.392 0.425 1.000 3.500
# Sentiment Score        0.366 1.097 -3.000 5.000
# Age                     NA    NA    Inf  -Inf

# 9.2 Categorical data analysis

# Gender distribution
gender_freq <- table(social_sample$audience_gender_distribution)
gender_prop <- prop.table(gender_freq) * 100
cat("\nGender Distribution:\n")
print(cbind(Count = gender_freq, Percentage = round(gender_prop, 2)))
#         Count Percentage
# female   762       50.8
# male     738       49.2

# Platform distribution
platform_freq <- table(social_sample$platform)
platform_prop <- prop.table(platform_freq) * 100
cat("\nPlatform Distribution:\n")
print(cbind(Count = platform_freq, Percentage = round(platform_prop, 2)))
#           Count Percentage
# Bilibili   289      19.27
# Instagram  291      19.40
# RedNote    304      20.27
# TikTok     307      20.47
# YouTube    309      20.60

# Age group distribution
age_freq <- table(social_sample$audience_age_distribution)
age_prop <- prop.table(age_freq) * 100
cat("\nAge Group Distribution:\n")
print(cbind(Count = age_freq, Percentage = round(age_prop, 2)))
#       Count Percentage
# 13-18   251      16.73
# 19-25   527      35.13
# 26-35   449      29.93
# 36-50   196      13.07
# 50+      77       5.13

# Emotion direction distribution
emotion_dir_freq <- table(social_sample$emotion_direction)
emotion_dir_prop <- prop.table(emotion_dir_freq) * 100
cat("\nEmotion Direction Distribution:\n")
print(cbind(Count = emotion_dir_freq, Percentage = round(emotion_dir_prop, 2)))
#           Count Percentage
# Negative   265      17.67
# Neutral    648      43.20
# Positive   587      39.13

# 9.3 Satisfaction by Demographics

# By Gender
sat_by_gender <- social_sample %>%
  group_by(audience_gender_distribution) %>%
  summarise(
    n = n(),
    mean_sat = round(mean(satisfaction), 2),
    sd_sat = round(sd(satisfaction), 2),
    median_sat = round(median(satisfaction), 2)
  )
cat("\nSatisfaction by Gender:\n")
print(sat_by_gender)
# audience_gender_distribution n mean_sat sd_sat median_sat
# female                      762     5.02   1.24      5.00
# male                        738     4.94   1.26      4.92

# By Platform
sat_by_platform <- social_sample %>%
  group_by(platform) %>%
  summarise(
    n = n(),
    mean_sat = round(mean(satisfaction), 2),
    sd_sat = round(sd(satisfaction), 2)
  ) %>%
  arrange(desc(mean_sat))
cat("\nSatisfaction by Platform:\n")
print(sat_by_platform)
# platform n mean_sat sd_sat
# YouTube  309     5.04   1.23
# RedNote  304     5.00   1.28
# Bilibili 289     4.98   1.30
# Instagram291     4.97   1.26
# TikTok   307     4.92   1.18

# By Age Group
sat_by_age <- social_sample %>%
  group_by(audience_age_distribution) %>%
  summarise(
    n = n(),
    mean_sat = round(mean(satisfaction), 2),
    sd_sat = round(sd(satisfaction), 2)
  )
cat("\nSatisfaction by Age Group:\n")
print(sat_by_age)
# audience_age_distribution n mean_sat sd_sat
# 13-18                    251     4.89   1.25
# 19-25                    527     5.02   1.23
# 26-35                    449     5.00   1.21
# 36-50                    196     4.89   1.34
# 50+                       77     5.06   1.33

# By Emotion Direction
sat_by_emotion_dir <- social_sample %>%
  group_by(emotion_direction) %>%
  summarise(
    n = n(),
    mean_sat = round(mean(satisfaction), 2),
    sd_sat = round(sd(satisfaction), 2)
  )
cat("\nSatisfaction by Emotion Direction:\n")
print(sat_by_emotion_dir)
# emotion_direction n mean_sat sd_sat
# Negative         265     4.94   1.23
# Neutral          648     4.96   1.23
# Positive         587     5.02   1.28

# 9.4 Emotional Intensity by Demographics

# By Gender
emo_by_gender <- social_sample %>%
  group_by(audience_gender_distribution) %>%
  summarise(
    n = n(),
    mean_emo = round(mean(emotional_intensity), 3),
    sd_emo = round(sd(emotional_intensity), 3)
  )
cat("\nEmotional Intensity by Gender:\n")
print(emo_by_gender)
# audience_gender_distribution n mean_emo sd_emo
# female                      762    1.39  0.406
# male                        738    1.40  0.443

# By Platform
emo_by_platform <- social_sample %>%
  group_by(platform) %>%
  summarise(
    n = n(),
    mean_emo = round(mean(emotional_intensity), 3),
    sd_emo = round(sd(emotional_intensity), 3)
  )
cat("\nEmotional Intensity by Platform:\n")
print(emo_by_platform)
# platform n mean_emo sd_emo
# Bilibili 289    1.35  0.408
# Instagram291    1.40  0.455
# RedNote  304    1.41  0.409
# TikTok   307    1.40  0.428
# YouTube  309    1.40  0.423

# By Age
emo_by_age <- social_sample %>%
  group_by(audience_age_distribution) %>%
  summarise(
    n = n(),
    mean_emo = round(mean(emotional_intensity), 3),
    sd_emo = round(sd(emotional_intensity), 3)
  )
cat("\nEmotional Intensity by Age:\n")
print(emo_by_age)
# audience_age_distribution n mean_emo sd_emo
# 13-18                    251    1.39  0.429
# 19-25                    527    1.40  0.430
# 26-35                    449    1.38  0.425
# 36-50                    196    1.39  0.419
# 50+                       77    1.42  0.392

# ============================================================================
# 10. DESCRIPTIVE PLOTS
# ============================================================================

cat("\n==================================================\n")
cat("CREATING DESCRIPTIVE PLOTS\n")
cat("==================================================\n")

setwd("G:\\TPSM_Assignment_2026\\outputs\\descriptive\\Social media")

# Plot 1: Age Distribution
p1 <- ggplot(social_sample, aes(x = audience_age_distribution, fill = audience_age_distribution)) +
  geom_bar() + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Audience Age Distribution", x = "Age group", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Social_media_Audience_Age_Distribution.png", p1, width = 8, height = 5)

# Plot 2: Gender Distribution
p2 <- ggplot(social_sample, aes(x = audience_gender_distribution, fill = audience_gender_distribution)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Audience Gender Distribution", x = "Gender", y = "Count") + 
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Gender_Distribution.png", p2, width = 8, height = 5)

# Plot 3: Location Distribution
p3 <- ggplot(social_sample, aes(x = audience_location, fill = audience_location)) +
  geom_bar() + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Audience Location Distribution", x = "Location", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Location_Distribution.png", p3, width = 8, height = 5)

# Plot 4: Platform Distribution
p4 <- ggplot(social_sample, aes(x = platform, fill = platform)) +
  geom_bar() +
  labs(title = "Platform Distribution", x = "Platform", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Platform_Distribution.png", p4, width = 8, height = 5)

# Plot 5: Satisfaction Distribution
p5 <- ggplot(social_sample, aes(x = satisfaction)) +
  geom_histogram(bins = 30, fill = "#FF6B6B", color = "black") +
  labs(
    title = "Distribution of Satisfaction Scores",
    subtitle = paste0("Mean = ", round(mean(social_sample$satisfaction), 2),
                      ", SD = ", round(sd(social_sample$satisfaction), 2)),
    x = "Satisfaction (0-10 scale)", 
    y = "Frequency"
  ) +
  theme_minimal()
ggsave("Distribution_of_Satisfaction_Scores.png", p5, width = 8, height = 5)

# Plot 6: Emotional Intensity Distribution
p6 <- ggplot(social_sample, aes(x = emotional_intensity)) +
  geom_histogram(bins = 30, fill = "#4ECDC4", color = "black") +
  labs(
    title = "Distribution of Emotional Intensity",
    subtitle = paste0("Mean = ", round(mean(social_sample$emotional_intensity), 2),
                      ", SD = ", round(sd(social_sample$emotional_intensity), 2)),
    x = "Emotional Intensity (1-5 scale)", 
    y = "Frequency"
  ) +
  theme_minimal()
ggsave("Emotional_Intensity_Distribution.png", p6, width = 8, height = 5)

# Plot 7: Satisfaction by Gender
p7 <- ggplot(social_sample, aes(x = audience_gender_distribution, y = satisfaction, 
                                fill = audience_gender_distribution)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  scale_fill_manual(values = c("Male" = "#A8D5FF", "Female" = "#FFB3BA")) +
  labs(title = "Satisfaction by Gender", x = "Gender", y = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Satisfaction_by_Gender.png", p7, width = 8, height = 5)

# Plot 8: Satisfaction by Platform
p8 <- ggplot(social_sample, aes(x = reorder(platform, satisfaction, FUN = median), 
                                y = satisfaction, fill = platform)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(title = "Satisfaction by Platform", x = "Platform", y = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Satisfaction_by_Platform.png", p8, width = 8, height = 5)

# Plot 9: Scatterplot - Emotional Intensity vs Satisfaction
p9 <- ggplot(social_sample, aes(x = emotional_intensity, y = satisfaction)) +
  geom_point(alpha = 0.3, color = "#45B7D1") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Emotional Intensity vs Satisfaction",
    x = "Emotional Intensity", 
    y = "Satisfaction"
  ) +
  theme_minimal()
ggsave("Emotional_Intensity_vs_Satisfaction.png", p9, width = 8, height = 5)

# Plot 10: Satisfaction by Emotion Direction
p10 <- ggplot(social_sample, aes(x = emotion_direction, y = satisfaction, fill = emotion_direction)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Positive" = "#90EE90", "Negative" = "#FFB6C1", "Neutral" = "#D3D3D3")) +
  labs(title = "Satisfaction by Emotion Direction", x = "Emotion Direction", y = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Satisfaction_by_Emotion_Direction.png", p10, width = 8, height = 5)

# Plot 11: Satisfaction by Age Group
p11 <- ggplot(social_sample, aes(x = reorder(audience_age_distribution, satisfaction, FUN = median), 
                                 y = satisfaction, fill = audience_age_distribution)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(title = "Satisfaction by Age", x = "Age", y = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Satisfaction_by_Age.png", p11, width = 8, height = 5)

# ============================================================================
# 11. EMOTIONAL QUARTILE ANALYSIS
# ============================================================================

cat("\n==================================================\n")
cat("EMOTIONAL QUARTILE ANALYSIS\n")
cat("==================================================\n")

# Create emotional engagement quartiles based on EMOTIONAL INTENSITY
social_sample$emotional_quartile <- ntile(social_sample$emotional_intensity, 4)

# Convert to factor with labels
social_sample$emotional_quartile <- factor(
  social_sample$emotional_quartile,
  levels = 1:4,
  labels = c("Low", "Medium-Low", "Medium-High", "High")
)

# Plot 12: Satisfaction Distribution by Emotional Intensity Quartile
boxplot_emotional <- ggplot(social_sample, aes(x = emotional_quartile, 
                                               y = satisfaction, 
                                               fill = emotional_quartile)) +
  geom_boxplot(alpha = 0.7, outlier.color = "darkgray", outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(
    title = "Satisfaction Distribution by Emotional Intensity Quartile",
    subtitle = "Dots show mean values",
    x = "Emotional Intensity Quartile",
    y = "Satisfaction Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(boxplot_emotional)
ggsave("satisfaction_by_emotion_quartile.png", boxplot_emotional, width = 8, height = 5)

# Summary statistics by quartile
summary_stats <- social_sample %>%
  group_by(emotional_quartile) %>%
  summarise(
    # Sample size
    count = n(),
    percentage = round(n() / nrow(social_sample) * 100, 1),
    
    # Emotional intensity stats
    mean_emotion = round(mean(emotional_intensity, na.rm = TRUE), 3),
    sd_emotion = round(sd(emotional_intensity, na.rm = TRUE), 3),
    
    # Satisfaction stats
    mean_satisfaction = round(mean(satisfaction, na.rm = TRUE), 2),
    median_satisfaction = round(median(satisfaction, na.rm = TRUE), 2),
    sd_satisfaction = round(sd(satisfaction, na.rm = TRUE), 2),
    
    # Engagement metrics
    mean_likes = round(mean(likes, na.rm = TRUE), 0),
    mean_shares = round(mean(shares, na.rm = TRUE), 0),
    mean_comments = round(mean(comments_count, na.rm = TRUE), 0),
    
    # Confidence intervals for satisfaction
    se_satisfaction = round(sd(satisfaction, na.rm = TRUE) / sqrt(n()), 3),
    ci_lower = round(mean(satisfaction, na.rm = TRUE) - 1.96 * 
                       (sd(satisfaction, na.rm = TRUE) / sqrt(n())), 2),
    ci_upper = round(mean(satisfaction, na.rm = TRUE) + 1.96 * 
                       (sd(satisfaction, na.rm = TRUE) / sqrt(n())), 2),
    
    .groups = 'drop'
  )

cat("\nSUMMARY STATISTICS BY EMOTIONAL INTENSITY QUARTILE\n")
print(summary_stats)
# emotional_quartile count percentage mean_emotion sd_emotion mean_satisfaction 
# Low                 375         25        1.000      0.000              5.01
# Medium-Low          375         25        1.140      0.223              4.91
# Medium-High         375         25        1.500      0.000              5.00
# High                375         25        1.930      0.385              5.00

# ============================================================================
# 12. INFERENTIAL ANALYSIS - MAIN HYPOTHESIS TESTING
# ============================================================================
# Hypothesis: "Audiences who feel emotionally connected report higher satisfaction"
# ============================================================================

cat("\n==================================================\n")
cat("INFERENTIAL ANALYSIS - MAIN HYPOTHESIS TESTING\n")
cat("==================================================\n")

sample_data <- social_sample

# 12.1 Variable Overview
cat("\n--- VARIABLE OVERVIEW ---\n")
cat("Sample size:", nrow(sample_data), "\n")
cat("Variables:", paste(names(sample_data), collapse = ", "), "\n\n")

cat("DV: Satisfaction - Mean:", round(mean(sample_data$satisfaction), 3),
    ", SD:", round(sd(sample_data$satisfaction), 3), "\n")
# DV: Satisfaction - Mean: 4.98, SD: 1.249

cat("IV: Emotional Intensity - Mean:", round(mean(sample_data$emotional_intensity), 3),
    ", SD:", round(sd(sample_data$emotional_intensity), 3), "\n")
# IV: Emotional Intensity - Mean: 1.392, SD: 0.425

cat("Age - Mean:", round(mean(sample_data$age, na.rm = TRUE), 1),
    ", SD:", round(sd(sample_data$age, na.rm = TRUE), 1), "\n")

# 12.2 Create high and low emotional engagement groups (median split)
emotional_median <- median(sample_data$emotional_intensity, na.rm = TRUE)
sample_data$emotional_group <- ifelse(sample_data$emotional_intensity > emotional_median, 
                                      "High", "Low")
sample_data$emotional_group <- factor(sample_data$emotional_group, levels = c("Low", "High"))

table(sample_data$emotional_group)
# Low   High 
# 1250   250

# ============================================================================
# 13. PEARSON CORRELATION
# ============================================================================

cat("\n==================================================\n")
cat("PEARSON CORRELATION\n")
cat("==================================================\n")

cor_test <- cor.test(
  sample_data$emotional_intensity, 
  sample_data$satisfaction,
  method = "pearson", 
  conf.level = 0.95
)

cat("\n--- OVERALL CORRELATION ---\n")
cat("  r =", round(cor_test$estimate, 3), "\n")
# r = -0.01

cat("  p =", round(cor_test$p.value, 4), "\n")
# p = 0.6899

cat("  95% CI = [", round(cor_test$conf.int[1], 3), ",", 
    round(cor_test$conf.int[2], 3), "]\n")
# 95% CI = [ -0.04 , 0.061 ]

# Interpret effect size
r_squared <- cor_test$estimate^2
cat("r² =", round(r_squared, 3))
if(r_squared < 0.01) {
  cat(" - Very weak effect\n")
} else if(r_squared < 0.09) {
  cat(" - Weak effect\n")
} else if(r_squared < 0.25) {
  cat(" - Moderate effect\n")
} else {
  cat(" - Strong effect\n")
}
# r² = 0 - Very weak effect

if(cor_test$p.value < 0.05) {
  cat("✓ Significant correlation\n")
} else {
  cat("✗ Not significant\n")
}
# ✗ Not significant

# ============================================================================
# 14. DEMOGRAPHIC MODERATION ANALYSIS
# ============================================================================

cat("\n==================================================\n")
cat("DEMOGRAPHIC MODERATION ANALYSIS\n")
cat("==================================================\n")

# 14.1 By Gender
cor_by_gender <- sample_data %>%
  group_by(audience_gender_distribution) %>%
  summarise(
    n = n(),
    r = cor(emotional_intensity, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_intensity, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY GENDER ---\n")
print(cor_by_gender)
# audience_gender_distribution n       r      p
# female                      762  0.0402 0.268
# male                        738 -0.0170 0.646

# 14.2 By Age Group
cor_by_age <- sample_data %>%
  group_by(audience_age_distribution) %>%
  filter(n() >= 30) %>%
  summarise(
    n = n(),
    r = cor(emotional_intensity, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_intensity, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY AGE GROUP ---\n")
print(cor_by_age)
# audience_age_distribution n       r      p
# 13-18                    251 -0.0466 0.462
# 19-25                    527  0.0169 0.699
# 26-35                    449  0.0156 0.742
# 36-50                    196  0.0935 0.192
# 50+                       77 -0.0105 0.362

# 14.3 By Platform
cor_by_platform <- sample_data %>%
  group_by(platform) %>%
  filter(n() >= 30) %>%
  summarise(
    n = n(),
    r = cor(emotional_intensity, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_intensity, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY PLATFORM ---\n")
print(cor_by_platform)
# platform n       r      p
# Bilibili 289  0.0577 0.328
# Instagram291  0.0305 0.604
# RedNote  304 -0.0330 0.567
# TikTok   307  0.0150 0.794
# YouTube  309 -0.0186 0.745

# 14.4 By Location (Top 5)
top_locations <- names(sort(table(sample_data$audience_location), 
                            decreasing = TRUE))[1:5]
cor_by_location <- sample_data %>%
  filter(audience_location %in% top_locations) %>%
  group_by(audience_location) %>%
  summarise(
    n = n(),
    r = cor(emotional_intensity, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_intensity, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY TOP LOCATIONS ---\n")
print(cor_by_location)
# audience_location n       r      p
# China             203 -0.0525 0.457
# Germany           190  0.0744 0.308
# India             190  0.0103 0.887
# Japan             198  0.1930 0.788
# USA               202 -0.0283 0.690

# ============================================================================
# 15. METHOD 1 - COMPARE MEANS (T-TEST)
# ============================================================================

cat("\n==================================================\n")
cat("METHOD 1: COMPARE MEANS (Independent t-test)\n")
cat("==================================================\n")

# Check if variances are equal
var_test <- leveneTest(satisfaction ~ emotional_group, data = sample_data)

# Run t-test
t_test <- t.test(satisfaction ~ emotional_group, 
                 data = sample_data,
                 var.equal = var_test$`Pr(>F)`[1] > 0.05)

# Calculate effect size (Cohen's d)
cohen_d <- cohens_d(satisfaction ~ emotional_group, 
                    data = subset(sample_data, emotional_group %in% c("Low", "High")))

# Group statistics
group_stats <- sample_data %>%
  group_by(emotional_group) %>%  
  summarise(
    n = n(),
    mean_sat = mean(satisfaction, na.rm = TRUE),  
    sd_sat = sd(satisfaction, na.rm = TRUE)       
  )

cat("\nGroup Statistics:\n")
print(group_stats)
# emotional_group n mean_sat sd_sat
# Low            1250     4.97   1.25
# High            250     5.02   1.26
cat("  Mean Difference:", round(mean(high_emo_sat) - mean(low_emo_sat), 3), "\n")

cat("\n▶ Independent t-test Results:\n")
cat("  t =", round(t_test$statistic, 3), 
    ", df =", round(t_test$parameter, 1),
    ", p =", round(t_test$p.value, 4), "\n")
# t = -0.57, df = 1498, p = 0.5687

cat("  Cohen's d =", round(cohen_d$Cohens_d, 3), "\n")
# Cohen's d = -0.039

# Cohen's d interpretation
if(abs(cohen_d$Cohens_d) < 0.2) {
  cat("  Effect size: Very small\n")
} else if(abs(cohen_d$Cohens_d) < 0.5) {
  cat("  Effect size: Small\n")
} else if(abs(cohen_d$Cohens_d) < 0.8) {
  cat("  Effect size: Medium\n")
} else {
  cat("  Effect size: Large\n")
}
# Effect size: Very small

if(t_test$p.value < 0.05) {
  cat("\n✓ MEANS TEST: High Emotion group has significantly HIGHER satisfaction\n")
  cat("  This SUPPORTS your hypothesis!\n")
} else {
  cat("\n✗ MEANS TEST: No significant difference in satisfaction means\n")
  cat("  This does NOT support your hypothesis\n")
}
# ✗ MEANS TEST: No significant difference in satisfaction means

# ============================================================================
# 16. METHOD 2 - COMPARE VARIANCES (F-TEST)
# ============================================================================

cat("\n==================================================\n")
cat("METHOD 2: COMPARE VARIANCES (F-test)\n")
cat("==================================================\n")

# Extract satisfaction by group
high_emo_sat <- sample_data %>% filter(emotional_group == "High") %>% pull(satisfaction)
low_emo_sat <- sample_data %>% filter(emotional_group == "Low") %>% pull(satisfaction)

# F-test for variances
var_test_f <- var.test(high_emo_sat, low_emo_sat)

cat("\nGroup Variances:\n")
cat("  High Emotion Group variance =", round(var(high_emo_sat), 4), "\n")
cat("  Low Emotion Group variance  =", round(var(low_emo_sat), 4), "\n")
cat("  Ratio (High/Low) =", round(var(high_emo_sat) / var(low_emo_sat), 3), "\n")

cat("\n▶ F-test Results:\n")
cat("  F-statistic =", round(var_test_f$statistic, 3), "\n")
cat("  numerator df =", var_test_f$parameter[1], "\n")
cat("  denominator df =", var_test_f$parameter[2], "\n")
cat("  p-value =", round(var_test_f$p.value, 4), "\n")

if(var_test_f$p.value < 0.05) {
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
# ✗ VARIANCES TEST: No significant difference in variances

# ============================================================================
# 17. METHOD 3 - COMPARE PROPORTIONS (Z-TEST)
# ============================================================================

cat("\n==================================================\n")
cat("METHOD 3: COMPARE PROPORTIONS (z-test)\n")
cat("==================================================\n")

# Create binary satisfaction if not already done
if(!"satisfied" %in% names(sample_data)) {
  sat_median <- median(sample_data$satisfaction, na.rm = TRUE)
  sample_data <- sample_data %>%
    mutate(satisfied = ifelse(satisfaction > sat_median, 1, 0))
}

# Create contingency table
prop_table <- table(sample_data$emotional_group, sample_data$satisfied)
colnames(prop_table) <- c("Not Satisfied", "Satisfied")
rownames(prop_table) <- c("Low Emotion", "High Emotion")

cat("\nContingency Table:\n")
print(prop_table)

# Calculate proportions
props <- sample_data %>%
  group_by(emotional_group) %>%
  summarise(
    n = n(),
    satisfied_count = sum(satisfied),
    proportion = satisfied_count / n,
    pct = round(proportion * 100, 1)
  )

cat("\nProportions Satisfied by Emotion Group:\n")
print(props)

# Two-proportions z-test (chi-square equivalent)
prop_test <- prop.test(prop_table, alternative = "greater")  # Testing if high > low

cat("\n▶ Two-Proportions Test Results:\n")
cat("  X-squared =", round(prop_test$statistic, 3), "\n")
cat("  df =", prop_test$parameter, "\n")
cat("  p-value =", round(prop_test$p.value, 4), "\n")

# Calculate difference in proportions
prop_diff <- props$proportion[props$emotional_group == "High"] - 
  props$proportion[props$emotional_group == "Low"]
cat("  Difference in proportions =", round(prop_diff, 4), 
    "(", round(prop_diff * 100, 1), "%)\n")

if(prop_test$p.value < 0.05) {
  cat("\n✓ PROPORTIONS TEST: High Emotion group has significantly HIGHER satisfaction rate\n")
  cat("  This SUPPORTS your hypothesis!\n")
} else {
  cat("\n✗ PROPORTIONS TEST: No significant difference in satisfaction rates\n")
  cat("  This does NOT support your hypothesis\n")
}
# ✗ PROPORTIONS TEST: No significant difference in satisfaction rates

# ============================================================================
# 18. METHOD 4 - SIMPLE LINEAR REGRESSION
# ============================================================================

cat("\n==================================================\n")
cat("METHOD 4: SIMPLE LINEAR REGRESSION\n")
cat("==================================================\n")

model_simple <- lm(satisfaction ~ emotional_intensity, data = sample_data)
summary_simple <- summary(model_simple)

cat("\n▶ Simple Linear Regression:\n")
cat("  R² =", round(summary_simple$r.squared, 4), "\n")
# R² = 1e-04

cat("  Adjusted R² =", round(summary_simple$adj.r.squared, 4), "\n")
# Adjusted R² = -6e-04

cat("  F =", round(summary_simple$fstatistic[1], 3),
    ", p =", round(pf(summary_simple$fstatistic[1],
                      summary_simple$fstatistic[2],
                      summary_simple$fstatistic[3],
                      lower.tail = FALSE), 4), "\n\n")
# F = 0.159, p = 0.6899

cat("  Emotional Intensity coefficient =", round(coef(model_simple)[2], 4), "\n")
# Emotional Intensity coefficient = 0.0303

cat("  t-value =", round(summary_simple$coefficients[2, 3], 3), "\n")
# t-value = 0.399

cat("  p-value =", round(summary_simple$coefficients[2, 4], 4), "\n")
# p-value = 0.6899

conf_int_simple <- confint(model_simple)["emotional_intensity", ]
cat("  95% CI = [", round(conf_int_simple[1], 4), ", ", 
    round(conf_int_simple[2], 4), "]\n")
# 95% CI = [ -0.1187, 0.1793 ]

if(summary_simple$coefficients[2, 4] < 0.05) {
  cat("\n✓ REGRESSION: Emotional intensity significantly predicts satisfaction\n")
} else {
  cat("\n✗ REGRESSION: Emotional intensity does NOT significantly predict satisfaction\n")
}
# ✗ REGRESSION: Emotional intensity does NOT significantly predict satisfaction

# ============================================================================
# 19. METHOD 5 - MULTIPLE LINEAR REGRESSION (with demographics)
# ============================================================================

cat("\n==================================================\n")
cat("METHOD 5: MULTIPLE LINEAR REGRESSION (with demographics)\n")
cat("==================================================\n")

# Create dummy variables
sample_data <- sample_data %>%
  mutate(
    gender_male = ifelse(audience_gender_distribution == "male", 1, 0),
    
    platform_tiktok = ifelse(platform == "TikTok", 1, 0),
    platform_instagram = ifelse(platform == "Instagram", 1, 0),
    platform_youtube = ifelse(platform == "YouTube", 1, 0),
    platform_bilibili = ifelse(platform == "Bilibili", 1, 0),
    platform_rednote = ifelse(platform == "RedNote", 1, 0),
    
    # AGE GROUP DUMMIES (Reference = "19-25" - most common group)
    age_13_18 = ifelse(audience_age_distribution == "13-18", 1, 0),
    age_26_35 = ifelse(audience_age_distribution == "26-35", 1, 0),
    age_36_50 = ifelse(audience_age_distribution == "36-50", 1, 0),
    age_50plus = ifelse(audience_age_distribution == "50+", 1, 0)
  )

# Multiple regression model
model_multiple <- lm(satisfaction ~ emotional_intensity + age_13_18 + age_26_35 + 
                       age_36_50 + age_50plus + gender_male + platform_tiktok + 
                       platform_instagram + platform_youtube + platform_bilibili,
                     data = sample_data)
summary_multiple <- summary(model_multiple)

cat("\n▶ Multiple Linear Regression Results:\n")
cat("  R² =", round(summary_multiple$r.squared, 4), "\n")
# R² = 0.0033

cat("  Adjusted R² =", round(summary_multiple$adj.r.squared, 4), "\n")
# Adjusted R² = -0.0027

cat("  F =", round(summary_multiple$fstatistic[1], 3),
    ", p =", round(pf(summary_multiple$fstatistic[1],
                      summary_multiple$fstatistic[2],
                      summary_multiple$fstatistic[3],
                      lower.tail = FALSE), 4), "\n\n")
# F = 0.549, p = 0.8389

cat("\nCoefficients:\n")
print(round(coef(summary_multiple), 4))
#                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)          5.0006     0.1360 36.7672   0.0000
# emotional_intensity  0.0293     0.0761  0.3846   0.7006
# age_13_18           -0.1357     0.0960 -1.4133   0.1578
# age_26_35           -0.0220     0.0804 -0.2741   0.7841
# age_36_50           -0.1280     0.1047 -1.2233   0.2214
# age_50plus           0.0277     0.1528  0.1810   0.8564
# gender_male         -0.0788     0.0651 -1.2109   0.2262
# platform_tiktok     -0.0789     0.1012 -0.7792   0.4360
# platform_instagram  -0.0230     0.1028 -0.2234   0.8232
# platform_youtube     0.0394     0.1011  0.3898   0.6968
# platform_bilibili   -0.0214     0.1029 -0.2076   0.8356

# Extract emotional intensity coefficient (with controls)
emo_coef_multiple <- coef(summary_multiple)["emotional_intensity", "Estimate"]
emo_p_multiple <- coef(summary_multiple)["emotional_intensity", "Pr(>|t|)"]
conf_int_multiple <- confint(model_multiple)["emotional_intensity", ]

cat("\nEmotional intensity (with controls) coefficient =", round(emo_coef_multiple, 4), "\n")
# Emotional intensity (with controls) coefficient = 0.0293

cat("95% CI [", round(conf_int_multiple[1], 4), ", ", 
    round(conf_int_multiple[2], 4), "]\n")
# 95% CI [ -0.1201, 0.1786 ]

cat("p-value =", round(emo_p_multiple, 4), "\n")
# p-value = 0.7006

if(emo_p_multiple < 0.05) {
  cat("✓ Emotional connection STILL significant after controls\n")
} else {
  cat("✗ Emotional connection NOT significant after controls\n")
}
# ✗ Emotional connection NOT significant after controls

# ============================================================================
# 20. METHOD 6 - COMPARE QUARTILES (ANOVA)
# ============================================================================

cat("\n==================================================\n")
cat("METHOD 6: COMPARE QUARTILES (ANOVA)\n")
cat("==================================================\n")

# Satisfaction by quartile
quartile_stats <- sample_data %>%
  group_by(emotional_quartile) %>%
  summarise(
    n = n(),
    mean_satisfaction = round(mean(satisfaction), 2),
    sd_satisfaction = round(sd(satisfaction), 2),
    se = round(sd(satisfaction) / sqrt(n()), 3)
  )

cat("\nSatisfaction by Emotional Intensity Quartile:\n")
print(quartile_stats)
# emotional_quartile n mean_satisfaction sd_satisfaction    se
# Low               375              5.01            1.19 0.061
# Medium-Low        375              4.91            1.30 0.067
# Medium-High       375              5.00            1.25 0.065
# High              375              5.00            1.25 0.065

# One-way ANOVA
anova_quartile <- aov(satisfaction ~ emotional_quartile, data = sample_data)
anova_summary <- summary(anova_quartile)

cat("\n▶ ANOVA Results:\n")
print(anova_summary)

if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("\n✓ ANOVA: Satisfaction differs significantly across quartiles\n")
  
  # Post-hoc tests (Tukey HSD)
  tukey <- TukeyHSD(anova_quartile)
  cat("\nPost-hoc Tests (Tukey HSD):\n")
  print(tukey$emotional_quartile)
} else {
  cat("\n✗ ANOVA: No significant difference across quartiles\n")
}
# ✗ ANOVA: No significant difference across quartiles

# Effect size
eta_sq <- eta_squared(anova_quartile)
cat("\nEffect size (η²) =", round(eta_sq$Eta2, 4))
if(eta_sq$Eta2 < 0.01) {
  cat(" (Very small)\n")
} else if(eta_sq$Eta2 < 0.06) {
  cat(" (Small)\n")
} else if(eta_sq$Eta2 < 0.14) {
  cat(" (Medium)\n")
} else {
  cat(" (Large)\n")
}
# Effect size (η²) = 0.0014 (Very small)

# ============================================================================
# 21. MODEL COMPARISON
# ============================================================================

cat("\n==================================================\n")
cat("MODEL COMPARISON\n")
cat("==================================================\n")

model_comparison <- anova(model_simple, model_multiple)
print(model_comparison)
# Res.Df    RSS Df Sum of Sq     F Pr(>F)
# 1498 2336.7                         
# 1490 2329.2  8     7.4801 0.5981 0.7801

if(model_comparison$`Pr(>F)`[2] < 0.05) {
  cat("\n✓ Adding demographics SIGNIFICANTLY improves the model\n")
} else {
  cat("\n✗ Adding demographics does NOT significantly improve the model\n")
}
# ✗ Adding demographics does NOT significantly improve the model

# ============================================================================
# 22. REGRESSION DIAGNOSTICS
# ============================================================================

cat("\n==================================================\n")
cat("REGRESSION DIAGNOSTICS\n")
cat("==================================================\n")

# VIF - Multicollinearity
vif_values <- vif(model_multiple)
cat("\nVariance Inflation Factor (VIF) - should be < 5:\n")
print(round(vif_values, 2))

if(max(vif_values) > 5) {
  cat("⚠ High multicollinearity detected\n")
} else {
  cat("✓ No serious multicollinearity issues\n")
}
# ✓ No serious multicollinearity issues

# Normality of residuals
residuals_test <- shapiro.test(residuals(model_multiple)[1:500])  # Subset
cat("\nNormality of residuals (Shapiro-Wilk on subset): p =", 
    round(residuals_test$p.value, 4), "\n")
if(residuals_test$p.value > 0.05) {
  cat("✓ Residuals approximately normal\n")
} else {
  cat("⚠ Residuals deviate from normality (common with large samples)\n")
}
# ⚠ Residuals deviate from normality (common with large samples)

# ============================================================================
# 23. FINAL CONCLUSION
# ============================================================================

cat("\n==================================================\n")
cat("FINAL CONCLUSION - HYPOTHESIS TESTING SUMMARY\n")
cat("==================================================\n")
cat("H₀: No relationship between emotional connection and satisfaction\n")
cat("H₁: Positive relationship exists\n\n")

# Summary table
results_table <- data.frame(
  Method = c("Pearson Correlation", "Means (t-test)", "Variances (F-test)", 
             "Proportions (z-test)", "Simple Regression", "Multiple Regression", 
             "ANOVA (Quartiles)"),
  Statistic = c(
    paste("r =", round(cor_test$estimate, 3)),
    paste("t =", round(t_test$statistic, 3)),
    paste("F =", round(var_test_f$statistic, 3)),
    paste("X² =", round(prop_test$statistic, 3)),
    paste("F =", round(summary_simple$fstatistic[1], 3)),
    paste("F =", round(summary_multiple$fstatistic[1], 3)),
    paste("F =", round(anova_summary[[1]]$`F value`[1], 3))
  ),
  P_value = c(
    round(cor_test$p.value, 4),
    round(t_test$p.value, 4),
    round(var_test_f$p.value, 4),
    round(prop_test$p.value, 4),
    round(pf(summary_simple$fstatistic[1],
             summary_simple$fstatistic[2],
             summary_simple$fstatistic[3],
             lower.tail = FALSE), 4),
    round(pf(summary_multiple$fstatistic[1],
             summary_multiple$fstatistic[2],
             summary_multiple$fstatistic[3],
             lower.tail = FALSE), 4),
    round(anova_summary[[1]]$`Pr(>F)`[1], 4)
  ),
  Effect_Size = c(
    paste("r² =", round(r_squared, 3)),
    paste("d =", round(cohen_d$Cohens_d, 3)),
    paste("Ratio =", round(var_test_f$estimate, 3)),
    paste("Diff =", round(prop_diff, 3)),
    paste("R² =", round(summary_simple$r.squared, 3)),
    paste("R² =", round(summary_multiple$r.squared, 3)),
    paste("η² =", round(eta_sq$Eta2, 4))
  ),
  Significant = c(
    cor_test$p.value < 0.05,
    t_test$p.value < 0.05,
    var_test_f$p.value < 0.05,
    prop_test$p.value < 0.05,
    summary_simple$coefficients[2, 4] < 0.05,
    emo_p_multiple < 0.05,
    anova_summary[[1]]$`Pr(>F)`[1] < 0.05
  )
)

print(results_table)

cat("\n", rep("-", 60), "\n")
cat("OVERALL CONCLUSION:\n")

sig_count <- sum(results_table$Significant)

if(sig_count == 0) {
  cat("  ALL methods show NO significant relationship.\n")
  cat("  STRONG evidence that emotional connection does NOT predict satisfaction.\n")
} else if(sig_count <= 2) {
  cat("  Only", sig_count, "out of 7 methods show significance.\n")
  cat("  Weak/Inconsistent evidence - likely no real relationship.\n")
} else if(sig_count <= 4) {
  cat("  Some methods (", sig_count, "/7) show significance.\n")
  cat("  Mixed evidence - relationship may be weak or context-dependent.\n")
} else {
  cat("  Multiple methods (", sig_count, "/7) show significance.\n")
  cat("  Evidence suggests emotional connection MAY affect satisfaction.\n")
}
# Only 0 out of 7 methods show significance.
# Weak/Inconsistent evidence - likely no real relationship.

# ============================================================================
# 24. FINAL BOXPLOT VISUALIZATION
# ============================================================================

# Create emotion level variable for visualization
sample_data$emotion_level <- ifelse(sample_data$emotional_intensity > median(sample_data$emotional_intensity),
                                    "High Emotion", "Low Emotion")

# Boxplot showing the difference
p_box <- ggplot(sample_data, aes(x = emotion_level, y = satisfaction, fill = emotion_level)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Low Emotion" = "#F44336", "High Emotion" = "#4CAF50")) +
  labs(
    title = "Satisfaction Distribution by Emotional Connection Level",
    subtitle = paste("t-test p =", round(t_test$p.value, 4),
                     ", ANOVA p =", round(anova_summary[[1]]$`Pr(>F)`[1], 4)),
    x = "Emotional Connection Group", 
    y = "Satisfaction Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

setwd("G:\\TPSM_Assignment_2026\\outputs\\inferential\\Social media")
ggsave("Social_Inferential_Boxplot.png", p_box, width = 8, height = 5)
print(p_box)

# Save summary table
write.csv(results_table, "Social_Inferential_Summary.csv", row.names = FALSE)

cat("\n==================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==================================================\n")

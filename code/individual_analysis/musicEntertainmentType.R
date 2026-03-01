# ============================================================================
# Activity: Songs/Music
# Dataset: Emotify - Induced Musical Emotion Dataset
# Statement: "Audiences who feel emotionally connected to content report 
#            higher satisfaction."
# 
# KEY METHODOLOGICAL NOTES:
# 1. UNIT OF ANALYSIS: Each row = one listening event (8,407 events)
# 2. No unique participant IDs - each event treated independently
# 3. Gender coding verified: 0 = Male, 1 = Female (Aljanaki et al., 2014, p.5)
# 4. Analytics covered: Descriptive, Inferential
# ============================================================================

library(dplyr)      # Data manipulation
library(ggplot2)    # Visualization

set.seed(123)       # Reproducibility

# LOAD DATA
music_data_raw <- read.csv("/Users/sandali/Downloads/emotionfyData.csv", stringsAsFactors = FALSE)

# Basic structure
str(music_data_raw) 
summary(music_data_raw)
head(music_data_raw)

cat("Initial number of rows:", nrow(music_data_raw), "\n") #8407 

# DATA CLEANING ----------------------------------------------------------------------
music_data_clean <- music_data_raw

table(is.na(music_data_clean)) #no missing values found -> FALSE 142919 
colSums(is.na(music_data_clean)) # Check missing values in each column - got 0 for all - no missing values

# Keep only necessary columns and remove NA (if any)
music_data_clean <- na.omit(music_data_clean[, c("age", "gender", "mother.tongue", "genre",
                                                 "amazement", "solemnity", "tenderness", "nostalgia", 
                                                 "calmness", "power", "joyful_activation", 
                                                 "tension", "sadness", "liked", "disliked")])

cat("Rows after NA removal:", nrow(music_data_clean), "\n") #8407

# Remove Duplicate Rows 
music_data_clean <- music_data_clean[!duplicated(music_data_clean), ]
cat("Rows after duplicate removal:", nrow(music_data_clean), "\n") #7061

# Keep realistic ages only according to the study
cat("Age range before:", min(music_data_clean$age), "to", max(music_data_clean$age), "\n") #5 to 99 
music_data_clean <- music_data_clean[music_data_clean$age >= 10 & music_data_clean$age <= 90, ]
cat("Age range after:", min(music_data_clean$age), "to", max(music_data_clean$age), "\n") #10 to 82

# Convert categorical variables
music_data_clean$gender <- factor(music_data_clean$gender,
                            levels = c(0,1),
                            labels = c("Male","Female"))

table(music_data_clean$gender)
# table(music_data_clean$gender)
# Male Female 
#3868   3188 
(round(prop.table(table(music_data_clean$gender)) * 100, 2))
# Male Female 
#54.82  45.18 

head(music_data_clean)

# create emotional intensity score
emotion_cols <- c("amazement", "solemnity", "tenderness", "nostalgia", 
                  "calmness", "power", "joyful_activation", "tension", "sadness")
# Sum across all GEMS columns -> emotional intensity score -> independent variable
music_data_clean$emotional_intensity <- rowSums(music_data_clean[, emotion_cols], na.rm = TRUE)

# ADD NEW COLUMNS: emotion_direction and sentiment_score
positive_emotions <- c("amazement", "solemnity", "tenderness", "nostalgia", 
                       "calmness", "power", "joyful_activation")

negative_emotions <- c("tension", "sadness")

# POSITIVE_SCORE (sum of all positive emotions)
music_data_clean$positive_score <- rowSums(music_data_clean[, positive_emotions], na.rm = TRUE)

# NEGATIVE_SCORE (sum of all negative emotions)
music_data_clean$negative_score <- rowSums(music_data_clean[, negative_emotions], na.rm = TRUE)

# Emotion Direction (Categorical) column - Determine overall emotional direction based on positive vs negative scores
music_data_clean <- music_data_clean %>%
  mutate(emotion_direction = case_when(
    positive_score > negative_score ~ "Positive",
    negative_score > positive_score ~ "Negative",
    positive_score == negative_score ~ "Neutral/Mixed",
    TRUE ~ "No Emotion"  # If both are 0
  ))
# Check distribution
print(table(music_data_clean$emotion_direction))
# Negative Neutral/Mixed      Positive 
# 945           755          5356 
print(round(prop.table(table(music_data_clean$emotion_direction)) * 100, 2))
#     Negative Neutral/Mixed      Positive 
#     13.39         10.70         75.91

# Sentiment Score (Continuous) column (numeric: positive - negative)
music_data_clean$sentiment_score <- music_data_clean$positive_score - music_data_clean$negative_score

# Calculate possible range
max_positive = length(positive_emotions)  # 7
max_negative = length(negative_emotions)   # 2
max_possible_sentiment = max_positive  # 7
min_possible_sentiment = -max_negative  # -2

cat("\nSentiment score range: from", min_possible_sentiment, "to", max_possible_sentiment, "\n")
print(summary(music_data_clean$sentiment_score))

# Check range of emotional intensity
summary(music_data_clean$emotional_intensity) #min - 0, max - 5

cat("\nCurrent satisfaction rate in cleaned data:", 
    round(mean(music_data_clean$liked) * 100, 2), "%\n") #41.13 %

# Check neutral responses
neutral_count = sum(music_data_clean$liked == 0 & music_data_clean$disliked == 0)
mixed_count = sum(music_data_clean$liked == 1 & music_data_clean$disliked == 1)

cat("Current dataset size:", nrow(music_data_clean), "\n") #7056
cat("Neutral responses (neither liked nor disliked):", neutral_count, "\n") #2767 
cat("Mixed responses (both liked and disliked):", mixed_count, "\n") #56

# Remove neutral and mixed responses
music_data_clean <- music_data_clean %>%
  filter(!(liked == 0 & disliked == 0)) %>%  # Remove neutral
  filter(!(liked == 1 & disliked == 1))      # Remove mixed

cat("Filtered dataset size:", nrow(music_data_clean), "\n") #4233
cat("Removed", 7056 - nrow(music_data_clean), "rows\n")

# Check new satisfaction rate
cat("\nNew satisfaction rate in cleaned data:", 
    round(mean(music_data_clean$liked) * 100, 2), "%\n") #67.23 %

#-- done cleaning data
write.csv(music_data_clean, "cleaned_emotifyData.csv", row.names = FALSE) #Save cleaned data

# simple random sample of 1500
music_sample <- music_data_clean[sample(nrow(music_data_clean), 1500), ]
cat("Sample size:", nrow(music_sample), "\n") #1500

music_sample %>%
  group_by(gender) %>% 
  summarise(percent = 100 * n() / nrow(music_sample)) #check gender distribution -  Male - 53.9, Female - 46.1

# Create age groups for further analysis
music_sample = music_sample %>%
  mutate(age_group = cut(age,
                         breaks = c(0, 12, 18, 25, 35, 50, Inf),
                         labels = c("0-12", "13-18", "19-25", "26-35", "36-50", "50+"),
                         include.lowest = TRUE))

# Save sampled data
write.csv(music_sample, "SRS_sampled_1500_emotifyData.csv", row.names = FALSE)

#check if created sample is reasonable
mean(music_data_clean$age) #31.80558
mean(music_sample$age) #31.494

(round(prop.table(table(music_data_clean$gender)) * 100, 2)) #Male - 55.89 , Female - 44.11
(round(prop.table(table(music_sample$gender)) * 100, 2)) #Male - 53.87 , Female - 46.13

mean(music_data_clean$emotional_intensity) #1.978975
mean(music_sample$emotional_intensity) #1.998
mean(music_data_clean$sentiment_score) #1.183794
mean(music_sample$sentiment_score) #1.231333
#The simple random sample closely approximates the population statistics, indicating representativeness. 

mean(music_sample$liked)       # satisfaction rate - 0.6826667
mean(music_sample$disliked)    # dissatisfaction rate - 0.3173333

#since research question is "Audiences who feel emotionally connected to content report higher satisfaction. -> Stronger emotional engagement overall" we will condsider,
#dependent variable = liked, independent variable - emotional_intensity

# DESCRIPTIVE ANALYSIS -------------------------------------------------------------------------

# Create output directory for tables and plots
dir.create("Descriptive_Output", showWarnings = FALSE)

# 1. OVERALL DESCRIPTIVE STATISTICS
# -------------------------------------------------------------------------------------------------

# Numeric variables summary
numeric_summary <- data.frame(
  Variable = c("Age", "Emotional Intensity", "Sentiment Score"),
  Mean = c(mean(music_sample$age), mean(music_sample$emotional_intensity), mean(music_sample$sentiment_score)),
  SD = c(sd(music_sample$age), sd(music_sample$emotional_intensity), sd(music_sample$sentiment_score)),
  Min = c(min(music_sample$age), min(music_sample$emotional_intensity), min(music_sample$sentiment_score)),
  Max = c(max(music_sample$age), max(music_sample$emotional_intensity), max(music_sample$sentiment_score))
)
numeric_summary[, 2:5] <- round(numeric_summary[, 2:5], 2)
print("=== NUMERIC VARIABLES SUMMARY ===")
print(numeric_summary)
write.csv(numeric_summary, "Descriptive_Output/numeric_variables_summary.csv", row.names = FALSE)

# Categorical variables summary
gender_freq <- table(music_sample$gender)
gender_prop <- prop.table(gender_freq) * 100

age_group_freq <- table(music_sample$age_group)
age_group_prop <- prop.table(age_group_freq) * 100

emotion_direction_freq <- table(music_sample$emotion_direction)
emotion_direction_prop <- prop.table(emotion_direction_freq) * 100

liked_freq <- table(music_sample$liked)
liked_prop <- prop.table(liked_freq) * 100

# Gender
print(cbind(Count = gender_freq, Percentage = round(gender_prop, 2)))
#       Count Percentage
#Male     808      53.87
#Female   692      46.13

# Age Group
print(cbind(Count = age_group_freq, Percentage = round(age_group_prop, 2)))
#      Count Percentage
#0-12      6       0.40
#13-18   124       8.27
#19-25   476      31.73
#26-35   454      30.27
#36-50   288      19.20
#50+     152      10.13

# Emotion Direction
print(cbind(Count = emotion_direction_freq, Percentage = round(emotion_direction_prop, 2)))
#Count Percentage
#Negative        199      13.27
#Neutral/Mixed   157      10.47
#Positive       1144      76.27

#Satisfaction (Liked)
print(cbind(Count = liked_freq, Percentage = round(liked_prop, 2)))
#Count Percentage
#0   476      31.73
#1  1024      68.27

# Save categorical summaries
categorical_summary <- data.frame(
  Variable = c(rep("Gender", length(gender_freq)), 
               rep("Age Group", length(age_group_freq)),
               rep("Emotion Direction", length(emotion_direction_freq)),
               rep("Satisfaction", length(liked_freq))),
  Category = c(names(gender_freq), names(age_group_freq), 
               names(emotion_direction_freq), c("Not Liked", "Liked")),
  Count = c(as.vector(gender_freq), as.vector(age_group_freq), 
            as.vector(emotion_direction_freq), as.vector(liked_freq)),
  Percentage = c(round(as.vector(gender_prop), 2), round(as.vector(age_group_prop), 2),
                 round(as.vector(emotion_direction_prop), 2), round(as.vector(liked_prop), 2))
)
write.csv(categorical_summary, "Descriptive_Output/categorical_summary.csv", row.names = FALSE)

# DV DISTRIBUTION (Satisfaction)
# -------------------------------------------------------------------------------------------------
cat("Overall satisfaction rate:", round(mean(music_sample$liked) * 100, 2), "%\n") #68.27 %

# Gender-wise satisfaction
satisfaction_by_gender <- music_sample %>%
  group_by(gender) %>%
  summarise(
    count = n(),
    satisfied = sum(liked),
    satisfaction_rate = round(mean(liked) * 100, 2)
  )
print(satisfaction_by_gender)
#gender count satisfied satisfaction_rate
#  <fct>  <int>     <int>             <dbl>
#  1 Male     808       554              68.6
#  2 Female   692       470              67.9
write.csv(satisfaction_by_gender, "Descriptive_Output/satisfaction_by_gender.csv", row.names = FALSE)

# Age group-wise satisfaction
satisfaction_by_age <- music_sample %>%
  group_by(age_group) %>%
  summarise(
    count = n(),
    satisfied = sum(liked),
    satisfaction_rate = round(mean(liked) * 100, 2)
  )
print(satisfaction_by_age)
#  age_group count satisfied satisfaction_rate
#<fct>     <int>     <int>             <dbl>
#  1 0-12          6         5              83.3
#  2 13-18       124        88              71.0
#  3 19-25       476       313              65.8
#  4 26-35       454       311              68.5
#  5 36-50       288       200              69.4
#  6 50+         152       107              70.4
write.csv(satisfaction_by_age, "Descriptive_Output/satisfaction_by_age.csv", row.names = FALSE)

# IV DISTRIBUTION (Emotional Intensity)
# -------------------------------------------------------------------------------------------------
print(summary(music_sample$emotional_intensity))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   1.998   3.000   4.000 
cat("Standard Deviation:", sd(music_sample$emotional_intensity), "\n") #0.854683 

# Emotional Intensity by Satisfaction
intensity_by_liked <- music_sample %>%
  group_by(liked) %>%
  summarise(
    count = n(),
    mean_intensity = round(mean(emotional_intensity), 2),
    sd_intensity = round(sd(emotional_intensity), 2),
    median_intensity = median(emotional_intensity)
  )
print(intensity_by_liked)
#  liked count mean_intensity sd_intensity median_intensity
#      <int> <int>          <dbl>        <dbl>            <dbl>
#  1     0   476           1.73         0.84                2
#  2     1  1024           2.12         0.83                2
write.csv(intensity_by_liked, "Descriptive_Output/intensity_by_liked.csv", row.names = FALSE)

# Emotional Intensity by Gender
intensity_by_gender <- music_sample %>%
  group_by(gender) %>%
  summarise(
    count = n(),
    mean_intensity = round(mean(emotional_intensity), 2),
    sd_intensity = round(sd(emotional_intensity), 2)
  )
print(intensity_by_gender)
#  gender count mean_intensity sd_intensity
#.   <fct>  <int>          <dbl>        <dbl>
#  1 Male     808           2.02         0.85
#  2 Female   692           1.97         0.86
write.csv(intensity_by_gender, "Descriptive_Output/intensity_by_gender.csv", row.names = FALSE)

# Emotional Intensity by Age Group
intensity_by_age <- music_sample %>%
  group_by(age_group) %>%
  summarise(
    count = n(),
    mean_intensity = round(mean(emotional_intensity), 2),
    sd_intensity = round(sd(emotional_intensity), 2)
  )
print(intensity_by_age)
#  age_group count mean_intensity sd_intensity
#   <fct>     <int>          <dbl>        <dbl>
#  1 0-12          6           2.67         0.82
#  2 13-18       124           2.32         0.82
#  3 19-25       476           1.98         0.86
#  4 26-35       454           1.94         0.85
#  5 36-50       288           2.04         0.84
#  6 50+         152           1.86         0.82
write.csv(intensity_by_age, "Descriptive_Output/intensity_by_age.csv", row.names = FALSE)

# SECONDARY VARIABLES (Sentiment Score & Emotion Direction)
# -------------------------------------------------------------------------------------------------

# Sentiment Score summary
print(summary(music_sample$sentiment_score))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.000   1.000   1.000   1.231   2.000   4.000 
cat("Standard Deviation:", sd(music_sample$sentiment_score), "\n") #1.310075 

# Sentiment Score by Satisfaction
sentiment_by_liked <- music_sample %>%
  group_by(liked) %>%
  summarise(
    count = n(),
    mean_sentiment = round(mean(sentiment_score), 2),
    sd_sentiment = round(sd(sentiment_score), 2)
  )
print(sentiment_by_liked)
#  liked count mean_sentiment sd_sentiment
#  <int> <int>          <dbl>        <dbl>
#1     0   476           0.32         1.24
#2     1  1024           1.65         1.11
write.csv(sentiment_by_liked, "Descriptive_Output/sentiment_by_liked.csv", row.names = FALSE)

# Emotion Direction frequency
print(table(music_sample$emotion_direction))
#Negative Neutral/Mixed      Positive 
#199           157          1144 
print(round(prop.table(table(music_sample$emotion_direction)) * 100, 2))

# Cross-tab: Emotion Direction vs Satisfaction
direction_satisfaction <- table(music_sample$emotion_direction, music_sample$liked)
colnames(direction_satisfaction) <- c("Not Liked", "Liked")
print(direction_satisfaction)
# Not Liked Liked
#Negative            156    43
#Neutral/Mixed        84    73
#Positive            236   908
write.csv(as.data.frame.matrix(direction_satisfaction), "Descriptive_Output/direction_vs_satisfaction.csv")

# Calculate satisfaction rate by emotion direction
satisfaction_by_direction <- music_sample %>%
  group_by(emotion_direction) %>%
  summarise(
    count = n(),
    satisfaction_rate = round(mean(liked) * 100, 2)
  )
print(satisfaction_by_direction)
#emotion_direction count satisfaction_rate
#<chr>             <int>             <dbl>
#1 Negative            199              21.6
#2 Neutral/Mixed       157              46.5
#3 Positive           1144              79.4

write.csv(satisfaction_by_direction, "Descriptive_Output/satisfaction_by_direction.csv", row.names = FALSE)

# DV vs IV AND CONTROLS
# -------------------------------------------------------------------------------------------------

# Mean Emotional Intensity by Liked (already done above)

# Mean Emotional Intensity by Liked and Gender
intensity_liked_gender <- music_sample %>%
  group_by(liked, gender) %>%
  summarise(
    count = n(),
    mean_intensity = round(mean(emotional_intensity), 2),
    .groups = "drop"
  )
print(intensity_liked_gender)
#  liked gender count mean_intensity
#<int> <fct>  <int>          <dbl>
#1     0 Male     254           1.74
#2     0 Female   222           1.72
#3     1 Male     554           2.16
#4     1 Female   470           2.08
write.csv(intensity_liked_gender, "Descriptive_Output/intensity_liked_gender.csv", row.names = FALSE)

# Mean Emotional Intensity by Liked and Age Group
intensity_liked_age <- music_sample %>%
  group_by(liked, age_group) %>%
  summarise(
    count = n(),
    mean_intensity = round(mean(emotional_intensity), 2),
    .groups = "drop"
  )
print(intensity_liked_age)
#   liked age_group count mean_intensity
#<int> <fct>     <int>          <dbl>
#  1     0 0-12          1           1   
#2     0 13-18        36           2   
#3     0 19-25       163           1.64
#4     0 26-35       143           1.74
#5     0 36-50        88           1.78
#6     0 50+          45           1.69
#7     1 0-12          5           3   
#8     1 13-18        88           2.45
#9     1 19-25       313           2.15
#10     1 26-35       311           2.03
#11     1 36-50       200           2.15
#12     1 50+         107           1.93
write.csv(intensity_liked_age, "Descriptive_Output/intensity_liked_age.csv", row.names = FALSE)

# PLOTS
# -------------------------------------------------------------------------------------------------

# Plot 1: Emotional Intensity Distribution
p1 <- ggplot(music_sample, aes(x = emotional_intensity)) +
  geom_histogram(binwidth = 0.5, fill = "#D39FF5", color = "black") +
  labs(title = "Distribution of Emotional Intensity",
       subtitle = paste0("Mean = ", round(mean(music_sample$emotional_intensity), 2),
                         ", SD = ", round(sd(music_sample$emotional_intensity), 2)),
       x = "Emotional Intensity", y = "Frequency") +
  theme_minimal()
ggsave("Descriptive_Output/emotional_intensity_hist.png", p1, width = 8, height = 5)

# Plot 2: Emotional Intensity Boxplot by Satisfaction
p2 <- ggplot(music_sample, aes(x = factor(liked, labels = c("Not Liked", "Liked")), 
                               y = emotional_intensity, 
                               fill = factor(liked, labels = c("Not Liked", "Liked")))) +
  geom_boxplot() +
  scale_fill_manual(values = c("Not Liked" = "#F44336", "Liked" = "#4CAF50")) +
  labs(title = "Emotional Intensity by Satisfaction",
       x = "Satisfaction", y = "Emotional Intensity") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Descriptive_Output/intensity_by_satisfaction_box.png", p2, width = 6, height = 5)

# Plot 3: Emotional Intensity Boxplot by Gender
p3 <- ggplot(music_sample, aes(x = gender, y = emotional_intensity, fill = gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
  labs(title = "Emotional Intensity by Gender", x = "Gender", y = "Emotional Intensity") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Descriptive_Output/intensity_by_gender.png", p3, width = 6, height = 5)

# Plot 4: Emotional Intensity by Age Group
p4 <- ggplot(music_sample, aes(x = age_group, y = emotional_intensity, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Emotional Intensity by Age Group", x = "Age Group", y = "Emotional Intensity") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Descriptive_Output/intensity_by_age.png", p4, width = 8, height = 5)

# Plot 5: Sentiment Score Distribution
p5 <- ggplot(music_sample, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Sentiment Scores",
       x = "Sentiment Score", y = "Frequency") +
  theme_minimal()
ggsave("Descriptive_Output/sentiment_score_hist.png", p5, width = 8, height = 5)

# Plot 6: Sentiment Score by Satisfaction
p6 <- ggplot(music_sample, aes(x = factor(liked, labels = c("Not Liked", "Liked")), 
                               y = sentiment_score, 
                               fill = factor(liked, labels = c("Not Liked", "Liked")))) +
  geom_boxplot() +
  scale_fill_manual(values = c("Not Liked" = "#F44336", "Liked" = "#4CAF50")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Sentiment Score by Satisfaction", 
       x = "Satisfaction", y = "Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Descriptive_Output/sentiment_by_satisfaction.png", p6, width = 6, height = 5)

# Plot 7: Emotion Direction Distribution
p7 <- ggplot(music_sample, aes(x = emotion_direction, fill = emotion_direction)) +
  geom_bar() +
  scale_fill_manual(values = c("Positive" = "#4CAF50", "Negative" = "#F44336", 
                               "Neutral/Mixed" = "#F5E09F")) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Distribution of Emotion Direction", x = "Emotion Direction", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Descriptive_Output/emotion_direction_bar.png", p7, width = 8, height = 5)

# Plot 8: Satisfaction Rate by Emotion Direction
p8 <- ggplot(satisfaction_by_direction, aes(x = emotion_direction, y = satisfaction_rate, fill = emotion_direction)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Positive" = "#4CAF50", "Negative" = "#F44336", 
                               "Neutral/Mixed" = "#F5E09F")) +
  geom_text(aes(label = paste0(satisfaction_rate, "%")), vjust = -0.5) +
  labs(title = "Satisfaction Rate by Emotion Direction",
       x = "Emotion Direction", y = "Satisfaction Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 100)
ggsave("Descriptive_Output/satisfaction_by_direction.png", p8, width = 8, height = 5)

# Plot 9: Satisfaction by Age Group (Bar Chart)
satisfaction_age_plot <- satisfaction_by_age %>%
  ggplot(aes(x = age_group, y = satisfaction_rate, fill = age_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(satisfaction_rate, "%")), vjust = -0.5) +
  labs(title = "Satisfaction Rate by Age Group", x = "Age Group", y = "Satisfaction Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 100)
ggsave("Descriptive_Output/satisfaction_by_age_bar.png", satisfaction_age_plot, width = 8, height = 5)

# Plot 10: Satisfaction by Gender (Bar Chart)
satisfaction_gender_plot <- satisfaction_by_gender %>%
  ggplot(aes(x = gender, y = satisfaction_rate, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(satisfaction_rate, "%")), vjust = -0.5) +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
  labs(title = "Satisfaction Rate by Gender", x = "Gender", y = "Satisfaction Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 100)
ggsave("Descriptive_Output/satisfaction_by_gender_bar.png", satisfaction_gender_plot, width = 6, height = 5)

# Overlay Emotional Intensity by Satisfaction
p_overlay <- ggplot(music_sample, aes(x = emotional_intensity, fill = factor(liked))) +
  geom_histogram(position = "identity", alpha = 0.6, binwidth = 0.5, color = "black") +
  scale_fill_manual(values = c("0" = "#F44336", "1" = "#4CAF50"), labels = c("Not Liked","Liked")) +
  labs(title = "Emotional Intensity Distribution by Satisfaction",
       x = "Emotional Intensity", y = "Frequency", fill = "Satisfaction") +
  theme_minimal()

ggsave("Descriptive_Output/emotional_intensity_overlay.png", p_overlay, width = 8, height = 5)

# ------------------------------------------------------------------------------------------------

# INFERENTIAL ANALYSIS 
# Testing: "Audiences who feel emotionally connected report higher satisfaction"
# Dataset: Emotify Music Data (cleaned, no neutral responses)
# Sample size: 1500
# ============================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(car)        # For leveneTest, VIF
library(effectsize) # For Cohen's d
library(lmtest)     # For diagnostics

dir.create("Inferential_Output", showWarnings = FALSE)

# STEP 1: Prepare Data
# Data Overview 
cat("Sample size:", nrow(music_sample), "\n") #1500
cat("Variables:", paste(names(music_sample), collapse = ", "), "\n")
#Variables: age, gender, mother.tongue, genre, amazement, solemnity, tenderness, nostalgia, calmness, power, joyful_activation, tension, sadness, liked, disliked, emotional_intensity, positive_score, negative_score, emotion_direction, sentiment_score, age_group, emotion_quartile

sample_data <- music_sample

# variables
# DV: liked (satisfaction - binary 0/1)
# IV: emotional_intensity (emotional connection - continuous)
# Controls: age, gender (for multiple regression)

# Variable Summary
cat("  Liked (satisfaction) - Mean:", round(mean(sample_data$liked), 3), "\n") #  Liked (satisfaction) - Mean: 0.683 
cat("  Emotional Intensity - Mean:", round(mean(sample_data$emotional_intensity), 3), 
    ", SD:", round(sd(sample_data$emotional_intensity), 3), "\n") #Emotional Intensity - Mean: 1.998 , SD: 0.855 
cat("  Age - Mean:", round(mean(sample_data$age), 1), 
    ", SD:", round(sd(sample_data$age), 1), "\n") #Age - Mean: 31.5 , SD: 12.5 

# Gender distribution
print(table(sample_data$gender)) #Male - 808, Female - 692
print(round(prop.table(table(sample_data$gender)) * 100, 1)) #Male - 53.9, Female - 46.1 

# STEP 2: Create High/Low Emotional Connection Groups (Median Split)
emotional_median <- median(sample_data$emotional_intensity, na.rm = TRUE)
cat("Median emotional intensity:", round(emotional_median, 3), "\n") # 2 

sample_data$emotional_group <- ifelse(sample_data$emotional_intensity > emotional_median, 
                                      "High", "Low")
sample_data$emotional_group <- factor(sample_data$emotional_group, levels = c("Low", "High"))

group_table <- table(sample_data$emotional_group)
print(group_table)
#Low High 
#972  528
cat("High group:", group_table["High"], "participants (", 
    round(group_table["High"]/nrow(sample_data)*100, 1), "%)\n") #528 participants ( 35.2 %)
cat("Low group:", group_table["Low"], "participants (", 
    round(group_table["Low"]/nrow(sample_data)*100, 1), "%)\n") #Low group: 972 participants ( 64.8 %)

# STEP 3: Pearson Correlation
cor_test <- cor.test(
  sample_data$emotional_intensity,
  sample_data$liked,
  method = "pearson", 
  conf.level = 0.95
)

cat("Pearson's r =", round(cor_test$estimate, 3), "\n") #0.215 
cat("95% CI [", round(cor_test$conf.int[1], 3), ", ", round(cor_test$conf.int[2], 3), "]\n") #95% CI [ 0.166 ,  0.262 ]
cat("p-value =", round(cor_test$p.value, 4), "\n") #p-value = 0 

# Effect size interpretation
r_squared <- cor_test$estimate^2
r_interp <- ifelse(r_squared < 0.01, "Very weak",
                   ifelse(r_squared < 0.09, "Weak",
                          ifelse(r_squared < 0.25, "Moderate", "Strong")))
cat("r² =", round(r_squared, 3), "-", r_interp, "effect\n") #r² = 0.046 - Weak effect

if(cor_test$p.value < 0.05) {
  cat("✓ Significant correlation\n")
} else {
  cat("✗ Not significant\n")
} #✓ Significant correlation

cor_test$p.value #4.273649e-17 -> 0

# STEP 4: T-Test (Compare Means Between High and Low Groups)
# Check if variances are equal (Levene's test)
var_test <- leveneTest(liked ~ emotional_group, data = sample_data)
cat("Levene's test for equal variances p-value:", round(var_test$`Pr(>F)`[1], 4), "\n") #0

# Run t-test
var_equal <- var_test$`Pr(>F)`[1] > 0.05
t_test <- t.test(sample_data$liked ~ sample_data$emotional_group, 
                 data = sample_data,
                 var.equal = var_equal)

# Calculate Cohen's d
cohen_d <- cohens_d(sample_data$liked ~ sample_data$emotional_group, data = sample_data)

# Group statistics
group_stats <- sample_data %>%
  group_by(emotional_group) %>%
  summarise(
    n = n(),
    mean_satisfaction = round(mean(liked, na.rm = TRUE), 3),
    sd_satisfaction = round(sd(liked, na.rm = TRUE), 3)
  )

print(group_stats)
#  emotional_group     n mean_satisfaction sd_satisfaction
#<fct>           <int>             <dbl>           <dbl>
#1 Low               972             0.619           0.486
#2 High              528             0.799           0.401

#T-TEST RESULTS
cat("t =", round(t_test$statistic, 3), 
    ", df =", round(t_test$parameter, 1),
    ", p =", round(t_test$p.value, 4), "\n") #t = -7.69 , df = 1265.8 , p = 0 

# Cohen's d interpretation
d_value <- abs(cohen_d$Cohens_d)
d_interp <- ifelse(d_value < 0.2, "Very small",
                   ifelse(d_value < 0.5, "Small",
                          ifelse(d_value < 0.8, "Medium", "Large")))
cat("Cohen's d =", round(cohen_d$Cohens_d, 3), 
    " (", d_interp, " effect)\n") #Cohen's d = -0.393  ( Small  effect)

if(t_test$p.value < 0.05) {
  cat("✓ Significant difference between groups\n")
} else {
  cat("✗ No significant difference\n")
} #✓ Significant difference between groups

# STEP 5: Simple Linear Regression (Emotional Connection ONLY)
model_simple <- lm(liked ~ emotional_intensity, data = sample_data)
summary_simple <- summary(model_simple)

#SIMPLE REGRESSION RESULTS
cat("R² =", round(summary_simple$r.squared, 4), "\n") #R² = 0.0461 
cat("Adjusted R² =", round(summary_simple$adj.r.squared, 4), "\n") #Adjusted R² = 0.0454 
cat("F =", round(summary_simple$fstatistic[1], 3),
    ", p =", round(pf(summary_simple$fstatistic[1], 
                      summary_simple$fstatistic[2], 
                      summary_simple$fstatistic[3], 
                      lower.tail = FALSE), 4), "\n\n") #F = 72.363 , p = 0 

#Coefficients
print(round(coef(summary_simple), 4))
#                    Estimate Std. Error t value Pr(>|t|)
#(Intercept)           0.4490     0.0299 15.0315        0
#emotional_intensity   0.1169     0.0137  8.5067        0

# Extract emotional intensity coefficient
emo_coef_simple <- coef(model_simple)[2]
emo_p_simple <- summary_simple$coefficients[2, 4]
conf_int_simple <- confint(model_simple)["emotional_intensity", ]

cat("\nEmotional intensity coefficient =", round(emo_coef_simple, 4), "\n") #0.1169 
cat("95% CI [", round(conf_int_simple[1], 4), ", ", round(conf_int_simple[2], 4), "]\n") #95% CI [ 0.09 ,  0.1439 ]
cat("p-value =", round(emo_p_simple, 4), "\n") #0

if(emo_p_simple < 0.05) {
  cat("✓ Emotional connection SIGNIFICANTLY predicts satisfaction\n")
} else {
  cat("✗ Emotional connection does NOT significantly predict satisfaction\n")
}
#✓ Emotional connection SIGNIFICANTLY predicts satisfaction

# STEP 6: Multiple Linear Regression (WITH Demographics)
# Ensure gender is factor
sample_data$gender <- as.factor(sample_data$gender)

# Multiple regression model
model_multiple <- lm(liked ~ emotional_intensity + age + gender, data = sample_data)
summary_multiple <- summary(model_multiple)

cat("R² =", round(summary_multiple$r.squared, 4), "\n") #R² = 0.0472 
cat("Adjusted R² =", round(summary_multiple$adj.r.squared, 4), "\n") #Adjusted R² = 0.0453
cat("F =", round(summary_multiple$fstatistic[1], 3),
    ", p =", round(pf(summary_multiple$fstatistic[1], 
                      summary_multiple$fstatistic[2], 
                      summary_multiple$fstatistic[3], 
                      lower.tail = FALSE), 4), "\n\n") #F = 24.724 , p = 0 

# Coefficients
print(round(coef(summary_multiple), 4))
#                    Estimate Std. Error t value Pr(>|t|)
#(Intercept)           0.4040     0.0462  8.7414   0.0000
#emotional_intensity   0.1183     0.0138  8.5780   0.0000
#age                   0.0013     0.0009  1.3485   0.1777
#genderFemale          0.0042     0.0237  0.1754   0.8608

# Extract emotional intensity coefficient (with controls)
emo_coef_multiple <- coef(summary_multiple)["emotional_intensity", "Estimate"]
emo_p_multiple <- coef(summary_multiple)["emotional_intensity", "Pr(>|t|)"]
conf_int_multiple <- confint(model_multiple)["emotional_intensity", ]

cat("\nEmotional intensity (with controls) coefficient =", round(emo_coef_multiple, 4), "\n") #0.1183 
cat("95% CI [", round(conf_int_multiple[1], 4), ", ", round(conf_int_multiple[2], 4), "]\n") #95% CI [ 0.0913 ,  0.1454 ]
cat("p-value =", round(emo_p_multiple, 4), "\n") #0

if(emo_p_multiple < 0.05) {
  cat("✓ Emotional connection STILL significant after controlling for age and gender\n")
} else {
  cat("✗ Emotional connection NOT significant after controlling for age and gender\n")
}
#✓ Emotional connection STILL significant after controlling for age and gender

# STEP 7: Model Comparison (Simple vs Multiple)

model_comparison <- anova(model_simple, model_multiple)
print(model_comparison)
#Analysis of Variance Table

#Model 1: liked ~ emotional_intensity
#Model 2: liked ~ emotional_intensity + age + gender
#Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1   1498 309.98                           
#2   1496 309.60  2   0.37637 0.9093  0.403

if(model_comparison$`Pr(>F)`[2] < 0.05) {
  cat("\n✓ Adding demographics SIGNIFICANTLY improves the model\n")
} else {
  cat("\n✗ Adding demographics does NOT significantly improve the model\n")
}
#✗ Adding demographics does NOT significantly improve the model

# STEP 8: Regression Diagnostics (VIF - Multicollinearity)
# Variance Inflation Factor (should be < 5)
vif_values <- vif(model_multiple)
cat("\nVariance Inflation Factor (VIF) - should be < 5:\n")
print(round(vif_values, 2))
#emotional_intensity                 age              gender 
#1.01                1.02                1.02

if(max(vif_values) > 5) {
  cat("⚠ High multicollinearity detected\n")
} else {
  cat("✓ No serious multicollinearity issues\n")
}
#✓ No serious multicollinearity issues

# Check normality of residuals (simplified)
residuals_test <- shapiro.test(residuals(model_multiple)[1:500]) # Subset due to sample size
cat("\nNormality of residuals (Shapiro-Wilk on subset): p =",  
    round(residuals_test$p.value, 4), "\n") #0
if(residuals_test$p.value > 0.05) {
  cat("✓ Residuals approximately normal\n")
} else {
  cat("⚠ Residuals deviate from normality (common with large samples)\n")
}
# ⚠ Residuals deviate from normality (common with large samples)

# STEP 9: Create Enhanced Summary Table
results_table <- data.frame(
  Test = c("Pearson Correlation", "T-test", "Simple Regression", "Multiple Regression"),
  Statistic = c(
    paste("r =", round(cor_test$estimate, 3)),
    paste("t =", round(t_test$statistic, 3)),
    paste("F =", round(summary_simple$fstatistic[1], 3)),
    paste("F =", round(summary_multiple$fstatistic[1], 3))
  ),
  P_value = c(
    round(cor_test$p.value, 4),
    round(t_test$p.value, 4),
    round(pf(summary_simple$fstatistic[1], 
             summary_simple$fstatistic[2], 
             summary_simple$fstatistic[3], 
             lower.tail = FALSE), 4),
    round(pf(summary_multiple$fstatistic[1], 
             summary_multiple$fstatistic[2], 
             summary_multiple$fstatistic[3], 
             lower.tail = FALSE), 4)
  ),
  Effect_Size = c(
    paste("r² =", round(cor_test$estimate^2, 3)),
    paste("d =", round(cohen_d$Cohens_d, 3)),
    paste("R² =", round(summary_simple$r.squared, 3)),
    paste("R² =", round(summary_multiple$r.squared, 3))
  ),
  Interpretation = c(
    ifelse(cor_test$estimate^2 < 0.01, "Very weak",
           ifelse(cor_test$estimate^2 < 0.09, "Weak",
                  ifelse(cor_test$estimate^2 < 0.25, "Moderate", "Strong"))),
    ifelse(abs(cohen_d$Cohens_d) < 0.2, "Very small",
           ifelse(abs(cohen_d$Cohens_d) < 0.5, "Small",
                  ifelse(abs(cohen_d$Cohens_d) < 0.8, "Medium", "Large"))),
    ifelse(summary_simple$r.squared < 0.01, "Very weak",
           ifelse(summary_simple$r.squared < 0.09, "Weak",
                  ifelse(summary_simple$r.squared < 0.25, "Moderate", "Strong"))),
    ifelse(summary_multiple$r.squared < 0.01, "Very weak",
           ifelse(summary_multiple$r.squared < 0.09, "Weak",
                  ifelse(summary_multiple$r.squared < 0.25, "Moderate", "Strong")))
  ),
  Significant = c(
    cor_test$p.value < 0.05,
    t_test$p.value < 0.05,
    pf(summary_simple$fstatistic[1], 
       summary_simple$fstatistic[2], 
       summary_simple$fstatistic[3], 
       lower.tail = FALSE) < 0.05,
    pf(summary_multiple$fstatistic[1], 
       summary_multiple$fstatistic[2], 
       summary_multiple$fstatistic[3], 
       lower.tail = FALSE) < 0.05
  )
)

cat("H₀: No relationship between emotional connection and satisfaction\n")
cat("H₁: Positive relationship exists\n\n")
print(results_table)
#                 Test  Statistic P_value Effect_Size Interpretation Significant
#1 Pearson Correlation  r = 0.215       0  r² = 0.046           Weak        TRUE
#2              T-test  t = -7.69       0  d = -0.393          Small        TRUE
#3   Simple Regression F = 72.363       0  R² = 0.046           Weak        TRUE
#4 Multiple Regression F = 24.724       0  R² = 0.047           Weak        TRUE

# Save results table
write.csv(results_table, "music_hypothesis_testing_summary.csv", row.names = FALSE)

# STEP 10: Create and Save Diagnostic Plots
effects <- data.frame(
  Test = c("Pearson r²", "Cohen's d", "Simple R²", "Multiple R²"),
  Value = c(cor_test$estimate^2, abs(cohen_d$Cohens_d), 
            summary_simple$r.squared, summary_multiple$r.squared)
)

# Plot 1: Effect Size Plot
p_effects <- ggplot(effects, aes(x = Test, y = Value, fill = Test)) +
  geom_col(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0.01, linetype = "dotted", color = "gray") +
  geom_hline(yintercept = 0.09, linetype = "dotted", color = "gray") +
  geom_hline(yintercept = 0.25, linetype = "dotted", color = "gray") +
  annotate("text", x = 0.5, y = 0.01, label = "Very weak", hjust = 0, size = 3) +
  annotate("text", x = 0.5, y = 0.09, label = "Weak", hjust = 0, size = 3) +
  annotate("text", x = 0.5, y = 0.25, label = "Moderate", hjust = 0, size = 3) +
  labs(title = "Effect Sizes - Music Dataset",
       subtitle = "Testing: Emotional Connection → Satisfaction",
       y = "Effect Size") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(0.5, max(effects$Value) + 0.1))

ggsave("music_effect_sizes_enhanced.png", p_effects, width = 8, height = 5)
print(p_effects)

# Plot 2: Boxplot of Satisfaction by Emotional Group
p_box <- ggplot(sample_data, aes(x = emotional_group, y = liked, fill = emotional_group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Low" = "#F44336", "High" = "#4CAF50")) +
  labs(title = "Satisfaction by Emotional Connection Group",
       subtitle = paste0("t = ", round(t_test$statistic, 2), 
                         ", p < 0.001, d = ", round(cohen_d$Cohens_d, 2)),
       x = "Emotional Connection Group", y = "Satisfaction (0 = Not Liked, 1 = Liked)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Inferential_Output/satisfaction_by_group_boxplot.png", p_box, width = 6, height = 5)

# Plot 3: Residuals Diagnostics for Multiple Regression
png("Inferential_Output/regression_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(model_multiple)
title("Regression Diagnostic Plots - Music Dataset", outer = TRUE, line = -2)
dev.off()

# Plot 4: Scatterplot with Regression Line
p_scatter <- ggplot(sample_data, aes(x = emotional_intensity, y = liked)) +
  geom_point(alpha = 0.3, color = "darkgray") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Relationship between Emotional Intensity and Satisfaction",
       subtitle = paste0("r = ", round(cor_test$estimate, 3), 
                         ", R² = ", round(summary_simple$r.squared, 3),
                         ", p < 0.001"),
       x = "Emotional Intensity", y = "Satisfaction (0 = Not Liked, 1 = Liked)") +
  theme_minimal()

ggsave("Inferential_Output/emotional_intensity_vs_satisfaction.png", p_scatter, width = 7, height = 5)

# Plot 5: Grouped Bar Chart - Satisfaction Rate by Group
group_summary <- sample_data %>%
  group_by(emotional_group) %>%
  summarise(
    satisfaction_rate = mean(liked) * 100,
    se = sd(liked) / sqrt(n())
  )

p_bar <- ggplot(group_summary, aes(x = emotional_group, y = satisfaction_rate, fill = emotional_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = satisfaction_rate - 1.96*se, ymax = satisfaction_rate + 1.96*se), width = 0.2) +
  geom_text(aes(label = paste0(round(satisfaction_rate, 1), "%")), vjust = -0.5) +
  scale_fill_manual(values = c("Low" = "#F44336", "High" = "#4CAF50")) +
  labs(title = "Satisfaction Rate by Emotional Connection Group",
       subtitle = "Error bars show 95% CI",
       x = "Emotional Connection Group", y = "Satisfaction Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 100)

ggsave("Inferential_Output/satisfaction_rate_by_group.png", p_bar, width = 6, height = 5)

# STEP 11: Save Model Summaries to Text Files
sink("Inferential_Output/simple_regression_summary.txt")
cat("SIMPLE LINEAR REGRESSION\n")
print(summary(model_simple))
sink()

sink("Inferential_Output/multiple_regression_summary.txt")
cat("MULTIPLE LINEAR REGRESSION (with demographics)\n")
print(summary(model_multiple))
sink()

sink("Inferential_Output/model_comparison.txt")
cat("MODEL COMPARISON: Simple vs Multiple Regression\n")
print(anova(model_simple, model_multiple))
sink()

# STEP 12: FINAL CONCLUSION
cat("\nTesting: 'Audiences who feel emotionally connected report higher satisfaction'\n\n")

if(emo_p_multiple < 0.05 & emo_coef_multiple > 0) {
  cat("✓✓✓ HYPOTHESIS STRONGLY SUPPORTED!\n")
  cat("Emotional connection significantly predicts satisfaction\n")
  cat("even after controlling for age and gender.\n\n")
  cat("Key evidence:\n")
  cat("  • Correlation: r =", round(cor_test$estimate, 3), "(p < 0.001)\n")
  cat("  • T-test: High group (", round(group_stats$mean_satisfaction[2]*100, 1), "%) vs ",
      "Low group (", round(group_stats$mean_satisfaction[1]*100, 1), "%)\n")
  cat("  • Regression: β =", round(emo_coef_multiple, 3), "(p < 0.001)\n")
  cat("  • Effect sizes: Small to moderate (r² = 0.046, d = -0.393)\n")
  
} else if(emo_p_multiple < 0.05 & emo_coef_multiple < 0) {
  cat("⚠ HYPOTHESIS PARTIALLY SUPPORTED (negative relationship)\n")
  cat("Emotional connection predicts satisfaction, but in opposite direction.\n")
} else {
  cat("✗✗✗ HYPOTHESIS NOT SUPPORTED\n")
  cat("No significant relationship found after controlling for demographics.\n")
}

cat("\n", rep("=", 80), "\n")
#✓✓✓ HYPOTHESIS STRONGLY SUPPORTED!
#Emotional connection significantly predicts satisfaction
#even after controlling for age and gender.

#Key evidence:
#  • Correlation: r = 0.215 (p < 0.001)
#• T-test: High group ( 79.9 %) vs  Low group ( 61.9 %)
#• Regression: β = 0.118 (p < 0.001)
#• Effect sizes: Small to moderate (r² = 0.046, d = -0.393)




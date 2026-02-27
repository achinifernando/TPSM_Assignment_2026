# ============================================================================
# COMPLETE ANALYSIS SCRIPT - MUSIC (EMOTIFY DATASET)
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

cat("Initial number of rows:", nrow(music_data_raw), "\n")

# DATA CLEANING ----------------------------------------------------------------------
music_data_clean <- music_data_raw

table(is.na(music_data_clean)) #no missing values found -> FALSE 142919 
colSums(is.na(music_data_clean)) # Check missing values in each column - got 0 for all - no missing values

# Keep only necessary columns and remove NA (if any)
music_data_clean <- na.omit(music_data_clean[, c("age", "gender", "mother.tongue", "genre",
                                                 "amazement", "solemnity", "tenderness", "nostalgia", 
                                                 "calmness", "power", "joyful_activation", 
                                                 "tension", "sadness", "liked", "disliked")])

# Remove missing values (if any)
music_data_clean <- na.omit(music_data_clean)
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

music_data_clean$genre <- as.factor(music_data_clean$genre)
music_data_clean$mother.tongue <- as.factor(music_data_clean$mother.tongue)

head(music_data_clean)

# create emotional intensity score
emotion_cols <- c("amazement", "solemnity", "tenderness", "nostalgia", 
                  "calmness", "power", "joyful_activation", "tension", "sadness")
# Sum across all GEMS columns -> emotional intensity score -> independent variable
music_data_clean$emotional_intensity <- rowSums(music_data_clean[, emotion_cols], na.rm = TRUE)

# Check range of emotional intensity
summary(music_data_clean$emotional_intensity) #min - 0, max - 5

#-- done cleaning data
write.csv(music_data_clean, "cleaned_emotifyData.csv", row.names = FALSE) #Save cleaned data

# simple random sample of 1500
music_sample <- music_data_clean[sample(nrow(music_data_clean), 1500), ]
cat("Sample size:", nrow(music_sample), "\n") #1500

music_sample %>%
  group_by(gender) %>% 
  summarise(percent = 100 * n() / nrow(music_sample)) #check gender distribution -  Male - 54.3, Female - 45.7

# Save sampled data
write.csv(music_sample, "SRS_sampled_1500_emotifyData.csv", row.names = FALSE)

# DESCRIPTIVE ANALYTICS -------------------------------------------------------------------------

# 1. Summary statistics
# Age
summary(music_sample$age) # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                          #11.00   23.00   29.00   32.16   39.00   82.00 
cat("Mean age:", mean(music_sample$age), "\n") #32.15933 
cat("SD age:", sd(music_sample$age), "\n") #12.35804

# Age Histogram
p_age <- ggplot(music_sample, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()
ggsave("hist_age_distribution.png", p_age, width = 7, height = 5)

# Age Boxplot
p_age_box <- ggplot(music_sample, aes(x = "Age", y = age)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Age Boxplot", x = "", y = "Age") +
  theme_minimal()
ggsave("boxplot_age.png", p_age_box, width = 5, height = 5)

# Outlier check for Age - on boxplot i saw some outliers so i wanted to check
age_Q1 = quantile(music_sample$age, 0.25)
age_Q3 = quantile(music_sample$age, 0.75)
age_IQR_value = age_Q3 - age_Q1
age_outlier_condition = music_sample$age < (age_Q1 - 1.5 * age_IQR_value) | music_sample$age > (age_Q3 + 1.5 * age_IQR_value)
cat("Number of age outliers:", sum(age_outlier_condition), "\n") # 26

# Gender distribution
table(music_sample$gender) #Male Female 
                          # 815    685 
prop.table(table(music_sample$gender)) * 100 #Male   Female 
                                            #54.33333 45.66667 

# Bar plot for Gender
p_gender <- ggplot(music_sample, aes(x = gender, fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()
ggsave("barplot_gender.png", p_gender, width = 5, height = 5)

# Mother tongue distribution
table(music_sample$mother.tongue)
prop.table(table(music_sample$mother.tongue)) * 100

head(sort(table(music_sample$mother.tongue), decreasing = TRUE), 10)
#English    Dutch  Russian   German   French  Italian  Spanish  Chinese    Greek Estonian 
#514      310      284       72       38       37       37       23       22       18 

# Bar plot for Mother Tongue
p_mother_tongue <- ggplot(music_sample, aes(x = mother.tongue)) +
  geom_bar(fill = "#7C379E") +
  labs(title = "Mother Tongue Distribution", x = "Mother Tongue", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("barplot_mother_tongue.png", p_mother_tongue, width = 7, height = 5)

# Genre distribution
table(music_sample$genre)
#classical electronic        pop       rock 
#480        341        319        360 
prop.table(table(music_sample$genre)) * 100

# Bar plot for Genre
genre_colors <- c("classical" = "#FF69B4",  
                  "rock"      = "#1E90FF",  
                  "electronic" = "#32CD32", 
                  "pop"        = "#FFA500") 

p_genre <- ggplot(music_sample, aes(x = genre, fill = genre)) +
  geom_bar() +
  scale_fill_manual(values = genre_colors) +
  labs(title = "Genre Distribution", x = "Genre", y = "Count") +
  theme_minimal()
ggsave("barplot_genre.png", p_genre, width = 7, height = 5)


# ----- EMOTIONAL INTENSITY -----
summary(music_sample$emotional_intensity)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   1.999   3.000   5.000 
cat("Mean:", mean(music_sample$emotional_intensity), "\n") #1.999333
cat("SD:", sd(music_sample$emotional_intensity), "\n") #0.840518

# Emotional Intensity Histogram
p_emotion <- ggplot(music_sample,
                    aes(x = emotional_intensity)) +
  geom_histogram(binwidth = 1,
                 fill = "steelblue",
                 color = "black") +
  labs(title = "Emotional Intensity Distribution",
       x = "Emotional Intensity",
       y = "Frequency") +
  theme_minimal()

ggsave("hist_emotional_intensity.png",
       p_emotion, width = 7, height = 5)

# Emotional Intensity Boxplot
p_emotion_box <- ggplot(music_sample,
                        aes(x = "Emotional Intensity",
                            y = emotional_intensity)) +
  geom_boxplot(fill = "#5E6DB5") +
  labs(title = "Emotional Intensity Boxplot",
       x = "",
       y = "Emotional Intensity") +
  theme_minimal()

ggsave("boxplot_emotion.png",
       p_emotion_box, width = 5, height = 5)

# Satisfaction 
cat("Overall satisfaction rate:", mean(music_sample$liked) * 100, "%\n") #41.06667 %

# Emotional Intensity vs Liked (Satisfaction) - Quartile-based Emotional Intensity Analysis 
# Divide sample into 4 quartiles of emotional intensity
music_sample <- music_sample %>%
  mutate(emotion_quartile = ntile(emotional_intensity, 4))  # Q1–Q4

# Summarize satisfaction (liked) by quartile
satisfaction_by_quartile <- music_sample %>%
  group_by(emotion_quartile) %>%
  summarise(
    Satisfaction_Rate = mean(liked) * 100,  # % of liked
    Count = n()
  )

print(satisfaction_by_quartile)

# Bar plot of satisfaction rate by emotional intensity quartile
p_quartile <- ggplot(satisfaction_by_quartile, aes(x = factor(emotion_quartile),
                                                   y = Satisfaction_Rate)) +
  geom_bar(stat = "identity", fill = "#FF7F50") +
  geom_text(aes(label = paste0(round(Satisfaction_Rate,1), "%")), 
            vjust = -0.5, size = 4) +
  labs(title = "Satisfaction by Emotional Intensity Quartiles",
       x = "Emotional Intensity Quartile (Q1 = lowest, Q4 = highest)",
       y = "Satisfaction Rate (%)") +
  theme_minimal() +
  ylim(0, 100)

ggsave("satisfaction_by_quartile.png", p_quartile, width = 7, height = 5)

# ----- Combined Summary Table -----
# Numerical
num_vars <- c("age", "emotional_intensity", "liked")
numerical_summary <- data.frame(
  Variable = c("Age","Emotional Intensity","Liked"),
  Type = "Numerical",
  Category = NA,
  Count_Mean = sapply(num_vars, function(x) round(mean(music_sample[[x]]),2)),
  Percent_SD = sapply(num_vars, function(x) round(sd(music_sample[[x]]),2)),
  Min = sapply(num_vars, function(x) min(music_sample[[x]])),
  Max = sapply(num_vars, function(x) max(music_sample[[x]]))
)

# Categorical
categorical_summary <- data.frame(
  Variable = c("Gender", "Genre", "Mother Tongue"),
  Type = "Categorical",
  Category = c(
    paste(levels(music_sample$gender), collapse = " / "),
    paste(levels(music_sample$genre), collapse = " / "),
    "Top 5 + Other"
  ),
  Count_Mean = c(
    paste(table(music_sample$gender), collapse = " / "),
    paste(table(music_sample$genre), collapse = " / "),
    {
      # Mother Tongue top 5 + Other
      freq <- sort(table(music_sample$mother.tongue), decreasing = TRUE)
      top5 <- head(freq, 5)
      other <- sum(freq[-(1:5)])
      combined <- c(top5, Other = other)
      paste(names(combined), combined, sep = ": ", collapse = " / ")
    }
  ),
  Percent_SD = c(
    paste(round(prop.table(table(music_sample$gender))*100,1), collapse = " / "),
    paste(round(prop.table(table(music_sample$genre))*100,1), collapse = " / "),
    {
      freq <- sort(table(music_sample$mother.tongue), decreasing = TRUE)
      top5 <- head(freq, 5)
      other <- sum(freq[-(1:5)])
      combined <- c(top5, Other = other)
      paste(names(combined), round(combined/sum(combined)*100,1), "%", sep = "", collapse = " / ")
    }
  ),
  Min = NA,
  Max = NA
)

# Combine both
combined_summary <- rbind(numerical_summary, categorical_summary)
write.csv(combined_summary, "music_descriptive_combined_summary.csv", row.names = FALSE)
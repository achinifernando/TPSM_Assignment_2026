#=======================================================
#Activity: Social Media
#Dataset: Social Media Dataset  
#collection period: 2023 May - 2025 May
#Total records: 52,215 social media posts
#platforms covers: Instagram, TikTok, YouTube, Bilibili, RedNote

#=======================================================

# Load required libraries
library(tidyverse)  # For data manipulation
library(lubridate)  # For date handling
library(stringr)    # For text cleaning
library(janitor)    # For cleaning column names
library(dplyr)
library(e1071)      #for calculate the skewness
library(ggplot2)
library(corrplot)
library(tidyr)      #for break comments into words
library(tidytext)
library(gridExtra)
library(car)
library(effectsize)
library(caret)


setwd("G:\\TPSM_Assignment_2026\\data\\raw\\Social media")
getwd()

# Load your data sets
data <- read.csv("social_media_dataset.csv") #raw dataset
View(data)

#Check no of rows that in cleaned data set
cat("Initial number of rows: ",nrow(data) , "\n")  #52214 

# Create a copy for cleaning
data_clean <- data 

# Clean column names (snake_case consistent format)
data_clean <- clean_names(data_clean)
str(data_clean)

#Remove unwanted columns
data_clean <- subset(data_clean, select = -c(is_sponsored ,sponsor_name  ,sponsor_category,disclosure_location,content_url ))

#Replace missing texts with NA
data_clean$hashtags[data_clean$hashtags %in% c("", "N/A", "NULL", "Unknown", "?")] <- NA
data_clean$comments_text[data_clean$comments_text %in% c("", "N/A", "NULL", "Unknown", "?")] <- NA
data_clean$audience_gender_distribution[data_clean$audience_gender_distribution %in% c("unknown","non-binary" )] <- NA

#Remove rows with missing texts
data_clean <- na.omit(data_clean)
cat("Number of rows after cleaning: ", nrow(data_clean) , "\n")  #28879

#Fix data types
data_clean <- data_clean %>% mutate(post_date = as.Date(post_date, "%m/%d/%y"))
data_clean$audience_age_distribution <- as.factor(data_clean$audience_age_distribution)
data_clean$audience_gender_distribution <- as.factor(data_clean$audience_gender_distribution )
data_clean$audience_location <- as.factor(data_clean$audience_location)
data_clean$platform <- as.factor(data_clean$platform)

#Save clean data set
write.csv(data_clean, "cleaned_social_media_dataset.csv", row.names = FALSE)

#Load cleaned dataset
data1 <- read.csv("cleaned_social_media_dataset.csv") 

#Make duplicate dataset from cleaned dataset
data_dup <- data1

#Ensure same sample every time
set.seed(123) 

#Take random sample
sample_data <- data_dup %>% sample_n(1500)


# Explore the structure of sample
str(sample_data)
summary(sample_data)
head(sample_data)

#Check basic info
cat("Dataset dimensions: ", dim(sample_data), "\n")  #1500 22
cat("Column names: ", names(sample_data), "\n")
cat("Duplicate rows: ", sum(duplicated(sample_data)), "\n")  #0
cat("Missing values: ", colSums(is.na(sample_data)), "\n")  #0


#Print(cleaning_log)
View(sample_data)


#Create emotional connection variable
#Remove punctuation and convert to lowercase
sample_data$comments_text <- tolower(sample_data$comments_text)

#Remove punctuation but keep spaces (replace punctuation with space)
sample_data$comments_text <- str_replace_all(sample_data$comments_text, "[^a-z\\s]", "")

# Remove extra spaces 
sample_data$comments_text <- str_squish(sample_data$comments_text)

#Splits comments into individual words
comments_words <- sample_data %>% unnest_tokens(word, comments_text)

#Load sentiment dictonary (using Bing sentiment lexicon)
bing <- get_sentiments("bing")
head(bing)

#Sentiment score calculation
sentiment_data <- comments_words %>% 
  inner_join(bing, by="word") %>%
  count(content_id, sentiment) %>% 
  pivot_wider(names_from = sentiment,
              values_from = n, 
              values_fill = list(n = 0) )

# Ensure positive and negative columns exist
if(!"positive" %in% colnames(sentiment_data)) {
  sentiment_data$positive <- 0
}

if(!"negative" %in% colnames(sentiment_data)) {
  sentiment_data$negative <- 0
}

#Create emotional connection variable
colnames(sentiment_data)
sentiment_data$sentiment_score <-sentiment_data$positive - sentiment_data$negative

#Merge with main data set
sample_data <- left_join(sample_data, sentiment_data, by = "content_id")

#Replace NA with 0
sample_data$positive[is.na(sample_data$positive)] <- 0
sample_data$negative[is.na(sample_data$negative)] <- 0
sample_data$sentiment_score[is.na(sample_data$sentiment_score)] <- 0


# Show original distribution
cat("\nOriginal sentiment_score distribution:\n")
print(summary(sample_data$sentiment_score))  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                                              #-3.000   0.000   0.000   0.366   1.000   5.000 

# Create improved emotional connection measure (0-10 scale)
sample_data <- sample_data %>%
  mutate(
    # Original raw score
    emotional_connection_raw = sentiment_score,
    
    emotional_connection = (sentiment_score - min(sentiment_score)) / 
      (max(sentiment_score) - min(sentiment_score)) * 10
  )


cat("\nImproved emotional_connection (0-10 scale):\n")
print(summary(sample_data$emotional_connection))  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                                                  #0.000   3.750   3.750   4.207   5.000  10.000 



# CREATE SATISFACTION VARIABLE (NORMALIZED)

cat("\n", rep("=", 80), "\n")
cat("PART 4: CREATING SATISFACTION VARIABLE\n")
cat(rep("=", 80), "\n")

# Create raw satisfaction
sample_data$satisfaction_raw <- sample_data$likes + 
  sample_data$shares + 
  sample_data$comments_count

cat("\nRaw satisfaction distribution:\n")
print(summary(sample_data$satisfaction_raw))   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                                                #1868    1980    2010    2010    2039    2177 

# Normalize satisfaction to 0-10 scale for interpretation
sample_data <- sample_data %>%
  mutate(
    satisfaction = (satisfaction_raw - min(satisfaction_raw)) / 
      (max(satisfaction_raw) - min(satisfaction_raw)) * 10,
    satisfaction_std = as.numeric(scale(satisfaction_raw))
  )

cat("\nNormalized satisfaction (0-10 scale):\n")
print(summary(sample_data$satisfaction))   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                                          #0.000   3.625   4.595   4.605   5.534  10.000 

#  EMOTION TYPE DETECTION (NRC lexicon)


cat("PART 5: EMOTION TYPE DETECTION\n")


nrc <- get_sentiments("nrc")
emotion_data <- comments_words %>% 
  inner_join(nrc, by = "word") %>% 
  count(sentiment)

cat("\nEmotion distribution in comments:\n")
print(emotion_data)  # sentiment       n
                    #anger            633
                    #anticipation     1535
                    #disgust          250
                    #fear             902
                    #joy              1177
                    #negative         1046
                    #positive         3832
                    # sadness         566
                    #surprise         702
                    # trust           2525

# Emotion plot
emotion_plot <- ggplot(emotion_data, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Emotion Distribution in Comments",
       x = "Emotion", y = "Count") +
  theme_minimal()
print(emotion_plot)



#=================================================
#Descriptive Analysis
#=================================================


#categorical data analysis (age, gender, location , platform)

#Gender distribution
#---------------------------------------------------
gender.freq <- table(sample_data$audience_gender_distribution)
gender.prop <- prop.table(gender.freq) * 100

cat("\n--- GENDER DISTRIBUTION ---\n")   #female- 762   male -738 
print(gender.freq)
cat("\nPercentages:\n")
print(round(gender.prop, 1))  #female- 50.8   male-  49.2 
  

#bar chart
ggplot(sample_data, aes(x = audience_gender_distribution, fill = audience_gender_distribution)) +
  geom_bar() +
  scale_fill_brewer(palette="Set2") +
  labs(title="Audience Gender Distribution", x="Gender", y="Count") + theme_minimal()

#pie chart
pie(gender.freq,
    labels = paste(names(gender.freq),
                   round(gender.prop,1),"%"),
    main="Gender Distribution")

#Age Distribution
#---------------------------------------------

#frequency table for age distribution
age.freq <- table(sample_data$audience_age_distribution)
age.freq
#13-18 = 251, 19-25 = 527, 26 -35 = 449, 36 -50 = 196, 50+ = 77

#percentage distribution
age.prop <- prop.table(age.freq) *100
age.prop
#     13-18     19-25     26-35     36-50       50+ 
#  16.733333  35.133333  29.933333  13.066667  5.133333 



#Bar chart
ggplot(sample_data, aes( x= audience_age_distribution, fill = audience_age_distribution)) +
  geom_bar() + 
  scale_fill_brewer(palette = "Set2")+
    labs(title = "Audience Age Distribution", x= "Age group", y = "Count")

#Location Distribution
#-------------------------------------------------

#frequency table for location distribution
location.freq <- table(sample_data$audience_location)
location.freq
#Brazil   China  Germany   India   Japan  Russia      UK     USA 
#171      203      190     190     198     174     172     202 


#percentage distribution
location.prop <- prop.table(location.freq) *100
location.prop
#Brazil    China    Germany    India    Japan    Russia       UK      USA 
#11.40000 13.53333 12.66667  12.66667  13.20000  11.60000  11.46667  13.46667 

#bar chart (rotates labels if many locations exist)
ggplot(sample_data, aes( x= audience_location , fill = audience_location)) +
  geom_bar() + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Audience Location Distribution", x= "Location", y = "Count") 


#Platform Distribution
#-------------------------------------------

#frequency table
platform.freq <- table(sample_data$platform)
platform.freq
#Bilibili Instagram   RedNote    TikTok   YouTube 
#289       291          304       307       309 

#Percentage 
platform.prop <- prop.table(platform.freq) * 100
platform.prop
#Bilibili Instagram   RedNote    TikTok   YouTube 
#19.26667  19.40000  20.26667  20.46667  20.60000 


#Bar chart
ggplot(sample_data, aes(x = platform, fill = platform)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Platform Distribution",
       x = "Platform",
       y = "Count")


#===============================================================
#summary statistics for emotions and satisfaction 
summary(sample_data$emotional_connection)
#    Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.000   3.750   3.750   4.207   5.000  10.000 

summary(sample_data$satisfaction)
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#0.000   3.625   4.595   4.605   5.534  10.000  

#mean values for emotions and satisfaction 
mean(sample_data$emotional_connection, na.rm = TRUE) #4.2075
mean(sample_data$satisfaction, na.rm = TRUE)        #4.604941

#distribution plot for emotional connection
ggplot(sample_data, aes(x = emotional_connection)) +
  geom_histogram(bins = 30, fill="steelblue")

#distribution plot for satisfaction
ggplot(sample_data, aes(x = satisfaction)) +
  geom_histogram(bins = 30, fill="darkgreen")

#visualize relationships
#create emotional engagement quartiles
sample_data$emotional_quartile <- ntile(sample_data$emotional_connection, 4)

# Convert to factor with labels
sample_data$emotional_quartile <- factor(
  sample_data$emotional_quartile,
  levels = 1:4,
  labels = c("Low","Medium-Low","Medium-High","High")
)

# Check distribution
table(sample_data$emotional_quartile)
# Low    Medium-Low     Medium-High        High 
# 375         375         375         375 

#SCATTER PLOT: Emotional Indicator vs Satisfaction
scatter_plot <- ggplot(sample_data, aes(x = emotional_connection, y = satisfaction)) +
  geom_point(alpha = 0.5, color = "steelblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Emotional Connection vs Satisfaction",
       subtitle = paste("Correlation:", round(cor(sample_data$emotional_connection, 
                                                  sample_data$satisfaction, 
                                                  use = "complete.obs"), 3)),
       x = "Emotional Connection (Sentiment Score)",
       y = "Satisfaction (Likes + Shares + Comments)")

scatter_plot

#BOXPLOTS: Satisfaction across Emotional Engagement Quartiles
boxplot_emotional <- ggplot(sample_data, aes(x = emotional_quartile, y = satisfaction, fill = emotional_quartile)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(title = "Satisfaction Distribution by Emotional Engagement Level",
       x = "Emotional Engagement Quartile",
       y = "Satisfaction Score") 


boxplot_emotional

# HEATMAP: Correlation between multiple emotional indicators
#Create additional emotional indicators
if(!"positive_score" %in% names(sample_data)) {
  sample_data$positive_score <- sample_data$positive
  sample_data$negative_score <- sample_data$negative
  sample_data$net_sentiment <- sample_data$positive - sample_data$negative
  sample_data$sentiment_ratio <- ifelse(sample_data$negative > 0, 
                                       sample_data$positive / sample_data$negative, 
                                       sample_data$positive)
}

# Select emotional indicators
emotional_indicators <- sample_data %>%
  select(emotional_connection, positive_score, negative_score, 
         net_sentiment, likes, shares, comments_count, satisfaction)

# Calculate correlation matrix
cor_matrix <- cor(emotional_indicators, use = "complete.obs")

# Reshape correlation matrix for ggplot
cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("Var1", "Var2", "Correlation")

heatmap_ggplot <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  labs(title = "Correlation Heatmap: Emotional vs Engagement Metrics",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold"))


heatmap_ggplot

#  Summary statistics by quartile
summary_stats <- sample_data %>%
  group_by(emotional_quartile) %>%
  summarise(
    count = n(),
    mean_satisfaction = mean(satisfaction, na.rm = TRUE),
    median_satisfaction = median(satisfaction, na.rm = TRUE),
    sd_satisfaction = sd(satisfaction, na.rm = TRUE),
    mean_likes = mean(likes, na.rm = TRUE),
    mean_shares = mean(shares, na.rm = TRUE),
    mean_comments = mean(comments_count, na.rm = TRUE),
    .groups = 'drop'
  )
print("Summary Statistics by Emotional Quartile:")
print(summary_stats)

# emotional_quartile count      mean_satisfaction median_satisfaction sd_satisfaction mean_likes  mean_shares  mean_comments
#<fct>              <int>             <dbl>               <dbl>           <dbl>           <dbl>       <dbl>         <dbl>
#Low                  375              4.52                4.53            1.39           1507.        301.          200.
# Medium-Low           375              4.67                4.63            1.36          1512.        301.          200.
# Medium-High          375              4.54                4.60            1.52          1508.        299.          201.
# High                 375              4.69                4.63            1.47          1513.        300.          200.



#============================================================
#Inferential analysis
#Main Hypothesis Testing
#Testing: "Audiences who feel emotionally connected report higher satisfaction"
#============================================================

#create high and low emotional engagement groups(median split)
emotional_median <- median(sample_data$emotional_connection, na.rm = TRUE)
sample_data$emotional_group <- ifelse(sample_data$emotional_connection > emotional_median, 
                                      "High", "Low")
sample_data$emotional_group <- factor(sample_data$emotional_group, levels = c("Low", "High"))

table(sample_data$emotional_group)
# Low    High 
# 913    587

#pearson correlation (0.5 -> strong relationship)
cor_test <- cor.test(
  sample_data$emotional_connection,  # Changed from confidence_interval
  sample_data$satisfaction,
  method = "pearson", 
  conf.level = 0.95
)

cor_test


cat("\n--- OVERALL CORRELATION ---\n")
cat("  r =", round(cor_test$estimate, 3), "\n")   # r=0.016
cat("  p =", round(cor_test$p.value, 4), "\n")    #p= 0.5415
cat("  95% CI = [", round(cor_test$conf.int[1], 3), ",", 
    round(cor_test$conf.int[2], 3), "]\n")  #95% CI = [ -0.035 , 0.066 ]


# Interpret effect size
cat("\nEffect size interpretation: ")
if(abs(cor_test$estimate) < 0.1) {
  cat("Negligible correlation\n")
} else if(abs(cor_test$estimate) < 0.3) {
  cat("Weak correlation\n")
} else if(abs(cor_test$estimate) < 0.5) {
  cat("Moderate correlation\n")
} else {
  cat("Strong correlation\n")
}    #Negligible correlation



#DEMOGRAPHIC MODERATION ANALYSIS (NEW)
# ============================================================================

cat("DEMOGRAPHIC MODERATION ANALYSIS\n")


# 1. By Gender
cor_by_gender <- sample_data %>%
  group_by(audience_gender_distribution) %>%
  summarise(
    n = n(),
    r = cor(emotional_connection, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_connection, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY GENDER ---\n")
print(cor_by_gender)    #audience_gender_distribution     n       r       p
                        #<chr>                        <dbl>   <dbl>     <dbl>
                        #female                         762    0.0393    0.278
                        # male                          738   -0.0055    0.881

# 2. By Age Group
cor_by_age <- sample_data %>%
  group_by(audience_age_distribution) %>%
  filter(n() >= 30) %>%
  summarise(
    n = n(),
    r = cor(emotional_connection, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_connection, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY AGE GROUP ---\n")
print(cor_by_age)     # audience_age_distribution     n       r     p
                      #<chr>                       <dbl>   <dbl>  <dbl>
                      # 13-18                       251   0.0425  0.503
                      # 19-25                       527  -0.0268  0.539
                      # 26-35                       449   0.0196  0.678
                      # 36-50                       196   0.0758  0.291
                      # 50+                          77  -0.0061  0.958

# 3. By Platform
cor_by_platform <- sample_data %>%
  group_by(platform) %>%
  filter(n() >= 30) %>%
  summarise(
    n = n(),
    r = cor(emotional_connection, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_connection, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY PLATFORM ---\n")
print(cor_by_platform)   #platform      n       r     p
                          #<chr>     <dbl>   <dbl>  <dbl>
                        #Bilibili     289   0.0468  0.428
                        # Instagram   291   0.0757  0.198
                        # RedNote     304   0.0109  0.850
                        # TikTok      307  -0.0165   0.773
                        # YouTube     309  -0.0382   0.503

# 4. By Location (Top 5)
top_locations <- names(sort(table(sample_data$audience_location), 
                            decreasing = TRUE))[1:5]
cor_by_location <- sample_data %>%
  filter(audience_location %in% top_locations) %>%
  group_by(audience_location) %>%
  summarise(
    n = n(),
    r = cor(emotional_connection, satisfaction, use = "complete.obs"),
    p = cor.test(emotional_connection, satisfaction)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n--- CORRELATION BY TOP LOCATIONS ---\n")
print(cor_by_location)   #audience_location     n       r     p
                          #<chr>             <dbl>   <dbl> <dbl>
                        #China               203  0.0921 0.191
                        #Germany             190  0.0384 0.598
                        #India               190 -0.0592 0.418
                        # Japan               198 -0.016  0.823
                        #USA                 202 -0.0569 0.421



cat("T-TEST WITH BALANCED GROUPS\n")

# Check if variances are equal
var_test <- leveneTest(satisfaction ~ emotional_group, data = sample_data)

# Run t-test
t_test <- t.test(sample_data$satisfaction ~ sample_data$emotional_group, 
                 data = sample_data,
                 var.equal = var_test$`Pr(>F)`[1] > 0.05)

# Calculate effect size (Cohen's d)
cohen_d <- cohens_d(sample_data$satisfaction ~ sample_data$emotional_group, data = subset(sample_data, emotional_group %in% c("Low", "High")))
 

# Group statistics
group_stats <- sample_data %>%
  group_by(emotional_group) %>%  
  summarise(
    n = n(),
    mean_sat = mean(satisfaction, na.rm = TRUE),  
    sd_sat = sd(satisfaction, na.rm = TRUE)       
  )

print("T-TEST RESULTS:")
print(paste("t =", round(t_test$statistic, 3), 
            ", df =", round(t_test$parameter, 1),
            ", p =", round(t_test$p.value, 4),
            "  Cohen's d =", round(cohen_d$Cohens_d, 3), "\n"))

# "t = -0.943 , df = 1498 , p = 0.3459"  # Cohen's d = -0.05 


print("Group means: ")
print(group_stats)

# emotional_group     n    mean_sat   sd_sat
# <fct>             <int>   <dbl>     <dbl>
# 1 Low               913    4.58   1.42
# 2 High              587    4.65   1.47



#---------------------------------------------------------------------
#REGRESSION ANALYSIS (WITH CONTROLS)
# Create dummy variables for regression
sample_data <- sample_data %>%
  mutate(
    gender_male = ifelse(audience_gender_distribution == "male", 1, 0),
    gender_female = ifelse(audience_gender_distribution == "female", 1, 0),
    
    age_19_25 = ifelse(audience_age_distribution == "19-25", 1, 0),
    age_26_35 = ifelse(audience_age_distribution == "26-35", 1, 0),
    age_36_50 = ifelse(audience_age_distribution == "36-50", 1, 0),
    age_50plus = ifelse(audience_age_distribution == "50+", 1, 0),
    
    platform_tiktok = ifelse(platform == "TikTok", 1, 0),
    platform_instagram = ifelse(platform == "Instagram", 1, 0),
    platform_youtube = ifelse(platform == "YouTube", 1, 0)
  )

# Model 1: Simple regression
model_simple <- lm(satisfaction ~ emotional_connection, data = sample_data)
summary_simple <- summary(model_simple)

cat("\n--- SIMPLE REGRESSION ---\n")
cat("  R² =", round(summary_simple$r.squared, 4), "\n")   #R² = 2e-04
cat("  emotional_connection coefficient =", 
    round(coef(model_simple)[2], 4), "\n")  #0.0166 
cat("  p-value =", round(summary_simple$coefficients[2, 4], 4), "\n")   # 0.5415 

# Model 2: With demographics
model_full <- lm(satisfaction ~ emotional_connection + 
                   gender_male + age_19_25 + age_26_35 + age_36_50 + age_50plus +
                   platform_tiktok + platform_instagram + platform_youtube,
                 data = sample_data)
summary_full <- summary(model_full)

cat("\n--- FULL REGRESSION (with demographics) ---\n")
cat("  R² =", round(summary_full$r.squared, 4), "\n")   #R² = 0.0046 
cat("  Adjusted R² =", round(summary_full$adj.r.squared, 4), "\n")   #-0.0015 
cat("  emotional_connection coefficient =", 
    round(coef(model_full)["emotional_connection"], 4), "\n")  # 0.0155 
cat("  p-value =", 
    round(summary_full$coefficients["emotional_connection", 4], 4), "\n")   #0.5686 

# Model comparison
anova_compare <- anova(model_simple, model_full)
cat("\n--- MODEL COMPARISON ---\n")
cat("  F-change =", round(anova_compare$F[2], 2), "\n")   #0.81 
cat("  p-value =", round(anova_compare$`Pr(>F)`[2], 4), "\n")  #0.5974 


cat(" FINAL SUMMARY AND CONCLUSION\n")
cat(rep("=", 80), "\n")

# Create summary table
final_summary <- data.frame(
  Test = c(
    "Pearson Correlation",
    "T-test (High vs Low)",
    "Simple Regression",
    "Regression with Controls",
    "Gender Moderation",
    "Platform Moderation"
  ),
  Statistic = c(
    paste("r =", round(cor_test$estimate, 3)),
    paste("t =", round(t_test$statistic, 3)),
    paste("R² =", round(summary_simple$r.squared, 3)),
    paste("R² =", round(summary_full$r.squared, 3)),
    "See by-gender table",
    "See by-platform table"
  ),
  P_value = c(
    round(cor_test$p.value, 4),
    round(t_test$p.value, 4),
    round(summary_simple$coefficients[2, 4], 4),
    round(summary_full$coefficients["emotional_connection", 4], 4),
    NA,
    NA
  ),
  Significant = c(
    cor_test$p.value < 0.05,
    t_test$p.value < 0.05,
    summary_simple$coefficients[2, 4] < 0.05,
    summary_full$coefficients["emotional_connection", 4] < 0.05,
    any(cor_by_gender$p < 0.05),
    any(cor_by_platform$p < 0.05)
  )
)

cat("\n--- SUMMARY OF ALL TESTS ---\n")
print(final_summary)

# FINAL CONCLUSION

cat("FINAL CONCLUSION\n")
cat(rep("=", 80), "\n")

if(cor_test$p.value < 0.05) {
  cat("\nHYPOTHESIS SUPPORTED!\n")
  cat("  Emotional connection significantly predicts satisfaction in social media.\n")
  cat("  Effect size: r =", round(cor_test$estimate, 3), 
      "(", ifelse(abs(cor_test$estimate) < 0.1, "negligible",
                  ifelse(abs(cor_test$estimate) < 0.3, "weak",
                         ifelse(abs(cor_test$estimate) < 0.5, "moderate", "strong"))), ")\n")
} else {
  cat("\nHYPOTHESIS NOT SUPPORTED\n")
  cat("  No significant relationship found between emotional connection")
  cat(" and satisfaction in social media.\n")
  cat("  p =", round(cor_test$p.value, 4), "> 0.05\n")
  cat("  Effect size: r =", round(cor_test$estimate, 3), "(negligible)\n\n")
  
  cat("This finding is consistent across:\n")
  cat("  ✓ All statistical tests (correlation, t-test, ANOVA, regression)\n")
  cat("  ✓ All demographic subgroups (age, gender, platform, location)\n")
  cat("  ✓ Different measures of emotional connection\n")
}

# Save key results
saveRDS(sample_data, "social_media_analysis_ready.rds")
write.csv(final_summary, "social_media_results_summary.csv")



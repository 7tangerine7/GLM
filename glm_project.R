
# SETUP AND CONFIGURATION

set.seed(42)

# Global options
options(pillar.width = 130, width = 200, scipen = 999, digits = 5)

library(broom)
library(performance)
library(parameters)
library(car)
library(lmtest)
library(GGally)
library(ggpubr)
library(datawizard)
library(see)
library(effectsize)
library(insight)
library(correlation)
library(modelbased)
library(glue)
library(scales)
library(rstatix)
library(matrixTests)
library(ggfortify)
library(qqplotr)
library(collapse)
library(tidyverse) 

##### Data loading

data_raw <- read_csv("project.csv", show_col_types = FALSE)
head(data_raw, 10)
glimpse(data_raw)


# #####Convert coded categorical variables into properly labelled factors. • Convert coded categorical variables into properly labelled factors. Choose sensible reference categories.

c_cols <- c("agecat","sexe","school_type","parent_educ",
              "sleep_qual","attend_pct_cat","web_access",
              "trav_time","extra_act","study_method")


data_raw[c_cols] <- lapply(data_raw[c_cols], factor)

factor_vars <- names(data_raw)[sapply(data_raw, is.factor)]

data_clean <- data_raw |>
  mutate(
    sexe = factor(sexe, 
                  levels = c(1, 2, 3),
                  labels = c("Female", "Male", "Other")),
    
    school_type = factor(school_type,
                         levels = c(1, 2),
                         labels = c("Public", "Private")),
    
    parent_educ = factor(parent_educ,
                         levels = c(1, 2, 3, 4, 5, 6),
                         labels = c("No formal", "High school", "Graduate", 
                                   "Postgrad 1", "Postgrad 2", "PhD")),
    
    sleep_qual = factor(sleep_qual,
                        levels = c(1, 2, 3),
                        labels = c("Poor", "Average", "Good")),
    
    web_access = factor(web_access,
                        levels = c(1, 2),
                        labels = c("No", "Yes")),
    
    trav_time = factor(trav_time,
                       levels = c(1, 2, 3, 4),
                       labels = c("<15 min", "15-30 min", "30-60 min", ">60 min")),
    
    extra_act = factor(extra_act,
                       levels = c(1, 2),
                       labels = c("No", "Yes")),
    
    study_method = factor(study_method,
                          levels = c(1, 2, 3, 4, 5, 6),
                          labels = c("Alone", "Groups", "Online videos", 
                                    "Notes", "Flashcards", "Mixed"))
  )
data_clean<-data_clean |>
  relabel(
    id = "Student ID",
    y = "Exam score",
    age = "Age (years)",
    agecat = "Age category",
    sexe = "Sex",
    school_type = "School type",
    parent_educ = "Parental education",
    study_hrs = "Weekly study hours",
    sleep_hrs = "Sleep hours per night",
    sleep_qual = "Sleep quality",
    attend_pct = "Attendance percentage",
    attend_pct_cat = "Attendance category",
    web_access = "Internet access",
    trav_time = "Commute time",
    extra_act = "Extracurricular activities",
    study_method = "Study method"
  )
# Check factor levels and counts
table(data_clean$sexe)
table(data_clean$school_type)
table(data_clean$parent_educ)
table(data_clean$sleep_qual)
table(data_clean$web_access)
table(data_clean$study_method)

# Verify reference levels
levels(data_clean$sexe)  # Female 
levels(data_clean$school_type)  # Public 
levels(data_clean$parent_educ)  # No formal 
levels(data_clean$web_access)  # No 
levels(data_clean$extra_act)  # No 


######DATA INTEGRITY AND PLAUSIBILITY


# Check for duplicate IDs
sum(duplicated(data_clean$id))

# Check for missing values
# Just in case 
colSums(is.na(data_clean))

# Check plausible ranges
data_clean |>
  select(where(is.numeric)) |>
  summary()

# Check for outliers
ggplot(data_clean, aes(y = y)) +
  geom_boxplot(fill = "lightblue", color = "black", linewidth = 0.5) +
  labs(y = "Exam Score", title = "Boxplot of Exam Scores") +
  theme_bw(base_size = 14) +
  labs_pubr()




#######Identify potentially redundant representations

# We keep quantitative variabels as it gives more precise estimation

data_clean$agecat <- NULL
data_clean$attend_pct_cat <- NULL



# ============================================================================
# Question 2: EXPLORATORY DATA ANALYSIS
# ============================================================================
#Provide descriptive statistics for quantitative variables
data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct) |>
  describe_distribution()

# Frequency Tables for Categorical Variables
freq_table(data_clean$sexe)
freq_table(data_clean$school_type)
freq_table(data_clean$parent_educ)
freq_table(data_clean$sleep_qual)
freq_table(data_clean$web_access)
freq_table(data_clean$study_method)

# Distribution of Response Variable (y)

# Histogram with density
# Looks like normal distribution
# It looks approximately symmetric (close to bell-shaped), so no strong skewness
# Potential outliers: there are some suspicions on the edges (very good grades or very low grades) but
#nothing extraordinary
#Median is approximately the same as the mean

ggplot(data_clean, aes(x = y)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Dist of Exam Scores", 
       x = "Exam Score", 
       y = "Freq") +
  theme_minimal()

# Boxplot
ggplot(data_clean, aes(y = y)) +
  geom_boxplot(fill = "lightblue", color = "black", linewidth = 0.5) +
  labs(y = "Exam Score", title = "Boxplot of Exam Scores") +
  theme_bw(base_size = 14) +
  labs_pubr()


########Y VS QUANTATIVE PREDICTORS

# Scatterplots

# Individual scatterplots for continuous predictors
# Plot 1: study_hrs
ggplot(data_clean, aes(x = study_hrs, y = y)) +
  geom_point(alpha = 0.3, size = 2, shape = 21, fill = "dodgerblue", color = "black") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_smooth(method = "lm", color = "blue", linetype = 2, se = FALSE) +
  labs(x = "Weekly Study Hours", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()
# very close to linear. We decide to keep it simple and to not upgrade to a quadratic term
# Plot 2: age
ggplot(data_clean, aes(x = age, y = y)) +
  geom_point(alpha = 0.3, size = 2, shape = 21, fill = "dodgerblue", color = "black") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_smooth(method = "lm", color = "blue", linetype = 2, se = FALSE) +
  labs(x = "Age (years)", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()
#linear

# Plot 3: attend_pct
ggplot(data_clean, aes(x = attend_pct, y = y)) +
  geom_point(alpha = 0.3, size = 2, shape = 21, fill = "dodgerblue", color = "black") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_smooth(method = "lm", color = "blue", linetype = 2, se = FALSE) +
  labs(x = "Attendance Percentage", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()
#almost linear

# Plot 4: sleep_hrs
ggplot(data_clean, aes(x = sleep_hrs, y = y)) +
  geom_point(alpha = 0.3, size = 2, shape = 21, fill = "dodgerblue", color = "black") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_smooth(method = "lm", color = "blue", linetype = 2, se = FALSE) +
  labs(x = "Sleep Hours", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()
# it is a little bit non-linear but it is not significant so for simplicity and to avoid
# overfitting we prefer to keep linear term
#Plots don't show a big change in spread of y with change of x, 
#so we can assume that there no signs of heterescedacity






# Boxplots: y vs Categorical Predictors

# Boxplot: parent_educ
ggplot(data_clean, aes(x = parent_educ, y = y)) +
  geom_boxplot(fill = "lightgreen", color = "black", linewidth = 0.5) +
  labs(x = "Parental Education", y = "Exam Score") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs_pubr()

# Boxplot: sleep_qual
ggplot(data_clean, aes(x = sleep_qual, y = y)) +
  geom_boxplot(fill = "lightyellow", color = "black", linewidth = 0.5) +
  labs(x = "Sleep Quality", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()

# Boxplot: web_access
ggplot(data_clean, aes(x = web_access, y = y)) +
  geom_boxplot(fill = "lightpink", color = "black", linewidth = 0.5) +
  labs(x = "Internet Access", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()

# Boxplot: school_type
ggplot(data_clean, aes(x = school_type, y = y)) +
  geom_boxplot(fill = "lightcyan", color = "black", linewidth = 0.5) +
  labs(x = "School Type", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()

# Boxplot: extra_act
ggplot(data_clean, aes(x = extra_act, y = y)) +
  geom_boxplot(fill = "lavender", color = "black", linewidth = 0.5) +
  labs(x = "Extracurricular Activities", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()

# Boxplot: trav_time
ggplot(data_clean, aes(x = trav_time, y = y)) +
  geom_boxplot(fill = "peachpuff", color = "black", linewidth = 0.5) +
  labs(x = "Travel Time", y = "Exam Score") +
  theme_bw(base_size = 14) +
  labs_pubr()

# Boxplot: study_method
ggplot(data_clean, aes(x = study_method, y = y)) +
  geom_boxplot(fill = "lightblue", color = "black", linewidth = 0.5) +
  labs(x = "Study Method", y = "Exam Score") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs_pubr()


####Numeric Numeric associations

# Compute correlation matrix
cor_matrix <- data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct) |>
  cor(method = "pearson")

print(round(cor_matrix, 3))

# Visualize correlation matrix
data_clean |>
  select(y, age, study_hrs, sleep_hrs, attend_pct) |>
  correlation(method = "pearson") |>
  summary(redundant = TRUE) |>
  plot() +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")


# Correlation matrix for quantitative variables
quant_vars <- data_clean |> 
  select(where(is.numeric))

cor_matrix <- cor(quant_vars, use = "complete.obs")
cor_matrix

# Visualize correlations
ggcorr(quant_vars, label = TRUE, label_round = 2, hjust = 0.75)

# Pairwise plots for key variables
ggpairs(data_clean |> 
          select(y, age, study_hrs, sleep_hrs, attend_pct),
        title = "Pairwise Relationships - Key Variables")


# Numeric-Numeric associations (already done in correlation matrix)
# Key findings from section 2.5:
# - No strong linear dependencies among continuous predictors







####
# Categorical-Categorical associations
# Check if categorical predictors are related to each other

# Parent education vs School type
table(data_clean$parent_educ, data_clean$school_type)
chisq.test(data_clean$parent_educ, data_clean$school_type)

#Are associated with eachother

# Interpretation: Educated parents are more likely to send kids to private school?

# Sleep quality vs Extracurricular activities
table(data_clean$sleep_qual, data_clean$extra_act)
chisq.test(data_clean$sleep_qual, data_clean$extra_act)

# are associated
## Interpretation: Significant associations detected (p < 0.001), 
# suggesting confounding that must be addressed through inclusion in the model.


# Web access vs School type
table(data_clean$web_access, data_clean$school_type)
chisq.test(data_clean$web_access, data_clean$school_type)
# are associated

# Web access vs Study method
table(data_clean$web_access, data_clean$study_method)
chisq.test(data_clean$web_access, data_clean$study_method)
# are associated


# Study hours by internet access
data_clean |>
  group_by(web_access) |>
  summarize(mean_study = mean(study_hrs), sd_study = sd(study_hrs))

ggplot(data_clean, aes(x = web_access, y = study_hrs)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Internet Access", y = "Study Hours") +
  theme_bw(base_size = 14)


# Attendance by school type
data_clean |>
  group_by(school_type) |>
  summarize(mean_attend = mean(attend_pct), sd_attend = sd(attend_pct))

ggplot(data_clean, aes(x = school_type, y = attend_pct)) +
  geom_boxplot(fill = "lightgreen") +
  labs(x = "School Type", y = "Attendance %") +
  theme_bw(base_size = 14)

# Sleep hours by sleep quality
data_clean |>
  group_by(sleep_qual) |>
  summarize(mean_sleep_hrs = mean(sleep_hrs), sd_sleep_hrs = sd(sleep_hrs))

ggplot(data_clean, aes(x = sleep_qual, y = sleep_hrs)) +
  geom_boxplot(fill = "lightyellow") +
  labs(x = "Sleep Quality", y = "Sleep Hours") +
  theme_bw(base_size = 14)


# Study hours by parent education
ggplot(data_clean, aes(x = parent_educ, y = study_hrs)) +
  geom_boxplot(fill = "lightcoral") +
  labs(x = "Parental Education", y = "Study Hours") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summary of predictor associations:
# sleep quality is linear with sleep hrs. Correlation between internet access and study hrs apparent. Many variables are confounding



##For ordinal predictors, include at least one visualization that helps assess monotone trends in y across
##ordered levels.


# Sleep quality: mean exam score by level
sleep_means <- data_clean %>%
  group_by(sleep_qual) %>%
  summarize(mean_y = mean(y), n = n()) %>%
  mutate(level = row_number())

ggplot(sleep_means, aes(x = level, y = mean_y)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:3, 
                     labels = levels(data_clean$sleep_qual)) +
  labs(x = "Sleep Quality", y = "Mean Exam Score",
       title = "Monotone Trend: Exam Score by Sleep Quality") +
  theme_bw(base_size = 14)

# clearly Monotone

# Travel time: mean exam score by level
travel_means <- data_clean %>%
  group_by(trav_time) %>%
  summarize(mean_y = mean(y), n = n()) %>%
  mutate(level = row_number())

ggplot(travel_means, aes(x = level, y = mean_y)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:4, 
                     labels = levels(data_clean$trav_time)) +
  labs(x = "Travel Time", y = "Mean Exam Score",
       title = "Monotone Trend: Exam Score by Travel Time") +
  theme_bw(base_size = 14)


#clearly monotone


####### 3.10 EDA Summary and Key Findings

# KEY FINDING 1: Strongest predictors
# attend_pct (r = 0.74), study_hrs (r = 0.41), sleep_hrs (r = 0.57) (from correlation matrix)

# KEY FINDING 2: Categorical predictors show clear patterns
# parent_educ: monotone trend; sleep_qual: ordered; web_access: 

# KEY FINDING 3: Linearity - no transformations needed

#KEY FINDING 4: No multicolinearity detected from correlation matrix

#kEY FINDING 5: it seems age and sex have very similar means - may omit in the model.

# Exploring Potential Interactions

# Hypothesis 1: study_hrs × web_access

ggplot(data_clean, aes(x = study_hrs, y = y, color = factor(web_access))) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_viridis_d(end = 0.85) +
  labs(x = "Study Hours", y = "Exam Score", color = "Internet Access") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

# we don't see any interaction

# Hypothesis 2: sleep_qual × age
ggplot(data_clean, aes(x = age, y = y, color = sleep_qual)) +
  geom_point(alpha = 0.2, size = 1) +                
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +  
  scale_color_manual(values = c(
    "Poor"    = "#D55E00",
    "Average" = "#0072B2",
    "Good"    = "#009E73"
  )) +
  labs(x = "Age", y = "Exam Score", color = "Sleep Quality") +
  theme_bw(base_size = 14) +
  labs_pubr()
#we can see some interaction

# Hypothesis 3: sleep_hrs × sexe

ggplot(data_clean, aes(x = sleep_hrs, y = y, color = sexe)) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "Female" = "#0072B2",
    "Male"   = "#D55E00",
    "Other"  = "#009E73"
  )) +
  labs(x = "Sleep Hours", y = "Exam Score", color = "Sexe") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")
#there is slight interaction

# Hypothesis 4: study_hrs × sexe

ggplot(data_clean, aes(x = study_hrs, y = y, color = sexe)) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "Female" = "#0072B2",
    "Male"   = "#D55E00",
    "Other"  = "#009E73"
  )) +
  labs(x = "Study Hours", y = "Exam Score", color = "Sexe") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

#there is very slight interaction

# Hypothesis 5: study_hrs × sexe

ggplot(data_clean, aes(x = study_hrs, y = y, color = sexe)) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "Female" = "#0072B2",
    "Male"   = "#D55E00",
    "Other"  = "#009E73"
  )) +
  labs(x = "Study Hours", y = "Exam Score", color = "Sexe") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

# Hypothesis 6: sleep_hrs vs sleep qlt

ggplot(data_clean, aes(x = sleep_hrs, y = y, color = sleep_qual)) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "Poor"    = "#D55E00",
    "Average" = "#0072B2",
    "Good"    = "#009E73"
  )) +
  labs(x = "Sleep Hours", y = "Exam Score", color = "Sleep Quality") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

# we can see interactions! plot looks weird because they actually should be "shorter"

# Hypothesis 7: attend_pct vs trav_time

ggplot(data_clean, aes(x = attend_pct, y = y, color = trav_time)) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "<15 min"   = "#0072B2",
    "15-30 min" = "#D55E00",
    "30-60 min" = "#009E73",
    ">60 min"   = "#CC79A7"
  )) +
  labs(x = "Attendance (%)", y = "Exam Score", color = "Commute time") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

#  no interaction

# Hypothesis 8: study_hrs vs parent_educ

ggplot(data_clean, aes(x = study_hrs, y = y, color = parent_educ)) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "No formal"       = "#0072B2",
    "High school"     = "#D55E00",
    "Graduate"        = "#009E73",
    "Postgrad 1" = "#CC79A7",
    "Postgrad 2" = "#E69F00",
    "PhD"             = "#56B4E9"
  )) +
  labs(x = "Study hours", y = "Exam Score", color = "Parental education") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

# no interaction 

# Hypothesis 8: attend_pct vs web_access

ggplot(data_clean, aes(x = attend_pct, y = y, color = web_access)) +
  geom_point(alpha = 0.15, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "No"  = "#D55E00",
    "Yes" = "#0072B2"
  )) +
  labs(x = "Attendance (%)", y = "Exam Score", color = "Web access") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

# no interaction


ggplot(data_clean, aes(x = study_hrs, y = y, color = sleep_qual)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Study Hours", y = "Exam Score", color = "Sleep Quality",
       title = "train_dataing Data: sleep_qual × study_hrs") +
  theme_bw(base_size = 14)
# interaction

# conclusion: interaction that is interesting is sleep hours and sleep quality, 
# study hours and sleep quality









# ============================================================================
# Q3
# ============================================================================

# Simple Linear Regressions (Baseline Associations)
# We begin with simple regressions to establish baseline associations
# and build intuition about individual predictor relationships with y


# Simple regression: y ~ attend_pct
# explained 55% of the model, significant
slr_attend <- lm(y ~ attend_pct, data = data_clean)
summary(slr_attend)
model_parameters(slr_attend, ci_method = "residual")

# Simple regression: y ~ school_type
# 4%
slr_school_type <- lm(y ~ school_type, data = data_clean)
summary(slr_school_type)
model_parameters(slr_school_type, ci_method = "residual")

# Simple regression: y ~ study_hrs
#explain 32% of the model, significant
slr_study <- lm(y ~ study_hrs, data = data_clean)
summary(slr_study)
model_parameters(slr_study, ci_method = "residual")

# Simple regression: y ~ sleep_hrs
#explain 17% of the model, significant
slr_sleep <- lm(y ~ sleep_hrs, data = data_clean)
summary(slr_sleep)
model_parameters(slr_sleep, ci_method = "residual")

# Simple regression: y ~ age
# almost doesn't explain the model
slr_age <- lm(y ~ age, data = data_clean)
summary(slr_age)
model_parameters(slr_age, ci_method = "residual")

# Simple regression: y ~ parent_educ
# explains 10% of the model
slr_parent <- lm(y ~ parent_educ, data = data_clean)
summary(slr_parent)
model_parameters(slr_parent, ci_method = "residual")

# Simple regression: y ~ sexe
#almost doesn't explain the model
slr_sexe <- lm(y ~ sexe, data = data_clean)
summary(slr_sexe)
model_parameters(slr_sexe, ci_method = "residual")

#Simple regression: y ~ study_method
#2%
slr_study_method <- lm(y ~ study_method, data = data_clean)
summary(slr_study_method)
model_parameters(slr_study_method, ci_method = "residual")

# Simple regression: y ~ sleep_qual
# explain 10% of the model
slr_sleepqual <- lm(y ~ sleep_qual, data = data_clean)
summary(slr_sleepqual)
model_parameters(slr_sleepqual, ci_method = "residual")

# Simple regression: y ~ web_access
# explain 4% of the model
slr_web <- lm(y ~ web_access, data = data_clean)
summary(slr_web)
model_parameters(slr_web, ci_method = "residual")

# Simple regression: y ~ trav_time
#explain 17% of the model
slr_trav <- lm(y ~ trav_time, data = data_clean)
summary(slr_trav)
model_parameters(slr_trav, ci_method = "residual")

# Simple regression: y ~ extra_act
# explains 7%
slr_extra_act <- lm(y ~ extra_act, data = data_clean)
summary(slr_extra_act)
model_parameters(slr_extra_act, ci_method = "residual")


# Based on R coeff, we can conclude that age and sex are not important
# in order to accurately identify explanatory and predictive potential of models, we should split data



#Split the data


# Create 80-20 train-test split
set.seed(42)
train_indices <- sample(1:nrow(data_clean), size = 0.8 * nrow(data_clean))
train_data <- data_clean[train_indices, ]
test_data <- data_clean[-train_indices, ]

# Verify split
nrow(train_data)
nrow(test_data)

#• Compare candidate models using criteria covered in class/labs  Summarize comparisons in a short model-selection table and justify the
#final choice.

# Null model
# for intuition and reference
null_model <- lm(y ~ 1, data = train_data)
summary(null_model)
model_parameters(null_model, ci_method = "residual")

####Model 1:

model1 <- lm(y ~ attend_pct + study_hrs + sleep_hrs + parent_educ + 
               sleep_qual + trav_time, 
             data = train_data)

summary(model1)
model_parameters(model1, ci_method = "residual", digits = 3)
glance(model1)
model_performance(model1)
vif(model1)


pred <- predict(model1, newdata = test_data)

rmse <- sqrt(mean((test_data$y - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test_data$y - pred), na.rm = TRUE)
r2   <- 1 - sum((test_data$y - pred)^2, na.rm = TRUE) / sum((test_data$y - mean(test_data$y, na.rm=TRUE))^2, na.rm=TRUE)

c(RMSE = rmse, MAE = mae, R2_test_data = r2)

baseline_pred <- rep(mean(train_data$y, na.rm=TRUE), nrow(test_data))
baseline_rmse <- sqrt(mean((test_data$y - baseline_pred)^2, na.rm=TRUE))
baseline_rmse

calib_df <- data.frame(observed = test_data$y, predicted = pred)

ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(x = "Predicted y", y = "Observed y",
       title = "Calibration check: Predicted vs Observed") +
  theme_bw(base_size = 14)

# Model 2: Extended Model

model2 <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
               parent_educ + sleep_qual + trav_time +
               web_access + school_type + extra_act + study_method,
             data = train_data)



summary(model2)
model_parameters(model2, ci_method = "residual", digits = 3)
glance(model2)
model_performance(model2)
vif(model2)

m_back <- step(model2, direction = "backward", trace = 1)

summary(m_back)
AIC(m_back)
formula(m_back)

pred <- predict(model2, newdata = test_data)

rmse <- sqrt(mean((test_data$y - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test_data$y - pred), na.rm = TRUE)
r2   <- 1 - sum((test_data$y - pred)^2, na.rm = TRUE) / sum((test_data$y - mean(test_data$y, na.rm=TRUE))^2, na.rm=TRUE)

c(RMSE = rmse, MAE = mae, R2_test_data = r2)

baseline_pred <- rep(mean(train_data$y, na.rm=TRUE), nrow(test_data))
baseline_rmse <- sqrt(mean((test_data$y - baseline_pred)^2, na.rm=TRUE))
baseline_rmse

calib_df <- data.frame(observed = test_data$y, predicted = pred)

ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(x = "Predicted y", y = "Observed y",
       title = "Calibration check: Predicted vs Observed") +
  theme_bw(base_size = 14)

# model 2 predicts better

## model 2a

model2a = m_back
## better model by AIC back-step



# test_data: Model 1 vs Model 2


anova(model1, model2a)
AIC(model1, model2a)
BIC(model1, model2a)
#model 2a is better

# Interaction Hypothesis 1: sleep_qual × study_hrs




############# Model 3a: Add sleep_qual:study_hrs
 

model3a <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                parent_educ + sleep_qual + trav_time +
                web_access + school_type + study_method +
                sleep_qual:study_hrs,
              data = train_data)

summary(model3a)
model_parameters(model3a, ci_method = "residual", digits = 3)
glance(model3a)

# test_data significance of this interaction
anova(model2a, model3a)

pred <- predict(model3a, newdata = test_data)

rmse <- sqrt(mean((test_data$y - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test_data$y - pred), na.rm = TRUE)
r2   <- 1 - sum((test_data$y - pred)^2, na.rm = TRUE) / sum((test_data$y - mean(test_data$y, na.rm=TRUE))^2, na.rm=TRUE)

c(RMSE = rmse, MAE = mae, R2_test_data = r2)

baseline_pred <- rep(mean(train_data$y, na.rm=TRUE), nrow(test_data))
baseline_rmse <- sqrt(mean((test_data$y - baseline_pred)^2, na.rm=TRUE))
baseline_rmse

calib_df <- data.frame(observed = test_data$y, predicted = pred)

ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(x = "Predicted y", y = "Observed y",
       title = "Calibration check: Predicted vs Observed") +
  theme_bw(base_size = 14)

# better model



####### Model 3b: Add sleep_hrs:sleep_qual (keeping sleep_qual:study_hrs)


model3b <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                parent_educ + sleep_qual + trav_time +
                web_access + school_type + study_method +
                sleep_qual:study_hrs + sleep_hrs:sleep_qual,
              data = train_data)

summary(model3b)
model_parameters(model3b, ci_method = "residual", digits = 3)
glance(model3b)

# test_data significance of adding this interaction


anova(model3a, model3b)
Anova(model3b)

pred <- predict(model3b, newdata = test_data)

rmse <- sqrt(mean((test_data$y - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test_data$y - pred), na.rm = TRUE)
r2   <- 1 - sum((test_data$y - pred)^2, na.rm = TRUE) / sum((test_data$y - mean(test_data$y, na.rm=TRUE))^2, na.rm=TRUE)

c(RMSE = rmse, MAE = mae, R2_test_data = r2)

baseline_pred <- rep(mean(train_data$y, na.rm=TRUE), nrow(test_data))
baseline_rmse <- sqrt(mean((test_data$y - baseline_pred)^2, na.rm=TRUE))
baseline_rmse

calib_df <- data.frame(observed = test_data$y, predicted = pred)

ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(x = "Predicted y", y = "Observed y",
       title = "Calibration check: Predicted vs Observed") +
  theme_bw(base_size = 14)

#better

model3c <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                parent_educ + sleep_qual + trav_time +
                web_access + school_type + study_method +
                sleep_qual:study_hrs + sleep_hrs:sleep_qual +parent_educ:school_type,
              data = train_data)

summary(model3c)
model_parameters(model3c, ci_method = "residual", digits = 3)
glance(model3c)

pred <- predict(model3c, newdata = test_data)

rmse <- sqrt(mean((test_data$y - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test_data$y - pred), na.rm = TRUE)
r2   <- 1 - sum((test_data$y - pred)^2, na.rm = TRUE) / sum((test_data$y - mean(test_data$y, na.rm=TRUE))^2, na.rm=TRUE)

c(RMSE = rmse, MAE = mae, R2_test_data = r2)

baseline_pred <- rep(mean(train_data$y, na.rm=TRUE), nrow(test_data))
baseline_rmse <- sqrt(mean((test_data$y - baseline_pred)^2, na.rm=TRUE))
baseline_rmse

calib_df <- data.frame(observed = test_data$y, predicted = pred)

ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(x = "Predicted y", y = "Observed y",
       title = "Calibration check: Predicted vs Observed") +
  theme_bw(base_size = 14)



model3d <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                parent_educ + sleep_qual + trav_time +
                web_access + school_type +  study_method +
                sleep_qual:study_hrs + sleep_hrs:sleep_qual +parent_educ:school_type +
                web_access:school_type,
              data = train_data)

summary(model3d)
model_parameters(model3d, ci_method = "residual", digits = 3)
glance(model3d)

Anova(model3d)


#seems like web_access:school_type doesn't contribute much, we go back to previous model


anova(model3d)

pred <- predict(model3d, newdata = test_data)

rmse <- sqrt(mean((test_data$y - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test_data$y - pred), na.rm = TRUE)
r2   <- 1 - sum((test_data$y - pred)^2, na.rm = TRUE) / sum((test_data$y - mean(test_data$y, na.rm=TRUE))^2, na.rm=TRUE)

c(RMSE = rmse, MAE = mae, R2_test_data = r2)

baseline_pred <- rep(mean(train_data$y, na.rm=TRUE), nrow(test_data))
baseline_rmse <- sqrt(mean((test_data$y - baseline_pred)^2, na.rm=TRUE))
baseline_rmse

calib_df <- data.frame(observed = test_data$y, predicted = pred)

ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(x = "Predicted y", y = "Observed y",
       title = "Calibration check: Predicted vs Observed") +
  theme_bw(base_size = 14)


model3e <- lm(y ~ attend_pct + study_hrs + sleep_hrs + 
                parent_educ + sleep_qual + trav_time +
                web_access + school_type +  study_method +
                sleep_qual:study_hrs + sleep_hrs:sleep_qual +parent_educ:school_type +web_access:study_method,
              data = train_data)

summary(model3e)
model_parameters(model3e, ci_method = "residual", digits = 3)
glance(model3e)

anova(model3c, model3e)
anova(model3e)


#the bigger model is better

pred <- predict(model3e, newdata = test_data)

rmse <- sqrt(mean((test_data$y - pred)^2, na.rm = TRUE))
mae  <- mean(abs(test_data$y - pred), na.rm = TRUE)
r2   <- 1 - sum((test_data$y - pred)^2, na.rm = TRUE) / sum((test_data$y - mean(test_data$y, na.rm=TRUE))^2, na.rm=TRUE)

c(RMSE = rmse, MAE = mae, R2_test_data = r2)

baseline_pred <- rep(mean(train_data$y, na.rm=TRUE), nrow(test_data))
baseline_rmse <- sqrt(mean((test_data$y - baseline_pred)^2, na.rm=TRUE))
baseline_rmse

calib_df <- data.frame(observed = test_data$y, predicted = pred)

ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(x = "Predicted y", y = "Observed y",
       title = "Calibration check: Predicted vs Observed") +
  theme_bw(base_size = 14)



# Model comparison


compare_performance(model1, model2, model2a, model3a, model3b, model3c, model3d, model3e)
#model3e doesn't improve anything
#seems like model3b is the best considering anova tests

# Detailed comparison
AIC(model1, model2, model2a, model3a, model3b, model3c, model3d, model3e)
BIC(model1, model2, model2a, model3a, model3b, model3c, model3d, model3e)

metrics_test <- function(model, test) {
  pred <- predict(model, newdata = test)
  y <- test$y
  
  mse   <- mean((y - pred)^2)
  rmse  <- sqrt(mse)
  medae <- median(abs(y - pred))
  r2_oos <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  
  c(MSE = mse, RMSE = rmse, MedAE = medae, R2_oos = r2_oos)
}

results <- bind_rows(
  data.frame(Model = "Baseline (y ~ 1)", t(metrics_test(null_model,  test_data))),
  data.frame(Model = "Model 1",      t(metrics_test(model1, test_data))),
  data.frame(Model = "Model 2",      t(metrics_test(model2, test_data))),
  data.frame(Model = "2a",            t(metrics_test(model2a, test_data))),
  data.frame(Model = "3a",            t(metrics_test(model3a, test_data))),
  data.frame(Model = "3b",            t(metrics_test(model3b, test_data))),
  data.frame(Model = "3c",            t(metrics_test(model3c, test_data))),
  data.frame(Model = "3d",            t(metrics_test(model3d, test_data))),
  data.frame(Model = "3e",            t(metrics_test(model3e, test_data)))
)

results

#keep model 3b as after it we don't see big improvement and this model is simpler

#Quick check since our intution said that study hrs and sleep hrs should be non linear, and the plot was a little quadratic/Check robustness


#Both are pretty linear, but small non linearity is detected particularly in weekly study hrs.
## Quadratic Study Hours 
model_quad_study <- lm(y ~ attend_pct + study_hrs + I(study_hrs^2) + sleep_hrs + 
                         parent_educ + sleep_qual + trav_time +
                         web_access + school_type + study_method +
                         sleep_qual:study_hrs + sleep_hrs:sleep_qual,
                       data = train_data)

## Quadratic Sleep Hours 
model_quad_sleep <- lm(y ~ attend_pct + study_hrs + sleep_hrs + I(sleep_hrs^2) + 
                         parent_educ + sleep_qual + trav_time +
                         web_access + school_type + study_method +
                         sleep_qual:study_hrs + sleep_hrs:sleep_qual,
                       data = train_data)

## Compare All Models
comparison_table <- tibble(
  Model = c("Model 3b (Linear)", "Quadratic Study", "Quadratic Sleep"),
  `Adj R^2` = c(
    summary(model3b)$adj.r.squared,
    summary(model_quad_study)$adj.r.squared,
    summary(model_quad_sleep)$adj.r.squared
  ),
  AIC = c(
    AIC(model3b),
    AIC(model_quad_study),
    AIC(model_quad_sleep)
  ),
  BIC = c(
    BIC(model3b),
    BIC(model_quad_study),
    BIC(model_quad_sleep)
  ),
  `Test RMSE` = c(
    sqrt(mean((test_data$y - predict(model3b, test_data))^2)),
    sqrt(mean((test_data$y - predict(model_quad_study, test_data))^2)),
    sqrt(mean((test_data$y - predict(model_quad_sleep, test_data))^2))
  )
)

comparison_table

# F-tests
anova(model3b, model_quad_study)  # Test quadratic study
anova(model3b, model_quad_sleep)  # Test quadratic sleep
#adj R^2=0.737, and RMSE 7.57 but Model 3b is simpler and only marginally worse.
#quadratic study is more relevant than quadratic sleep but not enough to warrant adding to the model, since we generally prefer smaller models. We prefer model 3b.
## Final model: model b!


final <- model3b


# ============================================================================
# Q4
# ============================================================================
#Provide and comment briefly on diagnostics for your final model (include at least: residuals vs fitted,
#residuals vs predictors, residuals vs omitted variables, Q–Q plot, and influence/leverage diagnostics such
#as Cook’s distance).


summary(final)
model_parameters(final, ci_method = "residual", digits = 3)
glance(final)

autoplot(final,1:4)


#No issues detected. Clearer statement in html file.


# ============================================================================
# Q5
# ============================================================================

#Present a coefficient table with estimates, standard errors, and 95% confidence intervals

parameters::model_parameters(final, ci_method = "residual")


#Interpret 3–6 key terms with attention to magnitude and units (meaningful increments for continuous
#predictors; correct contrasts for categorical/ordinal predictors). - discussion in main qmd file



#Provide scenario-based interpretation: define at least two realistic student profiles and report predicted
#scores (or predicted differences), with confidence intervals.

student1 <- data.frame(
  attend_pct = 95,
  study_hrs = 25,
  sleep_hrs = 8,
  parent_educ = "PhD",
  sleep_qual = "Good",
  trav_time = "<15 min",
  web_access = "Yes",
  school_type = "Private",
  study_method = "Mixed"
)

pred1_scenario <- predict(final, newdata = student1, 
                          interval = "prediction", level = 0.95)



#Scenario 2: 

student2 <- data.frame(
  attend_pct = 60,
  study_hrs = 8,
  sleep_hrs = 5,
  parent_educ = "No formal",
  sleep_qual = "Poor",
  trav_time = ">60 min",
  web_access = "No",
  school_type = "Public",
  study_method = "Online videos"
)

pred2_scenario <- predict(final, newdata = student2, 
                          interval = "prediction", level = 0.95)
print(pred2_scenario)

library(lmtest)

r2_test <- function(fit) {
  pred <- predict(fit, newdata = test_data)
  1 - sum((test_data$y - pred)^2) / sum((test_data$y - mean(test_data$y))^2)
}

# 1. Are the residuals of model3b pure noise?
res <- residuals(model3b)
shapiro.test(res)          # normality: a large p-value means no evidence against Gaussian noise
bptest(model3b)            # Breusch-Pagan: a large p-value means constant variance
resettest(model3b, power = 2:3, type = "fitted")  # RESET: is there curvature left in the fitted values?
# With n = 4000 the RESET test detects even tiny leftover curvature; step 2 checks
# whether capturing it actually improves out-of-sample prediction.

# 2. A much richer linear model: every pairwise interaction plus squared terms
rich <- lm(y ~ (attend_pct + study_hrs + sleep_hrs + age + parent_educ + sleep_qual +
                  trav_time + web_access + school_type + study_method + extra_act + sexe)^2 +
             I(attend_pct^2) + I(study_hrs^2) + I(sleep_hrs^2) + I(age^2),
           data = train_data)

# 3. Noise ceiling: if the rich model contains the true signal, its residual
#    variance estimates the irreducible noise, and no model can do better than
#    R^2_max = 1 - sigma^2 / Var(y)
sigma2_hat <- summary(rich)$sigma^2
r2_ceiling <- 1 - sigma2_hat / var(test_data$y)

ceiling_table <- data.frame(
  model   = c("model3b (ours)", "rich model (all interactions + squares)", "noise ceiling"),
  n_coef  = c(length(coef(model3b)), length(coef(rich)), NA),
  test_R2 = round(c(r2_test(model3b), r2_test(rich), r2_ceiling), 4)
)
print(ceiling_table)
cat(sprintf("model3b reaches %.1f%% of the achievable R^2\n",
            100 * r2_test(model3b) / r2_ceiling))


#Rest done in main QMD file. as mostly comments.


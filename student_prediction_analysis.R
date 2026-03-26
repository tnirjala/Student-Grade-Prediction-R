# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
library(effects)
library(vcd)
library(caret)
library(FactoMineR)
library(factoextra)
library(tidyr)
# Read the CSV file into a dataframe
student_data <- read.csv("data/student_prediction.csv")

# DATA PREPROCESSING
# Inspecting the data set
head(student_data)
tail(student_data)
str(student_data)
summary(student_data)

# check if null value exists and removing them
is.na(student_data)
na.omit(student_data)

# find and remove duplicates in the data set
duplicated(student_data)
distinct(student_data)

# Get the column names
column_names <- colnames(student_data)
print(column_names)

# Function to get unique values for each column
unique_values <- sapply(student_data[-1], unique)

# Print the unique values for each column
print(unique_values)

# Decode data
decoded_data <- student_data %>%
  mutate(
    AGE = recode(AGE, `1` = "<18 yrs", `2` = "18-22 yrs", `3` = ">20 yrs"),
    GENDER = recode(GENDER, `1` = "Male", `2` = "Female"),
    HS_TYPE = recode(HS_TYPE, `1` = "Public", `2` = "Private", `3` = "Home School"),
    SCHOLARSHIP = recode(SCHOLARSHIP, `1` = "None", `2` = "Partial", `3` = "Full", `4` = "Merit", `5` = "Need-based"),
    WORK = recode(WORK, `1` = "No", `2` = "Yes"),
    ACTIVITY = recode(ACTIVITY, `1` = "No", `2` = "Yes"),
    PARTNER = recode(PARTNER, `1` = "No", `2` = "Yes"),
    SALARY = recode(SALARY, `1` = "<RS.20k", `2` = "RS.20k-RS.40k", `3` = "RS.40k-RS.60k", `4` = "RS.60k-RS.80k", `5` = ">RS.80k"),
    TRANSPORT = recode(TRANSPORT, `1` = "Public", `2` = "Private", `3` = "Bike", `4` = "Walk"),
    LIVING = recode(LIVING, `1` = "Dorm", `2` = "Apartment", `3` = "House", `4` = "With Parents"),
    MOTHER_EDU = recode(MOTHER_EDU, `1` = "No Schooling", `2` = "Primary", `3` = "Secondary", `4` = "High School", `5` = "Undergraduate", `6` = "Graduate"),
    FATHER_EDU = recode(FATHER_EDU, `1` = "No Schooling", `2` = "Primary", `3` = "Secondary", `4` = "High School", `5` = "Undergraduate", `6` = "Graduate"),
    X._SIBLINGS = recode(X._SIBLINGS, `1` = "0", `2` = "1", `3` = "2", `4` = "3", `5` = "4+"),
    KIDS = recode(KIDS, `1` = "None", `2` = "One", `3` = "More than 1"),
    MOTHER_JOB = recode(MOTHER_JOB, `1` = "Unemployed", `2` = "Laborer", `3` = "Clerk", `4` = "Manager", `5` = "Professional"),
    FATHER_JOB = recode(FATHER_JOB, `1` = "Unemployed", `2` = "Laborer", `3` = "Clerk", `4` = "Manager", `5` = "Professional"),
    STUDY_HRS = recode(STUDY_HRS, `1` = "less than 1", `2` = "1 to 3", `3` = "3 to 5", `4` = "5 to 7", `5` = " more than 7"),
    READ_FREQ = recode(READ_FREQ, `1` = "Never", `2` = "Sometimes", `3` = "Often"),
    READ_FREQ_SCI = recode(READ_FREQ_SCI, `1` = "Never", `2` = "Sometimes", `3` = "Often"),
    ATTEND_DEPT = recode(ATTEND_DEPT, `1` = "Never", `2` = "Often"),
    IMPACT = recode(IMPACT, `1` = "Low", `2` = "Moderate", `3` = "High"),
    ATTEND = recode(ATTEND, `1` = "Regular", `2` = "Irregular"),
    PREP_STUDY = recode(PREP_STUDY, `1` = "Low", `2` = "Moderate", `3` = "High"),
    PREP_EXAM = recode(PREP_EXAM, `1` = "Low", `2` = "Moderate", `3` = "High"),
    NOTES = recode(NOTES, `1` = "Never", `2` = "Sometimes", `3` = "Often"),
    LISTENS = recode(LISTENS, `1` = "Never", `2` = "Sometimes", `3` = "Often"),
    LIKES_DISCUSS = recode(LIKES_DISCUSS, `1` = "Never", `2` = "Sometimes", `3` = "Often"),
    CLASSROOM = recode(CLASSROOM, `1` = "Quiet", `2` = "Moderate", `3` = "Noisy"),
    CUML_GPA = recode(CUML_GPA, `1` = "<2.0", `2` = "2.0-2.5", `3` = "2.5-3.0", `4` = "3.0-3.5", `5` = ">3.5"),
    EXP_GPA = recode(EXP_GPA, `1` = "<2.0", `2` = "2.0-2.5", `3` = "2.5-3.0", `4` = "3.0-3.5"),
    GRADE = recode(GRADE, `0` = "F", `1` = "D", `2` = "C", `3` = "C+", `4` = "B", `5` = "B+", `6` = "A", `7` = "A+")
  )

# Save to CSV
write.csv(decoded_data, "data/student.csv")


#-------------------------------------------------------------------------------
# NP069477
#-------------------------------------------------------------------------------
#Question 1: What is the average GPA of students in various courses? How does it vary with age group and high school type?
#Analysis 1.1
# Calculate average GPA for each course
avg_gpa_course <- student_data %>%
  mutate(CUML_GPA = as.numeric(recode(CUML_GPA, `1` = "1.5", `2` = "2.25", `3` = "2.75", `4` = "3.25", `5` = "3.75"))) %>%
  group_by(COURSE.ID) %>%
  summarize(avg_gpa = mean(CUML_GPA, na.rm = TRUE))
print(avg_gpa_course)

# Line plot of average GPA by course
ggplot(avg_gpa_course, aes(x = factor(COURSE.ID), y = avg_gpa, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average GPA by Course", x = "Course ID", y = "Average GPA") +
  theme_minimal()

#Analysis 1.2
# Calculate mean GPA by course and age group
mean_gpa_course_age <- student_data %>%
  mutate(CUML_GPA = as.numeric(recode(CUML_GPA, `1` = "1.5", `2` = "2.25", `3` = "2.75", `4` = "3.25", `5` = "3.75"))) %>%
  group_by(COURSE.ID, AGE) %>%
  summarize(mean_gpa = mean(CUML_GPA, na.rm = TRUE))
print(mean_gpa_course_age,n=Inf)

# Grouped Bar Chart for average GPA by course and age group
ggplot(mean_gpa_course_age, aes(x = factor(COURSE.ID), y = mean_gpa, 
                                fill = factor(AGE,levels = 1:3,labels = c("less than 18 yrs","18-22 yrs","more than 22 yrs")))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average GPA by Course and Age Group", x = "Course ID", y = "Average GPA", fill = "Age Group") +
  theme_minimal()

#Analysis 1.3
# Calculate mean GPA by course and high school type
mean_gpa_course_hs <- student_data %>%
  mutate(CUML_GPA = as.numeric(recode(CUML_GPA, `1` = "1.5", `2` = "2.25", `3` = "2.75", `4` = "3.25", `5` = "3.75"))) %>%
  group_by(COURSE.ID, HS_TYPE) %>%
  summarize(mean_gpa = mean(CUML_GPA, na.rm = TRUE))
print(mean_gpa_course_hs,n=Inf)

# Plot average GPA by course and high school type
ggplot(mean_gpa_course_hs, aes(x = factor(COURSE.ID), y = mean_gpa, 
                               fill = factor(HS_TYPE,levels=1:3,labels = c("public","private","home schooled")))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average GPA by Course and High School Type", x = "Course ID", y = "Average GPA", fill = "High School Type") +
  theme_minimal()   

#Question 2: Analyze the Impact of Parental education and Socioeconomic Factors on Student’s Academic Performance?
# Calculate correlation matrix
cor_data <- student_data %>%
  select(MOTHER_EDU, FATHER_EDU, SALARY, CUML_GPA) %>%
  cor(use = "complete.obs")
print(cor_data)

# Melt the correlation matrix
melted_cor_data <- melt(cor_data)

# Plot correlation matrix using ggplot2
ggplot(melted_cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  labs(title = "Correlation Matrix for Parental Education, Salary, and GPA", 
       x = "Variables", y = "Variables")

# Define the mappings to coded values
mother_edu_labels <- c("No Formal Education", "Primary", "Secondary", "High School", "Undergraduate", "Graduate")
father_edu_labels <- c("No Formal Education", "Primary", "Secondary", "High School", "Undergraduate", "Graduate")
salary_labels <- c("<RS.20k", "RS.20k-RS.40k", "RS.40k-RS.60k", "RS.60k-RS.80k", ">RS.80k")
gpa_labels <- c("<2.0", "2.0-2.5", "2.5-3.0", "3.0-3.5", ">3.5")

# Scatter Plot for Mother's Education and GPA
ggplot(student_data, aes(x = factor(MOTHER_EDU, levels = 1:6, labels = mother_edu_labels), 
                         y = factor(CUML_GPA, levels = 1:5, labels = gpa_labels))) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", col = "red") +
  labs(title = "Mother's Education vs GPA", x = "Mother's Education Level", y = "Cumulative GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter Plot for Father's Education and GPA
ggplot(student_data, aes(x = factor(FATHER_EDU, levels = 1:6, labels = father_edu_labels), 
                         y = factor(CUML_GPA, levels = 1:5, labels = gpa_labels))) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", col = "red") +
  labs(title = "Father's Education vs GPA", x = "Father's Education Level", y = "Cumulative GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter Plot for Salary and GPA
ggplot(student_data, aes(x = factor(SALARY, levels = 1:5, labels = salary_labels), 
                         y = factor(CUML_GPA, levels = 1:5, labels = gpa_labels))) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", col = "red") +
  labs(title = "Family Salary vs GPA", x = "Family Salary Range", y = "Cumulative GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Question 3: Is there an association between students' reading habits (READ_FREQ) and attendance (ATTEND) with their grades (GRADE)?
# Null Hypothesis (H0): There is no association between students' reading habits and attendance with their grades.
# Alternative Hypothesis (H1): There is an association between students' reading habits and attendance with their grades.
# Create contingency table for READ_FREQ and GRADE
table_read_freq_grade <- table(student_data$READ_FREQ, student_data$GRADE)

# Perform chi-square test
chi_square_read_freq_grade <- chisq.test(table_read_freq_grade)

# Print results
print(chi_square_read_freq_grade)

# Create contingency table for ATTEND and GRADE
table_attend_grade <- table(student_data$ATTEND, student_data$GRADE)

# Perform chi-square test
chi_square_attend_grade <- chisq.test(table_attend_grade)

# Print results
print(chi_square_attend_grade)

# Mosaic plot for READ_FREQ and GRADE
mosaic(~ READ_FREQ + GRADE, data = decoded_data,
       main = "Mosaic Plot: Reading Frequency vs. Grade",
       shade = TRUE, legend = TRUE)

# Mosaic plot for ATTEND and GRADE
mosaic(~ ATTEND + GRADE, data = decoded_data,
       main = "Mosaic Plot: Attendance vs. Grade",
       shade = TRUE, legend = TRUE)

#-------------------------------------------------------------------------------
# NP069469
#-------------------------------------------------------------------------------
# Question 4: Do students receiving scholarships have significantly higher GPAs compared to those who do not receive scholarships?
#Analysis 4-1: To find the mean and standard deviation of GPA for each level of scholarship
Scholarships_Statistics <- student_data %>% 
  group_by(SCHOLARSHIP) %>% 
  summarize(
    mean_GPA = mean(CUML_GPA, na.rm = TRUE),
    sd_GPA=sd(CUML_GPA,na.rm=TRUE)) 

print(Scholarships_Statistics)

#Analysis 4-2: To conduct hypothesis testing

# Filtering the data for the two scholarship levels (0% and 100%)
filtered_data <- student_data %>%
  filter(SCHOLARSHIP %in% c(1, 5))

# Performing the t-test comparing the GPA of students with 0% and 100% scholarships

#Null Hypothesis         : There is no difference in mean GPA between students with 0% and 100% scholarships.
#Alternative Hypothesis  : There is a difference in mean GPA between students with 0% and 100% scholarships.

result_of_ttest <- t.test(CUML_GPA ~ SCHOLARSHIP, data = filtered_data)
print(result_of_ttest)

# Question 5: What kind of impact can be seen on the cumulative GPA of students from different high school type?
#Analysis 5-1: To see the impact from density plot along with the histogram

# typecasting high school type as factor
student_data$HS_TYPE <- factor(student_data$HS_TYPE)

# Plotting the density plot along with histogram using after_stat(density)
ggplot(student_data, aes(x = CUML_GPA)) +
  geom_histogram(aes(y = after_stat(density), fill = HS_TYPE), alpha = 0.5, bins = 3) +
  geom_density(aes(fill = HS_TYPE), alpha = 0.7) +
  facet_wrap(~ HS_TYPE) +
  theme_minimal() +
  labs(title = "Cumulative GPA Distribution by High School Type",
       x = "Cumulative GPA",
       y = "Density",
       fill = "High School Type") 


# Question 6: How the note-taking, listening, and classroom environment affect the cumulative GPA of a student?
#Analysis 6-1:Linear model where CUNL_GPA is dependent variable and NOTES, LISTENS and CLASSROOM are independent variable 
model <- lm(CUML_GPA ~ NOTES + LISTENS + CLASSROOM, student_data)
summary(model)

# Summary of the model
summary(model)

#Analysis 6-2: to visualize the linear regression of analysis 1, creating an effect plot for all three factors
effect_plot <- allEffects(model)
plot(effect_plot)


#-------------------------------------------------------------------------------
# NP069458
#-------------------------------------------------------------------------------

#Question 7: Effect of Having Kids on Academic Performance (Logistic Regression)
# Convert GRADE to binary (Pass/Fail)
decoded_data <- decoded_data %>%
  mutate(GRADE_BIN = ifelse(GRADE %in% c("A", "B", "C", "Pass"), 1, 0))

# Logistic Regression
model_logit <- glm(GRADE_BIN ~ KIDS, data = decoded_data, family = "binomial")
summary(model_logit)

# Create a bar plot showing the proportion of Pass/Fail
#x: Sets the x-axis to represent the categories of KIDS (None, One, More than 1).
#fill: Uses GRADE_BIN to fill the bars based on pass/fail status.
#geom_bar(position = "fill"):
# Creates a stacked bar plot where each bar is normalized to show the proportion of pass/fail within each KIDS category.
#labs(...):Adds titles and labels to the plot for clarity.
#theme_minimal(): Applies a clean theme to the plot.
#scale_fill_manual(values = c("0" = "red", "1" = "green")):
#Customizes colors for the binary outcome. Here, "0" (Fail) is red and "1" (Pass) is green.
ggplot(decoded_data, aes(x = factor(KIDS, levels = c("None", "One", "More than 1")), fill = factor(GRADE_BIN))) +
  geom_bar(position = "fill") +
  labs(
    title = "Bar Plot of Pass/Fail Proportions by Number of Kids",
    x = "Number of Kids",
    y = "Pass/Fail Status (1 = Pass, 0 = Fail)",
    fill = "Pass/Fail"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "red", "1" = "green"))  # Custom colors for Pass (1) and Fail (0)


#Question 8: What is the relationship between the number of siblings (X._SIBLINGS) 
#and the hours spent studying (STUDY_HRS), and how does this relationship vary by gender (GENDER)?
# Convert 'X._SIBLINGS' and 'STUDY_HRS' to numeric
decoded_data <- decoded_data %>%
  mutate(
    X._SIBLINGS = as.factor(X._SIBLINGS),
    STUDY_HRS = recode(STUDY_HRS, `less than 1` = 0.5, `1 to 3` = 2, `3 to 5` = 4, `5 to 7` = 6, `more than 7` = 8),
    STUDY_HRS = as.numeric(STUDY_HRS)
  )

# Check unique values and factor levels
summary(decoded_data$X._SIBLINGS)
summary(decoded_data$GENDER)
summary(decoded_data$STUDY_HRS)

# Ensure no empty factor levels
clean_data <- decoded_data %>%
  filter(!is.na(X._SIBLINGS) & !is.na(STUDY_HRS) & !is.na(GENDER))

# Convert 'GENDER' to factor
clean_data$GENDER <- as.factor(clean_data$GENDER)

# Perform ANOVA
anova_result <- aov(STUDY_HRS ~ X._SIBLINGS * GENDER, data = clean_data)

# Print the summary of ANOVA
summary(anova_result)


# Calculate mean study hours
mean_study_hours <- clean_data %>%
  group_by(X._SIBLINGS, GENDER) %>%
  summarise(mean_study_hrs = mean(STUDY_HRS, na.rm = TRUE))

# Plot interaction
ggplot(mean_study_hours, aes(x = X._SIBLINGS, y = mean_study_hrs, color = GENDER, group = GENDER)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Study Hours by Number of Siblings and Gender",
       x = "Number of Siblings",
       y = "Mean Hours Spent Studying") +
  theme_minimal()


#Question 9: Impact of Age Distribution on Likelihood of Receiving a Scholarship Considering High School Type
# Convert categorical variables to dummy variables
dummy_data <- model.matrix(~ AGE + HS_TYPE - 1, data = decoded_data)

# Extract scholarship as numeric
decoded_data$SCHOLARSHIP_NUM <- as.numeric(factor(decoded_data$SCHOLARSHIP, levels = c("None", "Partial", "Full", "Merit", "Need-based")))

# Combine with scholarship numeric values
final_data <- cbind(dummy_data, Scholarship = decoded_data$SCHOLARSHIP_NUM)

# Normalize the data
scaled_data <- scale(final_data)

# Perform PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# Plot PCA results
fviz_eig(pca_result)  # Plot the variance explained by each principal component

# Plot the PCA scores
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Cluster <- decoded_data$Cluster
pca_scores$AGE <- decoded_data$AGE
pca_scores$SCHOLARSHIP <- decoded_data$SCHOLARSHIP

# Plot PCA scores with respect to age
ggplot(pca_scores, aes(x = PC1, y = PC2, color = AGE)) +
  geom_point() +
  labs(title = "PCA Scores by Age", x = "Principal Component 1", y = "Principal Component 2")

# Plot PCA scores with respect to scholarship
ggplot(pca_scores, aes(x = PC1, y = PC2, color = SCHOLARSHIP)) +
  geom_point() +
  labs(title = "PCA Scores by Scholarship Type", x = "Principal Component 1", y = "Principal Component 2")

#-------------------------------------------------------------------------------
# NP069494
#-------------------------------------------------------------------------------
# Question 10: What are the effects of extracurricular activities and employment on students’ academic performance?
# Q1: A1: Effects of Extracurricular Activities on Academic Performance
# Q1.A1.1: Distribution of Students Engaged in Extracurricular Activities
# 1.1.1.1: Number of students participating in extracurricular activities (ACTIVITY).

num_participating <- student_data %>%
  filter(ACTIVITY == "Yes") %>%
  summarise(Count = n())

print(num_participating)

num_not_participating <- student_data %>%
  filter(ACTIVITY == "No") %>%
  summarise(Count = n())

print(num_not_participating)
# 1.1.1.2: Gender-wise participation in extracurricular activities (GENDER, ACTIVITY).

# Sorting out gender groups of students
activity_gender_count_df <- student_data %>%
  group_by(ACTIVITY, GENDER) %>%
  summarize(count = n(), .groups = 'drop')

print("Intermediate Gender Count DataFrame:")
print(activity_gender_count_df)

activity_gender_count_df <- activity_gender_count_df %>%
  group_by(ACTIVITY) %>%
  mutate(percent = (count / sum(count) * 100))

print("Intermediate Gender Percent DataFrame:")
print(activity_gender_count_df)

# Total number of students in the data set 
total_students <- sum(activity_gender_count_df$count)
print(paste('There are', total_students, 'students in the dataset'))

# Plotting gender distribution across activities
ggplot(activity_gender_count_df, aes(x = ACTIVITY, y = count, fill = GENDER)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%d (%.1f%%)", count, percent)),
            position = position_dodge(width = 1.5), vjust = -0.3, size = 3.5) +  
  labs(title = "Gender Distribution Across Extracurricular Activities",
       y = "Count",
       x = "Activity",
       fill = "Gender") +
  theme_minimal() +
  coord_cartesian(clip='off')

# Q1.A1.2: Impact of Extracurricular Activities on GPA

# 1.2.1: Comparing CUML_GPA of students involved in 
#          extracurricular activities vs. those who are not (CUML_GPA, ACTIVITY).

gpa_mapping <- list(
  "<2.0" = 1.5,
  "2.0-2.5" = 2.25,
  "2.5-3.0" = 2.75,
  "3.0-3.5" = 3.25,
  ">3.5" = 3.75
)

# Forming the data set to work upon in analysis 1 sub-analysis 2
data <- decoded_data %>%
  mutate(
    cuml_gpa = as.numeric(recode(CUML_GPA, !!!gpa_mapping)),
    exp_gpa = as.numeric(recode(EXP_GPA, !!!gpa_mapping)),
    activity = ACTIVITY
  ) %>%
  dplyr::select(cuml_gpa, exp_gpa, activity)

# View the resulting data frame
View(data)

# Statistics for CUML_GPA based on participation in extracurricular activities
cuml_gpa_comparison <- data %>%
  group_by(activity) %>%
  summarize(
    mean_gpa = mean(cuml_gpa, na.rm = TRUE),
    sd_gpa = sd(cuml_gpa, na.rm = TRUE),
    median_gpa = median(cuml_gpa, na.rm = TRUE),
    n = n(),  # Number of students in each group
    .groups = 'drop'
  )

print(cuml_gpa_comparison)


# Plotting mean GPA with error bars for standard deviation
ggplot(cuml_gpa_comparison, aes(x = activity, y = mean_gpa, fill = activity)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_gpa - sd_gpa, ymax = mean_gpa + sd_gpa), width = 0.2) +
  labs(title = "Mean CUML_GPA with Standard Deviation by Activity",
       x = "Extracurricular Activity",
       y = "Mean CUML_GPA",
       fill = "Activity") +
  scale_fill_manual(values = c("No" = "#FFB6C1", "Yes" = "#90EE90")) +
  theme_minimal()

# 1.2.2: Comparing EXP_GPA of students involved in 
#          extracurricular activities vs. those who are not (EXP_GPA, ACTIVITY).

# Summary statistics for EXP_GPA based on participation in extracurricular activities
exp_gpa_comparison <- data %>%
  group_by(activity) %>%
  summarize(
    mean_gpa = mean(exp_gpa, na.rm = TRUE),
    sd_gpa = sd(exp_gpa, na.rm = TRUE),
    median_gpa = median(exp_gpa, na.rm = TRUE),
    n = n(),  # Number of students in each group
    .groups = 'drop'
  )

print(exp_gpa_comparison)

# Plot mean GPA with error bars for standard deviation
ggplot(exp_gpa_comparison, aes(x = activity, y = mean_gpa, fill = activity)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_gpa - sd_gpa, ymax = mean_gpa + sd_gpa), width = 0.2) +
  labs(title = "Mean CUML_GPA with Standard Deviation by Activity",
       x = "Extracurricular Activity",
       y = "Mean CUML_GPA",
       fill = "Activity") +
  scale_fill_manual(values = c("No" = "#32CD32", "Yes" = "#DB8780")) +
  theme_minimal()

# Q1: A2: Distribution of Working Students
# 1.2.1: Number of students engaged in employment (WORK).
num_working <- student_data %>%
  filter(WORK == "Yes") %>%
  summarise(Count = n())

print(num_working)

num_not_working <- student_data %>%
  filter(WORK == "No") %>%
  summarise(Count = n())

print(num_not_working)

# 1.2.2: Age-wise distribution of employed students (AGE, WORK)

# Age distribution across extracurricular activities
work_age_count_df <- student_data %>%
  group_by(WORK, AGE) %>%
  summarize(count = n(), .groups = 'drop')

print("Intermediate Age Count DataFrame:")
print(work_age_count_df)

work_age_count_df <- work_age_count_df %>%
  group_by(WORK) %>%
  mutate(percent = (count / sum(count) * 100))

print("Intermediate Age Percent DataFrame:")
print(work_age_count_df)

ggplot(work_age_count_df, aes(x = WORK, y = count, fill = AGE)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%d (%.1f%%)", count, percent)),
            position = position_dodge(width = 1.5), vjust = -0.3, size = 3.5) +  
  labs(title = "Age Distribution Across Employment",
       y = "Count",
       x = "Work",
       fill = "Age") +
  theme_minimal() +
  coord_cartesian(clip='off')
# Q1: A3: Effects of Extracurricular Activities and Employment

# Q1: A3: 1: Overall Impact on GPA

# 1.3.1.1: The relationship between employment (WORK) and students’ 
#         cumulative GPA (CUML_GPA) and expected GPA (EXP_GPA).
a3_data <- decoded_data %>%
  mutate(
    cuml_gpa = as.numeric(recode(CUML_GPA, !!!gpa_mapping)),
    exp_gpa = as.numeric(recode(EXP_GPA, !!!gpa_mapping))
  ) %>%
  select(cuml_gpa, exp_gpa, WORK, ACTIVITY)
View(a3_data)

fit1 <- lm(student_data$WORK~ cuml_gpa * exp_gpa, data= a3_data)
summary(fit1)

# Ensure 'activity' is a factor with consistent levels
a3_data$WORK <- factor(student_data$WORK, levels = c(1, 2), labels = c("Yes", "No"))

# Define the color palette
work_colors <- c("Yes" = "#90EE90", "No" = "#FFB6C1")

ggplot(a3_data, aes(x = exp_gpa, y = cuml_gpa, color = factor(WORK))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = factor(WORK)), linewidth = 1) +
  labs(
    title = "Effects of Employment on Academic Performance",
    x = "Expected GPA (EXP_GPA)",
    y = "Cumulative GPA (CUML_GPA)",
    color = "Employment"
  ) +
  scale_color_manual(values = work_colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size=8)
)
# 1.3.1.2: The relationship between activity (ACTIVITY) and students’ 
#         cumulative GPA (CUML_GPA) and expected GPA (EXP_GPA).
fit2 <- lm(student_data$ACTIVITY~ cuml_gpa * exp_gpa, data= a3_data) 

summary(fit2)

# Ensure 'activity' is a factor with consistent levels
a3_data$activity <- factor(student_data$ACTIVITY, levels = c(1, 2), labels = c("No", "Yes"))

# Define the color palette
activity_colors <- c("Yes" = "red", "No" = "blue")

ggplot(a3_data, aes(x = exp_gpa, y = cuml_gpa, color = factor(activity))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = factor(activity)), linewidth = 1) +
  labs(
    title = "Effects of Extracurricular Activities on Academic Performance",
    x = "Expected GPA (EXP_GPA)",
    y = "Cumulative GPA (CUML_GPA)",
    color = "Extracurricular Activity"
  ) +
  scale_color_manual(values = activity_colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

#Question 11: How do transportation methods (TRANSPORT) 
#                       and living arrangements (LIVING) influence 
#                       the amount of time students spend preparing 
#                       for the exams (PREP_EXAM)?

# Convertting the categorical variables to factors
q2_data  <- decoded_data %>%
  mutate(
    TRANSPORT = as.factor(TRANSPORT),
    LIVING = as.factor(LIVING),
    PREP_EXAM = as.factor(PREP_EXAM)
  )

# Contingency table for TRANSPORT vs PREP_EXAM
transport_prep_exam_table <- table(q2_data$TRANSPORT, q2_data$PREP_EXAM)

# Chi-Square Test of Independence
chi_square_result <- chisq.test(transport_prep_exam_table)
print(chi_square_result)

# Contingency table for LIVING vs PREP_EXAM
living_prep_exam_table <- table(decoded_data$LIVING, decoded_data$PREP_EXAM)

# Chi-Square Test of Independence
chi_square_result_living <- chisq.test(living_prep_exam_table)
print(chi_square_result_living)

# Heat map for TRANSPORT vs PREP_EXAM
heatmap_data_transport <- as.data.frame(transport_prep_exam_table)
names(heatmap_data_transport) <- c("Transport", "Prep_Exam", "Count")

# Heat map
ggplot(heatmap_data_transport, aes(x = Transport, y = Prep_Exam, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap of Transportation Methods vs Exam Preparation",
       x = "Transportation Method",
       y = "Exam Preparation Level") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
  )

# Heat map for LIVING vs PREP_EXAM
heatmap_data_living <- as.data.frame(living_prep_exam_table)
names(heatmap_data_living) <- c("Living", "Prep_Exam", "Count")

# Heat map
ggplot(heatmap_data_living, aes(x = Living, y = Prep_Exam, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap of Living Arrangements vs Exam Preparation",
       x = "Living Arrangement",
       y = "Exam Preparation Level") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
  )
# Question 12: How does living with family and family size 
#influence student engagement in discussions and extracurricular activities?
# Convert categorical variables to factors
student_data <- student_data %>%
  mutate(
    ACTIVITY = factor(ACTIVITY),
    LIKES_DISCUSS = factor(LIKES_DISCUSS),
    LIVING = factor(LIVING),
    X._SIBLINGS = factor(X._SIBLINGS),
    KIDS = factor(KIDS)
  )

# Fit the MANCOVA model
mancova_model <- manova(cbind(ACTIVITY, LIKES_DISCUSS) ~ LIVING + 
                          X._SIBLINGS + KIDS + CUML_GPA + EXP_GPA, data = student_data)

# Summary of the MANCOVA model
summary(mancova_model)

# Long format for faceting
long_data <- student_data %>%
  pivot_longer(cols = c(ACTIVITY, LIKES_DISCUSS),
               names_to = "Engagement_Type",
               values_to = "Engagement_Level")

# Facet grid bar plot
ggplot(long_data, aes(x = LIVING, y = Engagement_Level, fill = LIVING)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Engagement_Type, scales = "free_y") +
  labs(title = "Engagement Levels by Living Situation",
       x = "Living Situation",
       y = "Engagement Level",
       fill = "Living Situation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------

# Project Gantt Chart

# Tasks
tasks <- tibble(
  task = c(
    "Project Planning and Team Meetings",
    "Data Understanding and Initial Exploration",
    "Data Cleaning and Pre-processing",
    "Data Transformation and Manipulation",
    "Exploratory Data Analysis (EDA)",
    "Applying Data Analysis Techniques",
    "Advanced Data Analysis and Visualization",
    "Interpreting Results and Drawing Conclusions",
    "Report Writing and Documentation",
    "Final Review and Submission Preparation"
  ),
  start_date = as.Date(c(
    "2024-05-08", "2024-05-13", "2024-05-20", "2024-05-31", "2024-06-11", 
    "2024-06-21", "2024-07-06", "2024-07-16", "2024-07-21", "2024-07-28"
  )),
  end_date = as.Date(c(
    "2024-05-12", "2024-05-19", "2024-05-30", "2024-06-10", "2024-06-20", 
    "2024-07-05", "2024-07-15", "2024-07-20", "2024-07-27", "2024-07-30"
  ))
)

# Gantt chart
ggplot(tasks, aes(x = start_date, xend = end_date, y = reorder(task, desc(task)), 
                  yend = task)) +
  geom_segment(size = 6, color = "skyblue") +
  labs(title = "Project Gantt Chart", x = "Date", y = "Task") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%Y") +
  theme_minimal() +
  theme_light()+
  theme(
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8)
  )

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Move Field Trial Analysis Plan and Sample Tables 
#Daniela Luque-Sanchez
#06/15/2024


#Goal is to describe baseline demographic data for n=9 participants who started the study. 

#Install Packages if not already installed
#install.packages("tidyverse")
#install.packages("table1")
#install.packages("gtsummary")
#install.packages("kableExtra")

#Load packages and data
library(tidyverse)
library(table1)
library(gtsummary)
library(kableExtra)

REDCap <- read.csv("C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/DataRaw/REDCap/MoveFieldTrial-MoveFieldTrialDataPu_DATA_2024-05-21_1559.csv")

##### 
### Table 1 ###
#Created 1 column for race
REDCap <- REDCap %>% mutate(race = case_when(race___1 == 1 ~"1",
                                             race___2 == 1 ~"2",
                                             race___3 == 1 ~"3",
                                             race___4 == 1 ~"4",
                                             race___5 == 1 ~"5",
                                             race___6 == 1 ~"6",
                                             race___7 == 1 ~"7",
                                             race___8 == 1 ~"8")
)

#Filter by columns of interest in Table 1
REDCapBaseline <- REDCap%>% filter(redcap_event_name == "baseline_arm_1") %>% 
  select(record_id,avg_weight, bmi, wc_average, avg_systolic, avg_diastolic)


REDCapScreening <- REDCap%>% filter(redcap_event_name == "screening_arm_1") %>% 
  select(age, gender, sex, race, ethnicity, income, marital_status)


REDCapTable1 <- cbind(REDCapBaseline, REDCapScreening)


REDCapTable1$race <- factor(REDCapTable1$race, levels = c(1,2,3,4,5),
                            labels = c("American Indian or Alaska Native", "Asian", "Black or African American",
                                       "Native Hawaiian or other Pacific Islander", "White"))


REDCapTable1$gender <- factor(REDCapTable1$gender, levels=c(0,1),
                              labels=c("Woman", "Man"))


REDCapTable1$sex <- factor(REDCapTable1$sex, levels = c(0,1),
                           labels=c("Female", "Male"))
REDCapTable1$ethnicity <- factor(REDCapTable1$ethnicity, levels = c(1,2,3,4),
                                 labels = c("Hispanic", "Latino", "Spanish", "Non-Hispanic"))
REDCapTable1$income <- factor (REDCapTable1$income, levels=c(0, 1, 2, 3, 4),
                               labels=c("Less than $25,000", "$25,000-$44,999","$45,001-$69,999",
                                        "$70,000-$110,000", "More than $110,000"))
REDCapTable1$marital_status <- factor(REDCapTable1$marital_status, levels=c(1,2,4,5),
                                      labels=c("Married", "Divorced","Separated", 
                                               "Never Married"))


#Labeling Continuous Variables
label(REDCapTable1$age) <- "Age"
units(REDCapTable1$age) <- "y"


label(REDCapTable1$avg_weight) <- "Weight"
units(REDCapTable1$avg_weight) <- "kg"


label(REDCapTable1$bmi) <- "BMI"
units(REDCapTable1$bmi) <- "kg/m2"


label(REDCapTable1$wc_average) <- "Waist Circumference"
units(REDCapTable1$wc_average) <- "cm"


label(REDCapTable1$avg_systolic) <- "Systolic Blood Pressure"
units(REDCapTable1$avg_systolic) <- "mm/Hg"


label(REDCapTable1$avg_diastolic) <- "Diastolic Blood Pressure"
units(REDCapTable1$avg_diastolic) <- "mm/Hg"


#Labeling Categorical Variables
label(REDCapTable1$gender) <- "Gender"
label(REDCapTable1$sex) <- "Sex"
label(REDCapTable1$race) <- "Race"
label(REDCapTable1$ethnicity) <- "Ethnicity"
label(REDCapTable1$income) <- "Income"
label(REDCapTable1$marital_status) <- "Marital Status"


#Caption & Footnote for Table 1
caption <- "Table 1. Baseline Characteristics of Study Population ᵃ"
footnote <- "ᵃ Variables analyzed using descriptive statistics; Abbreviations are as follows: Body Mass Index (BMI)."


#Create Table 1
Table1<- table1(~age + avg_weight+ bmi + wc_average + avg_systolic + avg_diastolic + 
                  gender+ sex+ race + ethnicity + income + marital_status, data=REDCapTable1,
                caption=caption, footnote=footnote,  overall="All Study Participants", topclass="Rtable1-zebra"
)
#Print Table1
print(Table1)

##### 
### Table 2 ###


#Filter data to create Table2
REDCapTable2<- REDCap%>% filter(redcap_event_name == "week_12_arm_1") %>% 
  select(record_id, aim_class_score, aim_ind_score, aim_gis_score, aim_fitness_p_score,fim_class_score,
         fim_ind_score, fim_gis_score, fim_fitness_p_score, iam_class_score_v2, iam_ind_score, 
         iam_gis_score, iam_fitness_p_score, nps_p_class, nps_p_ind, nps_p_gis, nps_p_fitness_p )


# Create a function to calculate mean and 95% CI using t.test
calc_summary <- function(x) {
  t_result <- t.test(x, conf.level = 0.95)
  mean_x <- t_result$estimate
  ci_lower <- t_result$conf.int[1]
  ci_upper <- t_result$conf.int[2]
  summary <- sprintf("%.2f (%.2f, %.2f)", mean_x, ci_lower, ci_upper)
  return(summary)
}


# Calculate NPS
calc_nps <- function(x) {
  promoters <- sum(x >= 9, na.rm = TRUE)
  detractors <- sum(x <= 6, na.rm = TRUE)
  total <- length(x)
  nps <- ((promoters / total) - (detractors / total)) * 100
  return(round(nps))
}


# Summarize the data
Table2 <- tibble(
  ` ` = c("Acceptability", "Feasibility", "Appropriateness", "Net Promoter Score"),
  `Group-based classes` = c(
    calc_summary(REDCapTable2$aim_class_score),
    calc_summary(REDCapTable2$fim_class_score),
    calc_summary(REDCapTable2$iam_class_score_v2),
    calc_nps(REDCapTable2$nps_p_class)
  ),
  `Individualized Support Sessions` = c(
    calc_summary(REDCapTable2$aim_ind_score),
    calc_summary(REDCapTable2$fim_ind_score),
    calc_summary(REDCapTable2$iam_ind_score),
    calc_nps(REDCapTable2$nps_p_ind)
  ),
  `Guided Imagery Sessions` = c(
    calc_summary(REDCapTable2$aim_gis_score),
    calc_summary(REDCapTable2$fim_gis_score),
    calc_summary(REDCapTable2$iam_gis_score),
    calc_nps(REDCapTable2$nps_p_gis)
  ),
  `Online Fitness Membership` = c(
    calc_summary(REDCapTable2$aim_fitness_p_score),
    calc_summary(REDCapTable2$fim_fitness_p_score),
    calc_summary(REDCapTable2$iam_fitness_p_score),
    calc_nps(REDCapTable2$nps_p_fitness_p)
  )
)


#Print Table 2
print(Table2)


#Export Table 2
write.csv(Table2, "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/REDCapTable2.csv", row.names = FALSE)


#####


### Table 3 ###


# Example data extraction and calculations
REDCapTable3 <- REDCap %>%
  filter(redcap_event_name %in% c("screening_arm_1", "baseline_arm_1")) %>%
  select(record_id, class_attend_pct_wk9, ex_support1, ex_support2, ex_support3)


# Set attendance for support sessions to 100% if attended 
REDCapTable3$ex_support1[REDCapTable3$ex_support1 == 1] <- 100
REDCapTable3$ex_support2[REDCapTable3$ex_support2 == 1] <- 100
REDCapTable3$ex_support3[REDCapTable3$ex_support3 == 1] <- 100


# Calculate average attendance
avg_REDCapTable3 <- mean(REDCapTable3$class_attend_pct_wk9, na.rm = TRUE)
avg_support1 <- mean(REDCapTable3$ex_support1, na.rm = TRUE)
avg_support2 <- mean(REDCapTable3$ex_support2, na.rm = TRUE)
avg_support3 <- mean(REDCapTable3$ex_support3, na.rm = TRUE)


# Assuming there were 3 support sessions offered
total_support_sessions <- 3
avg_total_support <- (avg_support1 + avg_support2 + avg_support3) / total_support_sessions


# Round to 1 decimal place and format as character with percent sign
avg_REDCapTable3 <- sprintf("%.1f%%", avg_REDCapTable3)
avg_support1 <- sprintf("%.1f%%", avg_support1)
avg_support2 <- sprintf("%.1f%%", avg_support2)
avg_support3 <- sprintf("%.1f%%", avg_support3)
avg_total_support <- sprintf("%.1f%%", avg_total_support)


# Create the summary table
Table3 <- data.frame(
  Type = c("Group-based Attendance", "Attendance for 1:1 Session 1", "Attendance for 1:1 Session 2", 
           "Attendance for 1:1 Session 3", "Total Attendance for 1:1 Support Sessions"),
  Average_Attendance = c(avg_REDCapTable3, avg_support1, avg_support2, avg_support3, avg_total_support)
)
Table3 <- Table3 %>% rename("Average Attendance" = Average_Attendance)


# Print the Attendance Table
print(Table3)


#Export Table 3
write.csv(Table3, "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/REDCapTable3.csv", row.names = FALSE)
#####


### Table 4 ###
REDCapTable4<- REDCap%>%
  filter(redcap_event_name=="baseline_arm_1" | redcap_event_name=="week_12_arm_1") %>%
  select(redcap_event_name,avg_weight, bmi, wc_average, avg_systolic, avg_diastolic, steptest_hr_recovery,
         vo2peak,gpaq_rec_avg_minperday, gpaq16_minperday, gpaq_totalmvpa_minperwk,
         gpaq_avgmvpa_minperday, gpaq_totalmod_metminwk, breq3_amot,
         breq3_ext, breq3_intj, breq3_ident, breq3_integ,
         breq3_intr, breq3_rai, hs_score, ident_exmean, ident_exmismatch,
         wbis_score, frs_score, vmiq_int_score, vmiq_kin_score,
         ivi_avg, evi_avg, ki_avg)


#Table4 Data Frame

# Function to calculate mean, 95% CI, effect size, and change
calculate_stats <- function(data, variable) {
  baseline <- data %>% filter(redcap_event_name == "baseline_arm_1") %>% pull(!!sym(variable))
  week12 <- data %>% filter(redcap_event_name == "week_12_arm_1") %>% pull(!!sym(variable))
  
  # Calculate means
  mean_baseline <- mean(baseline, na.rm = TRUE)
  mean_week12 <- mean(week12, na.rm = TRUE)
  
  # Calculate 95% CI
  ci_baseline <- mean_ci(baseline)
  ci_week12 <- mean_ci(week12)
  
  # Calculate effect size
  mean_diff <- mean_week12 - mean_baseline
  pooled_sd <- sqrt((sd(baseline, na.rm = TRUE)^2 + sd(week12, na.rm = TRUE)^2) / 2)
  effect_size <- mean_diff / pooled_sd
  
  # Calculate change and its CI
  change_mean <- mean_week12 - mean_baseline
  change_ci_lower <- ci_week12[1] - ci_baseline[2]  # Lower CI of change
  change_ci_upper <- ci_week12[2] - ci_baseline[1]  # Upper CI of change
  
  return(data.frame(
    Characteristic = variable,
    baseline_arm_1 = sprintf("%.1f (%.1f, %.1f)", mean_baseline, ci_baseline[1], ci_baseline[2]),
    week_12_arm_1 = sprintf("%.1f (%.1f, %.1f)", mean_week12, ci_week12[1], ci_week12[2]),
    Change = sprintf("%.1f (%.1f, %.1f)", change_mean, change_ci_lower, change_ci_upper),
    Effect_Size = round(effect_size, 3)  # Effect size now is the last column
  ))
}

# Mean and CI calculation function (remains unchanged)
mean_ci <- function(x) {
  n <- length(x)
  mean_val <- mean(x, na.rm = TRUE)
  stderr <- sd(x, na.rm = TRUE) / sqrt(n)
  ci <- qt(0.975, df = n - 1) * stderr
  return(c(mean_val - ci, mean_val + ci))
}

# List of variables
variables <- c("avg_weight", "bmi", "wc_average", "avg_systolic", "avg_diastolic", 
               "steptest_hr_recovery", "vo2peak", "gpaq_rec_avg_minperday", 
               "gpaq16_minperday", "gpaq_totalmvpa_minperwk", "gpaq_avgmvpa_minperday", 
               "gpaq_totalmod_metminwk", "breq3_amot", "breq3_ext", "breq3_intj", 
               "breq3_ident", "breq3_integ", "breq3_intr", "breq3_rai", 
               "hs_score", "ident_exmean", "ident_exmismatch", "wbis_score", 
               "frs_score", "vmiq_int_score", "vmiq_kin_score", "ivi_avg", 
               "evi_avg", "ki_avg")


# Calculate stats for all variables and combine into a single data frame
Table4 <- do.call(rbind, lapply(variables, function(var) calculate_stats(REDCapTable4, var)))

# Print the final results
print(Table4)
new_vars <- c("Weight (kg)", "BMI (kg/m2)", "Waist Circumference (cm)", "Systolic Blood Pressure (mm/Hg)", 
              "Diastolic Blood Pressure (mm/Hg)", "3-Minute Step Test, Recovery Heart Rate (bpm)", 
              "3-Minutes Step Test, V02 Peak Estimate (ml/kg/min)", "Recreational Activity", "Sedentary Time", 
              "Total MVPA (min/wk)", "Total MVPA (min/d)", "Total MET-minutes", "Amotivation", 
              "External Regulation", "Introjected Regulation", "Identified Regulation", "Integrated Regulation", 
              "Intrinsic Regulation", "Relative Autonomy Index", "Exercise Habit Automaticity", "Identity", 
              "Identity Mismatch", "Internalized Weight Bias", "Figure Rating Scale Score", 
              "Internal Visual Imagery Score", "Kinaesthetic Imagery Score", "Internal Visual Imagery Score", 
              "External Visual Imagery Score", "Kinesthetic Imagery Score")
# Rename the variables in Table4 Characteristic Column
Table4$Characteristic <- new_vars
# Define the category headers and their insertion points
category_headers <- c("Anthropometric Measures", "Cardiovascular Measures", "Self-Reported Physical Activity", "Motivation for Exercise",
                      "Exercise Identity", "Weight Bias", "Imagery Scores")
insertion_points <- c(1, 3, 8, 13, 21, 23, 25 )  # Example row indices to insert headers

# Initialize an empty list to hold rows
rows <- list()

# Loop through each variable and insert category headers at specified points
current_row <- 1
for (i in seq_along(variables)) {
  if (current_row %in% insertion_points) {
    rows[[length(rows) + 1]] <- data.frame(
      Characteristic = category_headers[which(insertion_points == current_row)],
      baseline_arm_1 = " ",
      week_12_arm_1 = " ",
      Change = " ",
      Effect_Size = " ",
      stringsAsFactors = FALSE
    )
  }
  
  # Add the calculated stats row
  rows[[length(rows) + 1]] <- Table4[i, ]
  current_row <- current_row + 1
}

# Combine all rows into a new Table4
Table4_with_headers <- do.call(rbind, rows)

# Print the updated Table4 with category headers
print(Table4_with_headers)
#Rename Column Names
Table4_Final <- Table4_with_headers %>% rename("Baseline" = baseline_arm_1, "Week 12" = week_12_arm_1, 
                                               "Change (Week 12 – Baseline)" = Change, "Effect Size" = Effect_Size, "Outcome" = Characteristic)
# Create the table using kable and kableExtra without row names
Table4_kable <- Table4_Final %>%
  kable("html", escape = FALSE, row.names = FALSE, bordered = FALSE) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = TRUE, extra_css = "border: 2px solid black;") %>%  # Bold header row
  row_spec(c(1, 4, 10, 16, 24, 25, 28, 31), bold = TRUE) %>% # Bold specific category header rows
  row_spec(c(1, 4, 10, 16, 24, 25, 28, 31), extra_css = "border: 2px solid black; border-bottom: none;") %>%  # Full border for row 1
  row_spec(c(2:8, 11:14, 17:22, 26, 29, 32:35 ), extra_css = "border-left: 2px solid black; border-right: 2px solid black; border-top: none; border-bottom: none;") %>%  # Left and right for row 2
  row_spec(c(3, 9, 15, 23, 24, 27, 30, 36), extra_css = "border: 2px solid black; border-top: none;")  # Full border for row 3

# Print the formatted table
print(Table4_kable)

#Export Table4

# Save the table as an HTML file to a specific path
save_kable(Table4_kable, file = "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/Table4.html")

###Table 5

# Mean and CI calculation function for specified variables
mean_ci <- function(x) {
  n <- length(x)
  mean_val <- mean(x, na.rm = TRUE)
  stderr <- sd(x, na.rm = TRUE) / sqrt(n)
  ci <- qt(0.975, df = n - 1) * stderr
  return(c(mean_val - ci, mean_val + ci))
}

# Function to calculate mean, 95% CI, and range for variables
calculate_psych_needs <- function(data, variable) {
  week12 <- data %>% filter(redcap_event_name == "week_12_arm_1") %>% pull(!!sym(variable))
  
  # Calculate means and confidence intervals
  mean_val <- mean(week12, na.rm = TRUE)
  ci_val <- mean_ci(week12)
  
  # Calculate range
  range_val <- range(week12, na.rm = TRUE)
  
  return(data.frame(
    Variable = variable,
    Mean_95_CI = sprintf("%.2f (%.2f, %.2f)", mean_val, ci_val[1], ci_val[2]),
    Range = sprintf("%.2f - %.2f", range_val[1], range_val[2])
  ))
}

# List of variables
psych_needs_variables <- c("auto_sat", "auto_frus", "comp_sat", "comp_frus", "relate_sat", "relate_frus")

# Calculate stats for all variables and combine into a single data frame
Table5 <- do.call(rbind, lapply(psych_needs_variables, function(var) calculate_psych_needs(REDCap, var)))
t5_new_vars <- c("Autonomy Satisfaction Average", 
                 "Autonomy Frustration Average", 
                 "Competence Satisfaction Average", 
                 "Competence Frustration Average", 
                 "Relatedness Satisfaction Average", 
                 "Relatedness Frustration Average")

# Update Column Names in Table5
Table5$Variable <- t5_new_vars
Table5 <- Table5 %>% rename("Mean (95% CI)" = Mean_95_CI)

# Create the kable table from Table5 with a bold caption and outer border
Table5_kable <- Table5 %>%
  kable("html", escape = FALSE, row.names = FALSE, 
        caption = "<span style='font-weight: bold; color: black;'>Summary of Basic Psychological Needs Survey Data</span>",
        table.attr = "style='border: 2px solid black; border-collapse: collapse;'") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = TRUE)  # Make header row bold

# Print the kable table
print(Table5_kable)

# Save the kable Table5 as an HTML file
save_kable(Table5_kable, file = "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/Table5.html")

###Table6
# Define the variable names
eos_variables <- c("eos_1", "eos_2", "eos_3", "eos_4", "eos_5", "eos_6", 
                   "eos_7", "eos_9", "eos_10", "eos_11", 
                   "eos_12", "eos_13")

# Create a new dataset with only the specified columns
End_of_Study_Survey <- REDCap %>%
  select(all_of(eos_variables))
# Function to calculate mean, 95% CI, and range
summarize_variable <- function(data, variable) {
  values <- data[[variable]]
  
  # Check if the variable is numeric
  if (!is.numeric(values)) {
    return(data.frame(
      Variable = variable,
      Mean_95_CI = NA,
      Range = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  mean_val <- mean(values, na.rm = TRUE)
  ci <- mean_ci(values)
  range_val <- range(values, na.rm = TRUE)
  
  return(data.frame(
    Variable = variable,
    Mean_95_CI = sprintf("%.2f (%.2f, %.2f)", mean_val, ci[1], ci[2]),
    Range = sprintf("%.2f - %.2f", range_val[1], range_val[2]),
    stringsAsFactors = FALSE
  ))
}

# Function to calculate 95% CI
mean_ci <- function(x) {
  n <- length(x)
  mean_val <- mean(x, na.rm = TRUE)
  stderr <- sd(x, na.rm = TRUE) / sqrt(n)
  ci <- qt(0.975, df = n - 1) * stderr
  return(c(mean_val - ci, mean_val + ci))
}
#New Variables for Table6
t6_new_vars <- c(
  "1. Please rate the level of support you received from your family and friends for your participation in this program:",
  "2. The 3, group-based classes:",
  "3. The 3, individualized support sessions:",
  "4. The mental guided imagery sessions:",
  "5. The Peloton app:",
  "6. The Move Journal:",
  "7. The weekly newsletters:",
  "8. Would you say the length of the group-based classes were:",
  "9. Would you say the length of the individualized support sessions were:",
  "10. Would you say the length of the program (12 weeks) was:",
  "11. Over the past 12 weeks, did you purchase anything (tennis shoes, headphones, water bottle, yoga mat, etc.) to support your participation in the Move program?",
  "12. How confident do you feel about your ability to engage in 150 minutes/week of exercise over the next 6 months?"
)
scales <- c("1 (Not at all supportive) to 5 (Very supportive)",
            "1 (Not at all helpful) to 5 (Very helpful)",
            "1 (Not at all helpful) to 5 (Very helpful)",
            "1 (Not at all helpful) to 5 (Very helpful)",
            "1 (Not at all helpful) to 5 (Very helpful)",
            "1 (Not at all helpful) to 5 (Very helpful)",
            "1 (Not at all helpful) to 5 (Very helpful)",
            "1 (Too short) to 5 (Too long)",
            "1 (Too short) to 5 (Too long)",
            "1 (Too short) to 5 (Too long)",
            "0 = No, 1 = Yes",
            "1 (Not at all confident) to 5 (Very confident)"
            
            
            
)
# Summarize data for all variables
Table6_summary <- do.call(rbind, lapply(eos_variables, function(var) summarize_variable(End_of_Study_Survey, var)))
Table6_summary$Variable <- t6_new_vars
Table6_summary$Scale <- scales

# Reorder columns to have the Scale column as the second column
Table6_summary <- Table6_summary %>% 
  select(Variable, Scale, everything()) %>%
  rename(" " = Variable, "Mean (95% CI)" = Mean_95_CI)

# Convert to Kable and add styling
Table6 <- kable(Table6_summary, caption = "<span style='font-weight: bold; color: black;'>End of Study Survey Results</span>", format = "html", escape = FALSE) %>%
  kable_styling("striped", full_width = F, position = "center") %>%
  add_header_above(c(" " = 2, "Summary Statistics" = 2)) %>%
  column_spec(1, border_left = TRUE, bold = TRUE) %>%
  column_spec(4, border_right = TRUE) %>%
  row_spec(0, extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;") %>%
  row_spec(nrow(Table6_summary), extra_css = "border-bottom: 2px solid black;")

# Print the table
Table6
# Save the kable Table5 as an HTML file
save_kable(Table6, file = "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/Table6.html")

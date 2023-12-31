### Load Excel ###

library(readxl)
library(dplyr)
library(tibble)

responses <-  read_excel("C:/Users/m1012129/Downloads/responses.xlsx")

# Creating the table
significance <- tibble(
  question = character(), # Column 'question' as text
  answer = character(),  # Column 'answer' as text
  time1 = character(),   # Column 'time1' as text
  time2 = character(),   # Column 'time2' as text
  pvalue = double()     # Column 'pvalue' as a decimal
)

### find the latest data ###

time1 = "April 2023"
time2 = "April 2022"

latest <- responses %>%
  filter(time == time1)

previous <- responses %>%
  filter(time == time2)

answer1 = "Yes, I fully understand Defra's vision for farming"
question1 = "Do you know what Defra's vision means for farming?"
  
### Data ###
  
latest_question <- latest %>%
  filter(answer == answer1) # create a table of the latest time

previous_question <- previous %>%
  filter(answer == answer1) # create a table of the latest time

time_values <- unique(responses$time) # find all unique time names


# Group 1
n1 <- latest_question$total # Sample size for group 1
x1 <- latest_question$number  # Number of successes in group 1

# Group 2
n2 <- previous_question$total # Sample size for group 2
x2 <- previous_question$number  # Number of successes in group 2

# Performing the two-proportion z-test
# H0: p1 = p2 (the proportions are equal)
# H1: p1 ≠ p2 (the proportions are not equal)

### Manipulation ###

# Step 1: Compute the sample proportions
p1 <- x1 / n1
p2 <- x2 / n2

# Step 2: Compute the pooled sample proportion
p_pool <- (x1 + x2) / (n1 + n2)

# Step 3: Compute the standard error
SE <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))

# Step 4: Compute the test statistic
z <- (p1 - p2) / SE

# Step 5: Set the significance level (alpha)
alpha <- 0.05

# Step 6: Find the critical value for the test statistic
critical_value <- qnorm(1 - alpha / 2)

### Results ###

# Step 7: Compare the test statistic with the critical value
if (abs(z) > critical_value) {
  cat("Reject the null hypothesis. There is a significant difference between the proportions.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference between the proportions.\n")
}

# Step 8: Calculate the p-value
p_value <- 2 * (1 - pnorm(abs(z)))
cat("The p-value is:", p_value, "\n")

# Step 9: Add to the table

significance <- significance %>%
  add_row(question = question1, answer = answer1, time1 = time1, time2 = time2, pvalue = p_value)


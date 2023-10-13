# Example data

# Group 1
n1 <- 1418 # Sample size for group 1
x1 <- 142  # Number of successes in group 1

# Group 2
n2 <- 1223 # Sample size for group 2
x2 <- 86  # Number of successes in group 2

# Performing the two-proportion z-test
# H0: p1 = p2 (the proportions are equal)
# H1: p1 ≠ p2 (the proportions are not equal)

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

# Step 7: Compare the test statistic with the critical value
if (abs(z) > critical_value) {
  cat("Reject the null hypothesis. There is a significant difference between the proportions.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference between the proportions.\n")
}

# Step 8: Calculate the p-value
p_value <- 2 * (1 - pnorm(abs(z)))
cat("The p-value is:", p_value, "\n")

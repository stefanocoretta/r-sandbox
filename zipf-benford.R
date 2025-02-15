# Function to calculate the probability under Benford's Law for a given leading digit
benford_prob <- function(d) {
  log10(1 + 1 / d)
}

# Function to calculate the probability under Zipf's Law for a given rank
zipf_prob <- function(r, N, s) {
  zipf_normalizer <- sum(1 / (1:N)^s)
  (1 / r^s) / zipf_normalizer
}

# Function to find the leading digit d for a given rank r
find_leading_digit <- function(r, N, s) {
  # Calculate Zipf probability for rank r
  Pr <- zipf_prob(r, N, s)

  # Find the digit d (1 to 9) whose Benford probability is closest to Pr
  possible_digits <- 1:9
  benford_probs <- sapply(possible_digits, benford_prob)

  # Find the closest digit
  d <- possible_digits[which.min(abs(benford_probs - Pr))]

  return(d)
}

# Example parameters
N <- 1000  # Total number of elements
s <- 1     # Zipf exponent
r <- 5     # Example rank

# Find the leading digit for the given rank r
d_value <- find_leading_digit(r, N, s)

# Print the result
cat("The leading digit d for rank", r, "is:", d_value, "\n")

ds <- c()

for (i in 1:100) {
  ds <- c(ds, find_leading_digit(i, N, s))
}

plot(1:100, ds)

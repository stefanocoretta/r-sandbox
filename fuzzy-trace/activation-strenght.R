# Set the number of categories
numCategories <- 5

# Initialize memory traces and their activations
memoryTraces <- matrix(runif(numCategories * 3), nrow = numCategories)
memoryTraceActivations <- rep(1, numCategories)

# Function to categorize input based on memory traces activations
categorizeInput <- function(input) {
  # Calculate activation strengths based on similarity with memory traces
  activationStrengths <- rowSums(memoryTraces * input)

  # Assign input to the category with the highest activation strength
  category <- which.max(activationStrengths)

  # Update the memory trace activations
  memoryTraceActivations <<- memoryTraceActivations + (input - memoryTraceActivations) * 0.1

  # Return the assigned category
  return(category)
}

# Example usage
inputSeries <- matrix(runif(5 * 3), nrow = 5)  # Matrix of input values (5 inputs, 3 features each)

# Categorize each input in the series
for (i in 1:nrow(inputSeries)) {
  input <- inputSeries[i, ]
  category <- categorizeInput(input)
  cat("Input:", input, "\tCategory:", category, "\n")
}

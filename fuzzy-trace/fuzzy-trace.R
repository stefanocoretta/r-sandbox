fuzzy_trace <- function(memory_values, similarity_threshold = 0.97) {
  num_values <- length(memory_values)
  similarity_matrix <- matrix(0, nrow = num_values, ncol = num_values)

  # Calculate similarity between memory values
  for (i in 1:num_values) {
    for (j in 1:num_values) {
      if (i == j) {
        similarity_matrix[i, j] <- 1.0  # Same value, max similarity
      } else {
        similarity <- fuzzy_similarity(memory_values[[i]], memory_values[[j]])
        similarity_matrix[i, j] <- similarity
      }
    }
  }

  # Compute trace strength for each memory value
  trace_strengths <- rep(0, num_values)
  for (i in 1:num_values) {
    trace_strength <- 0.0
    for (j in 1:num_values) {
      if (similarity_matrix[i, j] >= similarity_threshold) {
        trace_strength <- trace_strength + similarity_matrix[i, j]
      }
    }
    trace_strengths[i] <- trace_strength
  }

  return(trace_strengths)
}

fuzzy_similarity <- function(value1, value2) {
  # Compute fuzzy similarity between two memory values
  similarity <- 0.0

  # Perform similarity computation (customize for your specific application)
  # You can use any similarity metric suitable for your data
  # Example: calculate similarity using cosine similarity
  dot_product <- sum(value1 * value2)
  norm1 <- sqrt(sum(value1^2))
  norm2 <- sqrt(sum(value2^2))
  similarity <- dot_product / (norm1 * norm2)

  return(similarity)
}

# Example usage
memory_values <- list(
  c(1, 2, 3),
  c(4, 5, 6),
  c(7, 8, 9)
)

memory_values <- list(
  c(1, 2, 3), c(100, 4500, 10000), c(10, 200, 300), c(1.5, 2.3, 3), c(1.5, 2, 3), c(1, 2.5, 3)
)

trace_strengths <- fuzzy_trace(memory_values)
trace_strengths

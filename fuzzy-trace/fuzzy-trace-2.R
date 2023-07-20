fuzzy_trace <- function(memory_values, similarity_threshold = 0.5) {
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

  # Compute gist memory and verbatim memory strengths
  gist_strengths <- rep(0, num_values)
  verbatim_strengths <- rep(0, num_values)

  for (i in 1:num_values) {
    gist_strength <- 0.0
    verbatim_strength <- 0.0

    for (j in 1:num_values) {
      if (i != j) {
        if (similarity_matrix[i, j] >= similarity_threshold) {
          verbatim_strength <- verbatim_strength + similarity_matrix[i, j]
        } else {
          gist_strength <- gist_strength + similarity_matrix[i, j]
        }
      }
    }

    gist_strengths[i] <- gist_strength
    verbatim_strengths[i] <- verbatim_strength
  }

  return(list(gist_strengths = gist_strengths, verbatim_strengths = verbatim_strengths))
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

memory_values1 <- list(
  c(1, 2, 3), c(1.5, 2.3, 3)
)

memory_values2 <- list(
  c(1, 2, 3), c(10, 200, 300), c(1.5, 2.3, 3)
)

trace_results <- fuzzy_trace(memory_values1, 0.97)
gist_strengths <- trace_results$gist_strengths
verbatim_strengths <- trace_results$verbatim_strengths

trace_results <- fuzzy_trace(memory_values2, 0.99)
gist_strengths <- trace_results$gist_strengths
verbatim_strengths <- trace_results$verbatim_strengths

print("Gist Strengths:", gist_strengths)
print("Verbatim Strengths:", verbatim_strengths)

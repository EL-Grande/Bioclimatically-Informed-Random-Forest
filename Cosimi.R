# Function to calculate cosine similarity
cosine_similarity <- function(vector1, vector2) {
  dot_product <- sum(vector1 * vector2)
  magnitude1 <- sqrt(sum(vector1^2))
  magnitude2 <- sqrt(sum(vector2^2))
  
  if (magnitude1 == 0 || magnitude2 == 0) {
    return(0)  # Avoid division by zero
  }
  
  similarity <- dot_product / (magnitude1 * magnitude2)
  return(similarity)
}



# Function to calculate Cosine Similarity (CS)
cosine_similarityB <- function(y_actual, y_predicted) {
  sum(y_actual * y_predicted) / (sqrt(sum(y_actual^2)) * sqrt(sum(y_predicted^2)))
}


# Example usage
vector_a <- c(1, 2, 3)
vector_b <- c(4, 5, 36)

similarity_score1 <- cosine_similarity(vector_a, vector_b)
similarity_score1

similarity_score2<-cosine_similarityB(vector_a, vector_b)
similarity_score2
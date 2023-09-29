### Axiell thesauri analysis

# Load required libraries
library(stringdist)
library(dplyr)
library(data.table)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri")

# Sample list of concepts
thesaurus <- fread("Thesarus-export(B&AC).csv")
setDT(thesaurus)
concepts <- thesaurus$term

# Function to find similar concepts
find_similar_concepts <- function(concepts, threshold = 0.75) {
  # Initialize an empty data frame to store results
  similar_concepts_df <- data.frame(concept1 = character(0), concept2 = character(0), similarity = numeric(0), stringsAsFactors = FALSE)
  
  # Iterate through each pair of concepts
  for (i in 1:length(concepts)) {
    for (j in (i+1):length(concepts)) {
      concept1 <- concepts[i]
      concept2 <- concepts[j]
      
      # Split compounded concepts into individual tokens
      tokens1 <- unlist(strsplit(concept1, " "))
      tokens2 <- unlist(strsplit(concept2, " "))
      
      # Calculate the Jaccard similarity between tokens
      similarity <- stringdist::stringdistmatrix(tokens1, tokens2, method = "jaccard")
      
      # Calculate the average similarity between tokens
      avg_similarity <- mean(similarity, na.rm = TRUE)  # Add na.rm = TRUE to handle missing values
      
      # If the average similarity is above the threshold and not NA, consider them similar
      if (!is.na(avg_similarity) && avg_similarity >= threshold) {
        similar_concepts_df <- bind_rows(similar_concepts_df, data.frame(concept1 = concept1, concept2 = concept2, similarity = avg_similarity))
      }
    }
  }
  
  return(similar_concepts_df)
}

# Find similar concepts with a threshold of 0.75
similar_concepts <- find_similar_concepts(concepts, threshold = 0.75)

# Print the result
print(similar_concepts)

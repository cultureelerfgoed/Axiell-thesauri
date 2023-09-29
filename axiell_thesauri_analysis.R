### Axiell thesauri analysis

# Load required libraries
library(stringdist)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri")

# Sample list of concepts
thesaurus <- fread("Thesarus-export(B&AC).csv")
setDT(thesaurus)
x <- thesaurus[sample(nrow(thesaurus), 10000), ]
concepts <- x$term


# Function to find similar concepts for a subset of concepts
find_similar_concepts_subset <- function(subset_concepts, all_concepts, threshold = 0.75) {
  # Initialize an empty data frame to store results
  similar_concepts_df <- data.frame(concept1 = character(0), concept2 = character(0), similarity = numeric(0), stringsAsFactors = FALSE)
  
  # Iterate through each pair of concepts in the subset
  for (i in 1:length(subset_concepts)) {
    for (j in seq_along(all_concepts)) {
      concept1 <- subset_concepts[i]
      concept2 <- all_concepts[j]
      
      # Split compounded concepts into individual tokens
      tokens1 <- unlist(strsplit(concept1, " "))
      tokens2 <- unlist(strsplit(concept2, " "))
      
      # Calculate the Jaccard similarity between tokens
      similarity <- stringdist::stringdistmatrix(tokens1, tokens2, method = "jaccard")
      
      # Calculate the average similarity between tokens
      avg_similarity <- mean(similarity, na.rm = TRUE)  # Add na.rm = TRUE to handle missing values
      
      # If the average similarity is above the threshold and not NA, consider them similar
      if (!is.na(avg_similarity) && avg_similarity >= threshold) {
        # Accumulate the results in a list
        similar_concepts_df <- list(similar_concepts_df, data.frame(concept1 = concept1, concept2 = concept2, similarity = avg_similarity))
      }
    }
  }
  
  return(similar_concepts_df)
}

# Set up parallel processing
num_cores <- 4  # You can adjust this to the number of cores you want to use
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Find similar concepts with a threshold of 0.75 in parallel
similar_concepts_list <- foreach(subset = concepts) %dopar% {
  find_similar_concepts_subset(subset, concepts, threshold = 0.75)
}

# Stop the parallel cluster
stopCluster(cl)

# Combine the results from the list
similar_concepts <- do.call(rbind, similar_concepts_list)

# Identify unique concepts and add a 'unique' variable
thesaurus$unique <- !thesaurus$term %in% unique(c(similar_concepts$concept1, similar_concepts$concept2))

# Print the updated data frame
head(thesaurus)

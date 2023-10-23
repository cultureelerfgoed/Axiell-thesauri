from fuzzywuzzy import fuzz
import csv
from tqdm import tqdm

# Define the path to the CSV file
csv_file = r"C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\thesaurus_onderwerp_matchingset.csv"

# Define the path to the output CSV file for matches
output_csv_file = r"C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\thesaurus_onderwerp_matches_run2.csv"

# Threshold similarity score for matches (70%)
threshold = 85

# List to store matching terms
matching_terms = []

# Open and read the CSV file
with open(csv_file, mode="r", encoding="utf-8") as file:
    reader = csv.DictReader(file)

    # Collect all terms from the CSV for comparison
    all_terms = [row["term"] for row in reader]

# Create a progress bar to track the script's progress
progress_bar = tqdm(total=len(all_terms) ** 2, unit="pairs", desc="Matching Progress")

# Calculate the Levenshtein-based similarity score between terms in the CSV
for term_in_csv in all_terms:
    for other_term in all_terms:
        if term_in_csv != other_term:  # Skip comparing a term to itself
            similarity_score = fuzz.ratio(term_in_csv, other_term)

            # Check if the similarity score is above the threshold
            if similarity_score >= threshold:
                matching_terms.append((term_in_csv, other_term, similarity_score))

            # Update the progress bar
            progress_bar.update(1)

# Close the progress bar
progress_bar.close()

# Write matching terms to the output CSV file
with open(output_csv_file, mode="w", encoding="utf-8", newline="") as output_file:
    fieldnames = ["Term in CSV", "Other Term in CSV", "Similarity Score"]
    writer = csv.DictWriter(output_file, fieldnames=fieldnames)
    writer.writeheader()

    for match in matching_terms:
        term_in_csv, other_term, similarity_score = match
        writer.writerow({
            "Term in CSV": term_in_csv,
            "Other Term in CSV": other_term,
            "Similarity Score": similarity_score
        })

print(f"Matching terms saved to {output_csv_file}")
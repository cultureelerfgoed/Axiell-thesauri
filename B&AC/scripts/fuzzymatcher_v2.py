import csv
from tqdm import tqdm
import Levenshtein

# Define the path to the CSV file
csv_file = r"C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\data\\thesaurus_onderwerp_matchingset.csv"

# Define the path to the output CSV file for matches
output_csv_file = r"C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\data\\thesaurus_onderwerp_matches_leven_80pc.csv"

# Minimum percentage match for matches (85%)
min_percentage_match = 80

# List to store matching terms
matching_terms = []

# Open and read the CSV file
with open(csv_file, mode="r", encoding="utf-8") as file:
    reader = csv.DictReader(file)

    # Collect all terms from the CSV for comparison
    all_terms = [row["term"] for row in reader]

# Create a progress bar to track the script's progress
progress_bar = tqdm(total=len(all_terms) ** 2, unit="pairs", desc="Matching Progress")

# Calculate the Levenshtein distance and percentage match between terms in the CSV
for term1 in all_terms:
    for term2 in all_terms:
        if term1 != term2:  # Skip comparing a term to itself
            distance = Levenshtein.distance(term1, term2)
            max_length = max(len(term1), len(term2))
            percentage_match = (1 - distance / max_length) * 100

            # Check if the percentage match is above the threshold
            if percentage_match >= min_percentage_match:
                matching_terms.append((term1, term2, percentage_match))

            # Update the progress bar
            progress_bar.update(1)

# Close the progress bar
progress_bar.close()

# Write matching terms to the output CSV file
with open(output_csv_file, mode="w", encoding="utf-8", newline="") as output_file:
    fieldnames = ["Term in CSV", "Other Term in CSV", "Percentage Match"]
    writer = csv.DictWriter(output_file, fieldnames=fieldnames, delimiter= "|")
    writer.writeheader()

    for match in matching_terms:
        term1, term2, percentage_match = match
        writer.writerow({
            "Term in CSV": term1,
            "Other Term in CSV": term2,
            "Percentage Match": percentage_match
        })

print(f"Matching terms saved to {output_csv_file}")


import spacy
import csv

# Load the spaCy model
nlp = spacy.load("nl_core_news_sm")

# Define the path to the CSV file
csv_file = r"C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\thesaurus_bouw.csv"

# Define the path to the output CSV file for matches
output_csv_file = r"C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\matches.csv"

# Threshold similarity score for matches
threshold = 0.80

# List to store matching terms
matching_terms = []

# Open and read the CSV file
with open(csv_file, mode="r", encoding="utf-8") as file:
    reader = csv.DictReader(file)

    # Collect all terms from the CSV for comparison
    all_terms = [row["term"] for row in reader]

# Calculate the similarity score between terms in the CSV
for term_in_csv in all_terms:
    for other_term in all_terms:
        if term_in_csv != other_term:  # Skip comparing a term to itself
            doc1 = nlp(term_in_csv)
            doc2 = nlp(other_term)
            similarity_score = doc1.similarity(doc2)

            # Check if the similarity score is above the threshold
            if similarity_score > threshold:
                matching_terms.append((term_in_csv, other_term, similarity_score))

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
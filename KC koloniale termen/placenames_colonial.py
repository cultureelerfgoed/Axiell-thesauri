import spacy
import geocoder
import csv
import os
from tqdm import tqdm

# Load spaCy model for NER in Dutch
nlp = spacy.load("nl_core_news_sm")

# Function to extract geo entities and update the original CSV
def process_csv(csv_file_path, geo_column_index):
    with open(csv_file_path, 'r', errors='ignore') as file:
        # Use csv.reader to handle CSV parsing
        csv_reader = csv.reader(file, delimiter=',')
        
        # Assuming the first line contains headers, modify if necessary
        headers = next(csv_reader)

        # Create a new CSV file to store the updated information
        updated_file_path = os.path.join(os.path.dirname(csv_file_path), 'geolocated_' + os.path.basename(csv_file_path))
        with open(updated_file_path, 'w', newline='') as updated_file:
            writer = csv.writer(updated_file, delimiter=';')

            # Add headers for original index and new information
            writer.writerow(["OriginalIndex"] + headers + ["IsInsideEurope"])

            for idx, values in tqdm(enumerate(csv_reader, start=1), desc="Processing CSV"):
                geo_text = values[geo_column_index]

                doc = nlp(geo_text)
                geo_entities = [ent.text for ent in doc.ents if ent.label_ == "GPE"]

                if geo_entities:  # Process only if there are GPE entities
                    is_inside_europe = False
                    for entity in geo_entities:
                        location = geocoder.osm(entity)
                        if location.ok:
                            europe_bounds = {"min_lat": 35.8, "max_lat": 71.7, "min_lon": -32.1, "max_lon": 39.3}
                            if (
                                europe_bounds["min_lat"] <= location.latlng[0] <= europe_bounds["max_lat"] and
                                europe_bounds["min_lon"] <= location.latlng[1] <= europe_bounds["max_lon"]
                            ):
                                is_inside_europe = True
                                break

                    # Append the information to the new row
                    writer.writerow([idx] + values + ["Inside Europe" if is_inside_europe else "Outside Europe"])
                else:
                    # If no GPE entities, write the original line without modification
                    writer.writerow([idx] + values)

# Replace 'your_csv_file.csv' with the actual path to your CSV file
# Replace 2 with the actual index of your geo column
process_csv("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\KC koloniale termen\\Termen_KC_clean.csv", 3)
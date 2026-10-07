import csv
import os

os.chdir("/Users/original_author/project")

with open("data.csv", newline="") as f:
    rows = list(csv.DictReader(f))
print(len(rows))

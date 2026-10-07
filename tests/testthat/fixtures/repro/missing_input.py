import csv

with open("not_in_repo.csv", newline="") as f:
    rows = list(csv.DictReader(f))
print(len(rows))

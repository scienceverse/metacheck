import csv

with open("intermediate.csv", newline="") as f:
    rows = list(csv.DictReader(f))
print(rows[0]["mean_x"])

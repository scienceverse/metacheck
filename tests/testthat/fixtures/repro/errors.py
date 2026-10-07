import csv

with open("data.csv", newline="") as f:
    rows = list(csv.DictReader(f))

raise RuntimeError("deliberate failure for testing")

import csv

with open("data.csv", newline="") as f:
    rows = list(csv.DictReader(f))

mean_x = sum(float(r["x"]) for r in rows) / len(rows)

with open("intermediate.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(["mean_x"])
    writer.writerow([mean_x])

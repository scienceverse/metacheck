import csv

with open("data.csv", newline="") as f:
    rows = list(csv.DictReader(f))

group_a = [float(r["x"]) for r in rows if r["g"] == "a"]
group_b = [float(r["x"]) for r in rows if r["g"] == "b"]

mean_a = sum(group_a) / len(group_a)
mean_b = sum(group_b) / len(group_b)
print("mean_a =", mean_a)
print("mean_b =", mean_b)

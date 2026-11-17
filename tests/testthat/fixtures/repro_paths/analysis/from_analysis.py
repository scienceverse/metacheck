# The authors run this script from its own folder, analysis/.
from pathlib import Path

import pandas as pd

HERE = Path(__file__).resolve().parent
PROC = HERE.parent / "data" / "processed"  # KEEP q01a
d01 = pd.read_csv(PROC / "trials.csv")  # KEEP q01b
d02 = pd.read_csv("../data/raw/raw_lab.csv")  # KEEP q02
d03 = pd.read_csv(HERE / ".." / "data" / "reference" / "ref.csv")  # KEEP q03

# The authors run this script from the top of the project.
import glob
import json
import os
import re
from pathlib import Path

import pandas as pd

RAW = "data/raw"
RAW_P = Path("data") / "raw"
d01 = pd.read_csv("data/raw/raw_lab.csv")  # KEEP p01
d02 = pd.read_csv("raw_lab.csv")  # RESOLVES p02 data/raw/raw_lab.csv
d03 = pd.read_csv(r"C:\Users\me\project\data\raw\raw_lab.csv")  # RESOLVES p03 data/raw/raw_lab.csv
d04 = pd.read_csv(os.path.join(RAW, "raw_lab.csv"))  # KEEP p04
d05 = pd.read_csv(os.path.join("data", "raw", "raw_lab.csv"))  # KEEP p05
d06 = pd.read_csv(Path("data") / "raw" / "raw_lab.csv")  # KEEP p06
d07 = pd.read_csv(RAW_P / "raw_lab.csv")  # KEEP p07
d08 = pd.read_csv(RAW + "/raw_lab.csv")  # KEEP p08
d09 = pd.read_csv(f"{RAW}/raw_online.csv")  # KEEP p09
f10 = glob.glob("data/raw/*.csv")  # KEEP p10
d11 = json.load(open("config/settings.json"))  # KEEP p11
s12 = re.sub(r"raw_(\w+)\.csv", r"clean_\1.csv", "raw_lab.csv")  # KEEP p12
files13 = ["raw_lab.csv", "raw_online.csv"]  # KEEP p13
d13 = [pd.read_csv(os.path.join(RAW, f)) for f in files13]
REF = Path("data") / "reference"  # KEEP p14a
d14 = pd.read_csv(REF / "ref.csv")  # KEEP p14b
PACK = {"a": ["raw_lab.csv"], "b": ["raw_online.csv"]}  # KEEP p15


def ref_path(name):
    return REF / name


d16 = pd.read_csv(ref_path("ref.csv"))  # KEEP p16

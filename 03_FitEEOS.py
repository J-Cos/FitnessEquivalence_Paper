# --------------------------------------------
# Purpose: Fits EEOS to macrostates
# Input Origin Scripts: 02_GetEcosystemMacrostates.R
# Output: csv of ecosystem macrostates with predicted B and dev
# ---------------------------------------------------

import numpy as np
import pandas as pd
import sys
import os

# Ensure we can import from Functions regardless of invocation directory
script_dir = os.path.dirname(os.path.abspath(__file__))
proj_root = os.path.abspath(os.path.join(script_dir, ".."))
if proj_root not in sys.path:
    sys.path.insert(0, proj_root)
if script_dir not in sys.path:
    sys.path.insert(0, script_dir)

try:
    from Code.Functions.eeos_functions import *
except ModuleNotFoundError:
    from Functions.eeos_functions import *


# Read suffix from command line arguments
suffix = sys.argv[1] if len(sys.argv) > 1 else "harte_wd"

# Read CSV
df = pd.read_csv(f"Outputs/MacrostatesHarteWD_{suffix}.csv")

# Apply biomass row-wise
df['B_pred'] = df.apply(biomass, axis=1)
df['dev'] = np.log(df['B'])-np.log(df['B_pred'])

# Save to new CSV
df.to_csv(f"Outputs/EEOSPredsHarteWD_{suffix}.csv", index=False)
if suffix == "harte_wd":
    df.to_csv("Outputs/EEOSPredsHarteWD.csv", index=False)

# --------------------------------------------
# Purpose: Fits EEOS to macrostates
# Input Origin Scripts: 02_GetEcosystemMacrostates.R
# Output: csv of ecosystem macrostates with predicted B and dev
# ---------------------------------------------------

import numpy as np
import pandas as pd
import sys
import os

# Ensure we can import from Functions
# Add the Functions directory specifically to sys.path
from Code.Functions.eeos_functions import *


# Read CSV
df = pd.read_csv("Outputs/MacrostatesHarteWD.csv")

# Apply biomass row-wise
df['B_pred'] = df.apply(biomass, axis=1)
df['dev'] = np.log(df['B'])-np.log(df['B_pred'])

# Optionally save to new CSV
df.to_csv("Outputs/EEOSPredsHarteWD.csv", index=False)

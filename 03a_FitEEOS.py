# --------------------------------------------
# Sript loads macrostate csvs and applies equation of state to make predictions
# also calculate dev (as in Williams and Pettorelli 2025 as deviation between observed and predicted biomass (log-observed biomass – log-predicted biomass) )
# each macrostate set with predicted B is output as csv
# ---------------------------------------------------

# first activate EEOS-env conda environment and then run python3

import numpy as np
import pandas as pd

#########################
# using wood density
#########################
# Read CSV
df = pd.read_csv("Outputs/Macrostates.csv")

# Apply biomass row-wise
df['B_pred'] = df.apply(biomass, axis=1)
df['dev'] = np.log(df['B'])-np.log(df['B_pred'])

# Optionally save to new CSV
df.to_csv("Outputs/EEOSPreds.csv", index=False)

######################
# harte appraoch
######################

# Read CSV
df = pd.read_csv("Outputs/MacrostatesHarte.csv")

# Apply biomass row-wise
df['B_pred'] = df.apply(biomass, axis=1)
df['dev'] = np.log(df['B'])-np.log(df['B_pred'])

# Optionally save to new CSV
df.to_csv("Outputs/EEOSPredsHarte.csv", index=False)


######################
# harte & WD appraoch
######################

# Read CSV
df = pd.read_csv("Outputs/MacrostatesHarteWD.csv")

# Apply biomass row-wise
df['B_pred'] = df.apply(biomass, axis=1)
df['dev'] = np.log(df['B'])-np.log(df['B_pred'])

# Optionally save to new CSV
df.to_csv("Outputs/EEOSPredsHarteWD.csv", index=False)



######################
# harte & WD+LMA appraoch
######################

# Read CSV
df = pd.read_csv("Outputs/MacrostatesHarteWDLMA.csv")

# Apply biomass row-wise
df['B_pred'] = df.apply(biomass, axis=1)
df['dev'] = np.log(df['B'])-np.log(df['B_pred'])

# Optionally save to new CSV
df.to_csv("Outputs/EEOSPredsHarteWDLMA.csv", index=False)
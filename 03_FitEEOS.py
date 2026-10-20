# --------------------------------------------
# Purpose: Fits the Ecological Equation of State (EEoS; exact METE sum-and-integral) to plot
#          macrostates and computes the signed deviation dev = ln(B_obs) - ln(B_pred).
# Input Origin Scripts: 02_GetEcosystemMacrostates.R
# Usage: python Code/03_FitEEOS.py [suffix ...]   (default: all six scalings)
# Output: Outputs/EEOSPreds_<suffix>.csv (macrostates + B_pred, dev, constraint errors con_N, con_E)
# ---------------------------------------------------

import os
import sys

import numpy as np
import pandas as pd

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, script_dir)
from Functions.eeos_functions import biomass, constraints, mete_lambdas  # noqa: E402

ALL_SUFFIXES = ["harte_nowd", "harte_wd", "quad_nowd", "quad_wd", "mte_wd", "chave"]


def fit_eeos(df):
    """Add predicted biomass, deviation and METE constraint errors to a macrostate table."""
    out = df.copy()
    out["B_pred"] = out.apply(biomass, axis=1)
    out["dev"] = np.log(out["B"]) - np.log(out["B_pred"])
    con = out.apply(lambda s: constraints(mete_lambdas(s, thresh=False), s), axis=1)
    out["con_N"] = [c[0] for c in con]
    out["con_E"] = [c[1] for c in con]
    return out


def main(suffixes):
    for suffix in suffixes:
        if suffix not in ALL_SUFFIXES:
            raise ValueError(f"Unknown suffix {suffix!r}; expected one of {ALL_SUFFIXES}")
        macro = pd.read_csv(f"Outputs/Macrostates_{suffix}.csv")
        if not (macro["E"] > macro["N"]).all():  # METE needs E > N (lambda_2 = S/(E-N) > 0)
            raise ValueError(f"{suffix}: some plots have E <= N, outside METE's domain")
        preds = fit_eeos(macro)
        preds.to_csv(f"Outputs/EEOSPreds_{suffix}.csv", index=False)
        worst = np.abs(preds[["con_N", "con_E"]]).to_numpy().max()
        print(f"EEoS fitted for {suffix}: {len(preds)} plots, max |constraint error| = {worst:.2e}")


if __name__ == "__main__":
    main(sys.argv[1:] or ALL_SUFFIXES)

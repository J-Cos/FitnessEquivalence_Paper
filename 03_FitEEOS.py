# --------------------------------------------
# Purpose: Fits the Ecological Equation of State (EEoS; exact METE sum-and-integral) to plot
#          macrostates and computes signed/absolute EEoS deviations plus the exact 3-run
#          additive BIOMASS channel split (dev = r_size + w_H + w_rho).
# Input Origin Scripts: 02_GetEcosystemMacrostates.R
# Usage: python Code/03_FitEEOS.py
# Output: Outputs/EEOSPreds.csv (macrostates + B_pred, AGB_pred_Mg, Q_pred, dev, absdev,
#         r_size, w_H, w_rho, and METE constraint errors con_N, con_E)
# ---------------------------------------------------

import os
import sys

import numpy as np
import pandas as pd

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, script_dir)
from Functions.eeos_functions import biomass, constraints, mete_lambdas  # noqa: E402

MACROSTATES_PATH = "Outputs/Macrostates.csv"
PREDS_PATH = "Outputs/EEOSPreds.csv"


def fit_eeos(df):
    """Add predicted biomass (dimensionless B_pred and physical AGB_pred_Mg [Mg]),
    signed and absolute EEoS deviations (dev, absdev), the exact 3-run additive
    decomposition channels (r_size, w_H, w_rho), and METE constraint errors."""
    out = df.copy()
    if not (out["E"] > out["N"]).all():  # METE needs E > N (lambda_2 = S/(E-N) > 0)
        raise ValueError("Some plots have E <= N, outside METE's domain")

    out["B_pred"] = out.apply(biomass, axis=1)
    if "AGB_ref_Mg" in out.columns:
        out["AGB_pred_Mg"] = out["B_pred"] * out["AGB_ref_Mg"]

    out["Q_pred"] = (out["B_pred"] / out["N"]) / (out["E"] / out["N"]) ** (4.0 / 3.0)
    out["dev"] = np.log(out["B"]) - np.log(out["B_pred"])
    out["absdev"] = np.abs(out["dev"])

    if {"B_size", "W_H", "W_rho"}.issubset(out.columns):
        out["r_size"] = np.log(out["B_size"]) - np.log(out["B_pred"])
        out["w_H"] = np.log(out["W_H"])      # == ln(B_rho / B_size)
        out["w_rho"] = np.log(out["W_rho"])  # == ln(B / B_rho)
        decomp_err = np.max(np.abs((out["r_size"] + out["w_H"] + out["w_rho"]) - out["dev"]))
        if decomp_err > 1e-12:
            raise ValueError(f"Additive decomposition identity violated: max error = {decomp_err:.3e}")

    con = out.apply(lambda s: constraints(mete_lambdas(s, thresh=False), s), axis=1)
    out["con_N"] = [c[0] for c in con]
    out["con_E"] = [c[1] for c in con]
    return out


def main(in_path=MACROSTATES_PATH, out_path=PREDS_PATH):
    macro = pd.read_csv(in_path)
    preds = fit_eeos(macro)
    preds.to_csv(out_path, index=False)
    worst = np.abs(preds[["con_N", "con_E"]]).to_numpy().max()
    decomp_err = np.max(np.abs((preds["r_size"] + preds["w_H"] + preds["w_rho"]) - preds["dev"]))
    print(
        f"EEoS fitted: {len(preds)} plots -> {out_path} | "
        f"max |constraint error| = {worst:.2e} | "
        f"max |r_size + w_H + w_rho - dev| = {decomp_err:.2e}"
    )


if __name__ == "__main__":
    main()

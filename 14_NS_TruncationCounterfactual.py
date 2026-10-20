# --------------------------------------------
# Purpose: N/S truncation counterfactual for undisturbed Jurua plots (SI Table S3).
#          Shows why epsilon ~ DBH^2 gives a positive EEoS baseline offset in 0.25 ha,
#          DBH >= 10 cm plots (low N/S), and that the offset vanishes when N/S is raised to 5
#          while holding the observed trees fixed.
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/TableS3_NS_Counterfactual.csv (per plot) and
#         Outputs/TableS3_NS_Counterfactual_summary.csv (means over plots)
# ---------------------------------------------------

import os
import sys

import numpy as np
import pandas as pd

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, script_dir)
from Functions.eeos_functions import biomass, mete_lambdas  # noqa: E402

NS_COUNTERFACTUAL = 5.0


def mete_moments(S, N, E):
    """METE per-individual CV of metabolic rate, predicted biomass, and scale 1/lambda_2."""
    s = pd.Series({"S": float(S), "N": float(N), "E": float(E)})
    lam = mete_lambdas(s, thresh=False)
    mean_e = biomass(s, power=1.0) / N
    mean_e2 = biomass(s, power=2.0) / N
    cv = np.sqrt(max(0.0, mean_e2 - mean_e ** 2)) / mean_e
    return cv, biomass(s, power=4.0 / 3.0), 1.0 / lam[1]


def plot_row(plot_id, g, min_dbh):
    D = g["DBH"].to_numpy() / min_dbh
    N, S = len(g), g["Binomial_correct"].nunique()
    sp_counts = g["Binomial_correct"].value_counts()
    row = {"Region_Plot": plot_id, "S": S, "N": N, "N_S": N / S,
           "prop_singletons": (sp_counts == 1).mean(), "prop_le2": (sp_counts <= 2).mean()}

    # epsilon = D (linear, as in the main analysis without wood density)
    e1 = D
    cv1, b1, scale1 = mete_moments(S, N, e1.sum())
    row.update(eps1_mean=e1.mean(), cv1_obs=e1.std() / e1.mean(), cv1_mete=cv1,
               scale1_dbh_cm=scale1 * min_dbh, dev1=np.log((e1 ** (4 / 3)).sum()) - np.log(b1))

    # epsilon = D^2 (quadratic, metabolic-theory scaling)
    e2 = D ** 2
    b2_obs = (e2 ** (4 / 3)).sum()
    cv2, b2, scale2 = mete_moments(S, N, e2.sum())
    row.update(eps2_mean=e2.mean(), cv2_obs=e2.std() / e2.mean(), cv2_mete=cv2,
               scale2_dbh_cm=np.sqrt(scale2) * min_dbh, dev2=np.log(b2_obs) - np.log(b2))

    # Counterfactual: same trees (mean epsilon and B/N fixed), N/S raised to 5
    n_cf = NS_COUNTERFACTUAL * S
    cv_cf, b_cf, scale_cf = mete_moments(S, n_cf, n_cf * e2.mean())
    row.update(cv2_cf_mete=cv_cf, scale2_cf_dbh_cm=np.sqrt(scale_cf) * min_dbh,
               dev2_cf=np.log(b2_obs / N) - np.log(b_cf / n_cf))
    return row


def main():
    comm = pd.read_csv("Outputs/comm.csv").dropna(subset=["DBH", "Binomial_correct"])
    min_dbh = comm["DBH"].min()  # global minimum, as in the macrostate calculation
    jurua = comm[comm["Region"] == "Jurua"]
    per_plot = pd.DataFrame([plot_row(pid, g, min_dbh) for pid, g in jurua.groupby("Region_Plot")])
    summary = per_plot.drop(columns="Region_Plot").mean().to_frame("mean_over_plots")
    summary["n_plots"] = len(per_plot)
    per_plot.to_csv("Outputs/TableS3_NS_Counterfactual.csv", index=False)
    summary.to_csv("Outputs/TableS3_NS_Counterfactual_summary.csv", index_label="quantity")
    print(summary.round(4).to_string())


if __name__ == "__main__":
    main()

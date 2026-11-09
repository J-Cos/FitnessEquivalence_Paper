"""Unit tests for Functions/eeos_functions.py (METE / EEoS).

Run from the project root (with the EEOS-env conda environment installed):
    PYTHONNOUSERSITE=1 conda run -n EEOS-env python -m pytest Code/tests -q
"""
import os
import sys
import warnings

import numpy as np
import pandas as pd
import pytest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from Functions.eeos_functions import biomass, constraints, mete_lambdas  # noqa: E402

# (S, N, E): a Jurua-like plot (low N/S), a disturbed-plot-like case, and a high-N/S case
STATES = [(96, 172, 354.0), (54, 117, 260.0), (300, 3000, 15000.0)]


def state(S, N, E):
    return pd.Series({"S": float(S), "N": float(N), "E": float(E)})


@pytest.mark.parametrize("S,N,E", STATES)
def test_lambdas_satisfy_mete_constraints(S, N, E):
    s = state(S, N, E)
    con = constraints(mete_lambdas(s, thresh=False), s)
    assert np.all(np.abs(con) < 1e-3)


@pytest.mark.parametrize("S,N,E", STATES)
def test_lambda2_closed_form(S, N, E):
    s = state(S, N, E)
    assert mete_lambdas(s, thresh=False)[1] == pytest.approx(S / (E - N))


@pytest.mark.parametrize("S,N,E", STATES)
def test_zeroth_and_first_moments_recover_N_and_E(S, N, E):
    """S * sum_n int n e^k R de equals N for k = 0 and E for k = 1 (the METE constraints)."""
    s = state(S, N, E)
    assert biomass(s, power=0.0) == pytest.approx(N, rel=1e-3)
    assert biomass(s, power=1.0) == pytest.approx(E, rel=1e-3)


@pytest.mark.parametrize("S,N,E", STATES)
def test_biomass_bounds(S, N, E):
    """By Jensen's inequality with e >= 1: E <= B(4/3) <= B(2) when power increases."""
    s = state(S, N, E)
    b43 = biomass(s)
    assert E < b43 < biomass(s, power=2.0)


def test_biomass_increases_with_E():
    S, N = 96, 172
    values = [biomass(state(S, N, E)) for E in (300.0, 354.0, 500.0, 1000.0)]
    assert all(np.diff(values) > 0)


def test_biomass_nan_input_returns_nan():
    assert np.isnan(biomass(pd.Series({"S": np.nan, "N": 10.0, "E": 50.0})))


def test_constraint_check_flags_negative_violations():
    """mete_lambdas must warn when a constraint is violated in either direction."""
    s = state(96, 172, 354.0)
    lam = mete_lambdas(s, thresh=False)
    bad = lam * np.array([1.0, 0.5])  # halve lambda_2 -> E/S constraint badly violated
    con = constraints(bad, s)
    assert np.any(np.abs(con) > 0.05)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        from Functions import eeos_functions as ef
        ef._check_constraints(bad, s, thresh=0.05)
    assert any("constraint" in str(w.message).lower() for w in caught)


def test_reproduces_harte2022_published_benchmarks():
    """eeos_functions.py reproduces Harte et al. (2022) published predictions across all 42 datasets."""
    bench_path = os.path.abspath(
        os.path.join(os.path.dirname(__file__), "..", "..", "Data", "Harte2022_data_statevariables.csv")
    )
    if not os.path.exists(bench_path):
        pytest.skip("Harte2022_data_statevariables.csv not present")
    from Functions import eeos_functions as ef

    h = pd.read_csv(bench_path).dropna(subset=["S", "N", "E"])
    assert len(h) == 42
    for _, row in h.iterrows():
        s = state(row["S"], row["N"], row["E"])
        lam = ef.mete_lambdas(s, thresh=False)
        assert lam[0] == pytest.approx(row["lambda_1"], abs=1e-12)
        assert lam[1] == pytest.approx(row["lambda_2"], abs=1e-12)
        assert lam.sum() == pytest.approx(row["beta"], abs=1e-12)
        assert ef.biomass(s) == pytest.approx(row["pBnum"], rel=1e-12)
        assert ef.biomass_approx(s, order=0) == pytest.approx(row["pB0"], rel=1e-12)
        assert ef.biomass_approx(s, order=1) == pytest.approx(row["pB1"], rel=1e-12)
        s23 = state(row["S"], row["N"], row["E_2_3"])
        assert ef.biomass(s23, power=3 / 2) == pytest.approx(row["pBnum_2_3"], rel=1e-12)


def test_fit_eeos_exact_additive_decomposition():
    """03_FitEEOS.fit_eeos adds B_pred, AGB_pred_Mg, Q_pred, dev, absdev, and exact r_size + w_H + w_rho == dev."""
    import importlib

    fit_mod = importlib.import_module("03_FitEEOS")
    df = pd.DataFrame(
        {
            "Region": ["Jurua", "PGM"],
            "Region_Plot": ["Jurua_1", "PGM_1"],
            "S": [96, 54],
            "N": [172, 117],
            "E": [920.0, 480.0],
            "B_size": [2050.0, 960.0],
            "B_rho": [2190.0, 1005.0],
            "B": [2240.0, 910.0],
            "AGB_ref_Mg": [0.045, 0.045],
        }
    )
    df["Q_obs"] = (df["B"] / df["N"]) / (df["E"] / df["N"]) ** (4.0 / 3.0)
    df["Q_size"] = (df["B_size"] / df["N"]) / (df["E"] / df["N"]) ** (4.0 / 3.0)
    df["W_H"] = df["B_rho"] / df["B_size"]
    df["W_rho"] = df["B"] / df["B_rho"]

    out = fit_mod.fit_eeos(df)
    assert np.allclose(out["AGB_pred_Mg"], out["B_pred"] * out["AGB_ref_Mg"])
    assert np.allclose(out["absdev"], np.abs(out["dev"]))
    assert np.allclose(out["r_size"] + out["w_H"] + out["w_rho"], out["dev"], atol=1e-13)

    bad = df.copy()
    bad.loc[0, "E"] = bad.loc[0, "N"]
    with pytest.raises(ValueError, match="outside METE's domain"):
        fit_mod.fit_eeos(bad)


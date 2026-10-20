"""Unit tests for Functions/eeos_functions.py (METE / EEoS).

Run from the project root:
    PYTHONNOUSERSITE=1 /home/j/miniconda3/envs/EEOS-env/bin/python -m pytest Code/tests -q
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

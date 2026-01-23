
"""
    Ensemble de fonctions
    permettant de caractériser les bonnes nuits
    C'est à dire les nuits les plus plates
"""

import numpy as np
import pandas as pd
from scipy.stats import median_abs_deviation
from scipy.signal import savgol_filter

def flatness_std(y: pd.Series) -> float:
    """
    Standard deviation (écart-type) de la série.

    Parameters
    ----------
    y : pandas.Series
        Série temporelle numérique.

    Returns
    -------
    float
        Écart-type calculé en ignorant les valeurs manquantes (NaN).
    """
    return float(y.std(skipna=True))


def flatness_mad(y: pd.Series) -> float:
    """
    Median Absolute Deviation (MAD) normalisé.

    Parameters
    ----------
    y : pandas.Series
        Série numérique.

    Returns
    -------
    float
        MAD (normalisé) calculé avec `scipy.stats.median_abs_deviation`.
    """

    return float(median_abs_deviation(y.dropna(), scale="normal"))


def flatness_total_variation(y: pd.Series) -> float:
    """

    Variation totale (TV) de la série.

     La variation totale mesure la somme des variations absolues
     entre points adjacents :

     .. math::
         TV = \\sum_{t=1}^{T-1} |y_t - y_{t-1}|

     Très sensible aux oscillations rapides.

     Parameters
     ----------
     y : pandas.Series
         Série temporelle.

     Returns
     -------
     float
         Variation totale. ``np.nan`` si la série contient moins de 2 points.

    """
    y = y.dropna().values
    if len(y) < 2:
        return np.nan
    return float(np.abs(np.diff(y)).sum())


def flatness_mean_abs_diff(y: pd.Series) -> float:
    """
    Moyenne de la dérivée absolue : énergie de la pente.

    Mesure la moyenne de ``|Δy|`` entre points adjacents :

    Parameters
    ----------
    y : pandas.Series
        Série temporelle.

    Returns
    -------
    float
        Moyenne des variations absolues. ``np.nan`` si moins de 2 points.
    """

    y = y.dropna().values
    if len(y) < 2:
        return np.nan
    return float(np.mean(np.abs(np.diff(y))))


def amplitude(y: pd.Series) -> float:
    """
    Amplitude du signal : différence entre max et min.
    Parameters
    ----------
    y : pandas.Series
        Série numérique.

    Returns
    -------
    float
        Amplitude (max - min). ``np.nan`` si série vide.
    """

    y = y.dropna().values
    if len(y) == 0:
        return np.nan
    return float(y.max() - y.min())


def smooth_savgol(y: pd.Series, window=11, poly=2) -> pd.Series:
    """
        Lissage Savitzky–Golay (atténuer le bruit instrumental)

    Parameters
    ----------
    y : pandas.Series
        Série temporelle.
    window : int, default=11
        Taille de la fenêtre (doit être impaire).
    poly : int, default=2
        Ordre du polynôme de lissage.

    Returns
    -------
    pandas.Series
        Série lissée. Si moins d'échantillons que ``window``,
        la série originale est renvoyée.
    """

    yv = y.values
    if len(yv) < window:
        return y  # pas assez de points
    return pd.Series(
        savgol_filter(yv, window_length=window, polyorder=poly), index=y.index
    )


def zero_crossings_of_diff(y: pd.Series) -> int:
    """
    Comptage des oscillations via les changements de signe de la dérivée.

    Un zéro-crossing correspond à un changement de signe dans la dérivée
    première ``Δy``. Cela détecte explicitement les oscillations rapides.

    Parameters
    ----------
    y : pandas.Series
        Série temporelle.

    Returns
    -------
    int
        Nombre de changements de signe. ``0`` si série trop courte.
    """

    y = y.dropna().values
    if len(y) < 3:
        return 0
    dy = np.diff(y)
    signs = np.sign(dy)
    # ignore les zéros en les propageant
    signs[signs == 0] = np.nan
    signs = pd.Series(signs).fillna(method="ffill").fillna(method="bfill").values
    return int(np.sum(signs[1:] * signs[:-1] < 0))


def flatness_score(y: pd.Series) -> float:
    """
    Score global de « flatness » basé sur plusieurs métriques.

    Le score combine :
    - moyenne des variations absolues (`flatness_mean_abs_diff`)
    - variation totale (`flatness_total_variation`)
    - amplitude (`amplitude`)
    - nombre de zéro-crossings (`zero_crossings_of_diff`)

    Les métriques sont préalablement lissées (Savitzky–Golay)
    puis **normalisées min-max** localement pour éviter qu’une
    seule domine.

    Parameters
    ----------
    y : pandas.Series
        Série temporelle.

    Returns
    -------
    float
        Score de platitude dans [0,1].
        ``np.nan`` si aucune mesure n'est calculable.
    """

    # Lisse d'abord
    ys = smooth_savgol(y, window=11, poly=2) if len(y) >= 11 else y

    s1 = flatness_mean_abs_diff(ys)  # lissée
    s2 = flatness_total_variation(ys)
    s3 = amplitude(ys)
    s4 = zero_crossings_of_diff(ys)

    # Mise à l’échelle simple pour éviter qu’un indicateur domine
    # (ou utiliser sklearn.preprocessing.StandardScaler sur un lot de nuits)
    comps = np.array([s for s in [s1, s2, s3, s4] if not pd.isna(s)], dtype=float)
    if len(comps) == 0:
        return np.nan
    # Normalisation min-max locale (fallback si constantes)
    denom = comps.max() - comps.min()
    if denom == 0:
        norm = comps / (comps.max() + 1e-12)
    else:
        norm = (comps - comps.min()) / (denom + 1e-12)
    return float(norm.mean())

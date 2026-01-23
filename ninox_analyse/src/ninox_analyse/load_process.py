"""
    Fonctions permettant d'analyser les données issues des capteurs NINOX 
"""
import pandas as pd
import numpy as np
import os

from statistics import mode
from ninox_analyse.score_data import flatness_score


def _drop_if_exists(df: pd.DataFrame, cols_to_drop):
    """
    Suppression de colonnes dans un dataFrame si présentes
    """
    cols_presentes = [c for c in cols_to_drop if c in df.columns]
    if cols_presentes:
        df = df.drop(columns=cols_presentes)

    return df 


def clean_date(df: pd.DataFrame, date_col: str = "date") -> pd.DataFrame:
    """
    Ajoute des colonnes de découpe temporelle et un identifiant de 'nuit' astronomique.
    - y, m, d : année, mois, jour en UTC
    - night_date : date 'astronomique' (nuit) en décalant de 12h (noon-to-noon)
    - night_num_year : numéro de nuit séquentiel au sein d'une année (dense)
    """
    if date_col not in df.columns:
        raise ValueError(f"La colonne '{date_col}' est absente du DataFrame.")

    # S'assure que la colonne est bien de type datetime
    if not pd.api.types.is_datetime64_any_dtype(df[date_col]):
        df = df.copy()
        df[date_col] = pd.to_datetime(df[date_col], utc=True, errors="coerce")

    # Champs de base
    df = df.copy()
    df["y"] = df[date_col].dt.year
    df["m"] = df[date_col].dt.month
    df["d"] = df[date_col].dt.day

    # Heures (UTC)
    df["heure"] = df[date_col].dt.hour
    # Heure décimale utile pour les graphiques (UTC)
    df["heure_graph"] = (
        df[date_col].dt.hour
        + df[date_col].dt.minute / 60.0
        + df[date_col].dt.second / 3600.0
        + df[date_col].dt.microsecond / 3_600_000_000.0
    )

    # Convention 'nuit' astro : on décale de 12h pour regrouper la nuit sur une seule date
    # (noon-to-noon, comme en astronomie).
    # Exemple : le 2025-06-01 à 01:00 UTC -> night_date = 2025-05-31
    night_anchor = (df[date_col] - pd.Timedelta(hours=12)).dt.date
    df["night_date"] = pd.to_datetime(night_anchor)

    # Numéro de nuit séquentiel par année (dense rank)
    df["night_num_year"] = (
        df.drop_duplicates(["y", "night_date"])
        .sort_values(["y", "night_date"])
        .assign(nnum=lambda x: x.groupby("y").cumcount() + 1)
        .merge(df[["y", "night_date"]], on=["y", "night_date"], how="right")["nnum"]
        .values
    )

    return df


def load_and_process_file_csv(file_path: str, year: int | None = None) -> pd.DataFrame:
    """
    Charge le CSV (export NINOX), supprime certaines colonnes,
    convertit le JD en datetime UTC, ajoute des champs de date et un numéro de nuit,
    puis filtre par année si 'year' est fourni.

    Args:
        file_path: chemin du fichier CSV (séparateur ',' ; décimal '.')
        year: année à filtrer (optionnel)

    Returns:
        DataFrame pandas transformé
    """
    # 1) Chargement (équivalent read.csv2(..., sep=",", dec="."), ici read_csv défaut convient)
    data = pd.read_csv(file_path)

    # 2) Retirer colonnes sans intérêt si elles existent
    cols_to_drop = [
        "az",
        "alt",
        "temp_sensor",
        "temp_ambiant",
        "humidity",
        "pressure",
        "cloud_cover",
        "wind_speed",
    ]
    data = _drop_if_exists(data, cols_to_drop)

    # 3) Conversion JD -> datetime UTC
    #    En R : as.POSIXct(as.Date(jd_utc, origin = structure(-2440588 + 0.5, class="Date")))
    #    En Python : JD 2440587.5 = 1970-01-01 UTC
    if "jd_utc" not in data.columns:
        raise ValueError("La colonne 'jd_utc' est absente du CSV.")

    data = data.copy()
    data["date"] = pd.to_datetime(
        (data["jd_utc"] - 2440587.5) * 86400, unit="s", utc=True
    )

    # 4) Nettoyage & enrichissement des dates (équivalent clean_date)
    data = clean_date(data)  # ajoute y, m, d, night_date, night_num_year

    # 5) Filtre par année si demandé
    if year is not None:
        data = data.loc[data["y"] == int(year)].copy()

    return data


def get_modal_sqm_mag_value(data_in: pd.DataFrame) -> float:
    """
    Calcule la valeur modale de sqm_mag.
    Si plusieurs modes, retourne le premier.
    """
    try:
        return mode(data_in["sqm_mag"].dropna())
    except:
        # Si pas de mode unique, on peut prendre la valeur la plus fréquente
        return data_in["sqm_mag"].value_counts().idxmax()


def calculate_stats_data(data_in: pd.DataFrame) -> dict:
    """
    Calcule des statistiques sur les données :
    - moyenne, médiane, modal
    - nombre de jours distincts (ymd)
    - nombre de mesures
    - nombre de mesures avec sqm_mag > 21
    """
    # Moyenne et médiane
    mean_val = data_in["sqm_mag"].mean()
    median_val = data_in["sqm_mag"].median()

    # Modal
    sqm_mag_mod = get_modal_sqm_mag_value(data_in)

    # Nombre de jours distincts
    n_day = data_in["night_date"].nunique() if "night_date" in data_in.columns else None

    # Nombre de mesures distinctes
    n_measure = (
        data_in["measure_id"].nunique() if "measure_id" in data_in.columns else None
    )

    # Nombre de mesures avec sqm_mag > 21
    n_measure_21 = (
        data_in.loc[data_in["sqm_mag"] > 21, "measure_id"].nunique()
        if "measure_id" in data_in.columns
        else None
    )

    stats = {
        "mean": mean_val,
        "median": median_val,
        "sqm_mag_mod": sqm_mag_mod,
        "n_day": n_day,
        "n_measure": n_measure,
        "n_measure_21": n_measure_21,
    }

    return stats


def select_flattest_nights(
    df: pd.DataFrame,
    sun_alt_min: float = -250,
    nb_flat_day: int | None = None,
    bounds: tuple[float, float] | None = (21.3, 22.2),
    max_exclusion_ratio: float = 0.40,  # exclure si >= 40% des points sont exclus
) -> pd.DataFrame:
    """
    Retourne les données des nuits sélectionnées comme les plus plates.
    """
    d = df.loc[df["sun_alt"] < sun_alt_min].copy()

    # 1) Comptage initial par nuit (avant filtres)
    base = df.dropna(subset=["night_num_year"])
    base_counts = base.groupby("night_num_year")["sqm_mag"].size()

    # Optionnel : filtrer par plage de valeurs utiles
    if bounds is not None:
        lo, hi = bounds
        d = d.loc[(d["sqm_mag"] >= lo) & (d["sqm_mag"] <= hi)]

    # 3) Ratio de rétention par nuit (points après / points avant)
    filt_counts = d.groupby("night_num_year")["sqm_mag"].size()
    retention_ratio = (filt_counts / base_counts).fillna(0.0)

    # Nuits à conserver selon le ratio (au moins 30% de points retenus si max_exclusion_ratio=0.70)
    keep_by_ratio = retention_ratio[retention_ratio >= max_exclusion_ratio].index

    # Appliquer l'exclusion par ratio
    d = d[d["night_num_year"].isin(keep_by_ratio)].copy()

    # Score par nuit
    scores = (
        d.groupby("night_num_year")["sqm_mag"]
        .apply(flatness_score)
        .rename("flatness_score")
        .reset_index()
        .dropna()
        .sort_values("flatness_score", ascending=True)
    )

    # Sélection
    if nb_flat_day is not None:
        keep_nights = scores.head(nb_flat_day)["night_num_year"].tolist()
    else:
        # par défaut : garder toutes les nuits triées (tu peux mettre un seuil)
        keep_nights = scores["night_num_year"].tolist()

    # Retour des données correspondantes
    return df[df["night_num_year"].isin(keep_nights)].copy(), scores


def print_report(
    stats: dict,
    data_file_name: str,
    export_file_name: str,
    nom_site: str,
    export_dir: str = "export",
):
    """
    Génère un rapport texte avec les statistiques et l'enregistre dans un fichier .txt.
    """

    # Créer le dossier s'il n'existe pas
    os.makedirs(export_dir, exist_ok=True)

    # Chemin du fichier rapport
    report_path = f"{export_dir}/{export_file_name}.txt"

    # Contenu du rapport
    report_txt = [
        f"SITE : {nom_site}",
        f"FILE : {data_file_name}",
        f"MEAN : {round(stats.get('mean', 0), 2)}",
        f"MEDIAN : {round(stats.get('median', 0), 2)}",
        f"MODAL : {round(stats.get('sqm_mag_mod', 0), 2)}",
        f"Number of days with measurements : {stats.get('n_day', 0)}",
        f"Number of measure sqm > 21 : {stats.get('n_measure_21', 0)}",
        f"Number of measure : {stats.get('n_measure', 0)}",
    ]

    # Écriture dans le fichier
    with open(report_path, "w", encoding="utf-8") as f:
        f.write("\n".join(report_txt))

    print(f"Rapport généré : {report_path}")

"""
    Ensemble de fonctions
    de génération de graphiques
"""

import os
import seaborn as sns
import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
from matplotlib import cm


def calculate_min_max_hour(data_in: pd.DataFrame, x_col: str):
    """
    Calcule l’heure minimale et maximale autour d’une nuit
    ainsi que la durée totale couvrant cet intervalle sur 24h.

    Parameters
    ----------
    data_in : pandas.DataFrame
        Table contenant au moins une colonne horaire numérique.
    x_col : str
        Nom de la colonne représentant les heures (0–23).

    Returns
    -------
    tuple of int
        Un triplet ``(start_hour, end_hour, total_hour)`` où :
        - ``start_hour`` : plus petite heure >= 12
        - ``end_hour`` : plus grande heure <= 12
        - ``total_hour`` : durée totale en heures, avec passage éventuel par minuit

    """
    start_hour = int(data_in[data_in[x_col] >= 12][x_col].min())
    end_hour = int(data_in[data_in[x_col] <= 12][x_col].max())
    total_hour = (24 - start_hour) + end_hour
    return (start_hour, end_hour, total_hour)


def generate_graph_density(
    data_in: pd.DataFrame, nom_site: str, sous_titre: str, export_dir: str = "exports"
):
    """
    Génère un graphique de densité (scatter plot) :
    - X : heure_graph
    - Y : sqm_mag (inversé)
    - Points bleus très fins
    - Titre avec nombre de nuits
    Sauvegarde en JPG.
    """

    # Créer le dossier s'il n'existe pas
    os.makedirs(export_dir, exist_ok=True)

    # 1) Calcul du nombre de nuits distinctes
    nb_nuits = (
        data_in["night_num_year"].nunique()
        if "night_num_year" in data_in.columns
        else 0
    ) 
    title_nb_nights = f" ({nb_nuits} nights)" if nb_nuits > 1 else ""

    df = data_in.copy()
    x_col = "heure_graph"
    y_col = "sqm_mag"

    start_hour, end_hour, total_hour = calculate_min_max_hour(df, x_col)
    if x_col not in df or y_col not in df:
        raise ValueError(f"Colonnes requises absentes: {x_col}, {y_col}")

    # 1) Axe X modulo 24 décalé de start_hour
    df["_x_plot"] = (df[x_col] - start_hour) % 24

    # 2) Tracé
    plt.figure(figsize=(5, 6))
    sns.scatterplot(x="_x_plot", y=y_col, data=df, color="blue", s=1)  # points fins

    # 3) Axe Y inversé
    ax = plt.gca()
    plt.ylim(16, 23)
    plt.gca().invert_yaxis()

    # # 4) Ticks X à chaque heure, labels reconstitués
    ticks = np.arange(0, total_hour, 1)  # 0..24
    labels = [f"{int((t + start_hour) % 24):02d}h" for t in ticks]
    plt.xticks(ticks, labels)

    # Labels
    plt.xlabel("Time (UTC)")
    plt.ylabel("NSB (magsqm/arcsec²)")

    # Titre
    plt.title(f"NINOX {nom_site} {sous_titre}{title_nb_nights}")

    # Sauvegarde
    filename = f"{nom_site}_{sous_titre}_densite.jpg".replace(" ", "_")
    filepath = os.path.join(export_dir, filename)
    plt.tight_layout()
    plt.savefig(filepath, dpi=300)
    plt.close()

    print(f"Graphique sauvegardé sous : {filename}")


def generate_graph_density_heatmap(
    data_in: pd.DataFrame,
    nom_site: str,
    sous_titre: str,
    modal: float,
    export_dir: str = "exports",
):
    """
    Heatmap de densité avec :
    - Palette Spectral inversée
    - Axe Y inversé (16 en haut, 23 en bas)
    - Axe X réorganisé (start_hour -> start_hour+24)
    - Barre de couleur pour la densité
    """
    # Créer le dossier s'il n'existe pas
    os.makedirs(export_dir, exist_ok=True)

    # Préparer les données
    x = data_in["heure_graph"]
    y = data_in["sqm_mag"]

    # Décalage de l'heure pour commencer à start_hour
    start_hour, end_hour, total_hour = calculate_min_max_hour(data_in, "heure_graph")
    print(start_hour)
    x_plot = (x - start_hour) % 24

    # Histogramme 2D
    counts, xedges, yedges = np.histogram2d(x_plot, y, bins=300)

    # Palette Spectral inversée
    cmap = cm.get_cmap("Spectral_r")

    # Tracé
    plt.figure(figsize=(6, 6))
    img = plt.imshow(
        counts.T,
        origin="lower",
        aspect="auto",
        extent=[xedges[0], xedges[-1], yedges[0], yedges[-1]],
        cmap=cmap,
    )

    # Inverser Y pour avoir 16 en haut et 23 en bas
    plt.ylim(16, 22.2)
    plt.gca().invert_yaxis()

    # Ligne horizontale pour modal
    plt.axhline(y=modal, color="blue", linestyle="--")

    # Annotation
    plt.text(
        xedges[-1],
        modal,
        f"modal {round(modal, 2)}",
        ha="right",
        va="bottom",
        color="blue",
    )

    # Ticks X réorganisés
    ticks = np.arange(0, total_hour, 2)  # toutes les 2 heures
    labels = [f"{int((t + start_hour) % 24):02d}h" for t in ticks]
    plt.xticks(ticks, labels)

    # Titre et axes
    plt.title(f"NINOX {nom_site} {sous_titre}")
    plt.xlabel(f"Heure (UTC) – départ {start_hour:02d}h")
    plt.ylabel("NSB (magsqm/arcsec²)")

    # Barre de couleur
    cbar = plt.colorbar(img)
    cbar.set_label("Densité (nombre de points)")

    # Sauvegarde
    filename = f"{nom_site}_{sous_titre}_heatmap.jpg".replace(" ", "_")
    filepath = os.path.join(export_dir, filename)
    plt.tight_layout()
    plt.savefig(filepath, dpi=300)
    plt.close()

    print(f"Heatmap sauvegardée sous : {filename}")


def generate_graph_magnitude(
    data_in: pd.DataFrame,
    nom_site: str,
    sous_titre: str,
    modal: float,
    export_dir: str = "exports",
):
    """
    Histogramme des valeurs sqm_mag avec :
    - ligne verticale à la valeur modal
    - titre et sous-titre
    - sauvegarde en JPG
    """

    # Créer le dossier s'il n'existe pas
    os.makedirs(export_dir, exist_ok=True)

    # Histogramme avec binwidth ~0.09
    binwidth = 0.025
    bins = int((23 - 16) / binwidth)

    plt.figure(figsize=(5, 6))
    sns.histplot(data_in["sqm_mag"], bins=bins, color="blue", edgecolor="black")

    # Ligne verticale pour modal
    plt.axvline(modal, color="black", linestyle="--", linewidth=0.8)

    # Axe X : limites et ticks
    plt.xlim(16, 23)
    plt.xticks([round(x, 0) for x in list(np.arange(16, 22.2, 0.5))])

    # Labels
    plt.xlabel("NSB (magsqm/arcsec²)")
    plt.ylabel("Fréquence")

    # Titre et sous-titre
    nb_nuits = (
        data_in["night_num_year"].nunique()
        if "night_num_year" in data_in.columns
        else "?"
    )
    plt.title(
        f"NINOX {nom_site} {sous_titre} ({nb_nuits} nights)\nModal value = {round(modal, 2)}"
    )

    # Sauvegarde
    filename = f"{nom_site}_{sous_titre}_magnitude.jpg".replace(" ", "_")
    filepath = os.path.join(export_dir, filename)

    plt.tight_layout()
    plt.savefig(filepath, dpi=300)
    plt.close()

    print(f"Histogramme sauvegardé sous : {filepath}")

# Paquet python d'analyse des données issues des catpeurs ninox

## installation
pip install . 

## usage
exemple de script 

```sh
import os

from ninox_analyse.load_process import (
    load_and_process_file_csv,
    select_flattest_nights,
    calculate_stats_data,
    print_report,
)
from ninox_analyse.generate_graph import (
    generate_graph_density,
    generate_graph_density_heatmap,
    generate_graph_magnitude,
)


def process_ninox_data(
    nom_site: str, data_file: str, data_file_path: str, export_dir: str, year: int
):
    # Test file exist
    data_file_path = f"{data_path}/{data_file}"
    if not os.path.isfile(data_file_path):
        print(f"{data_file_path} not found")
        return

    df = load_and_process_file_csv(data_file_path, year)
    df = df.loc[df["sqm_mag"] < 22.2].copy()
    # df.to_csv(f"{export_dir}/test_export.csv")

    df_select_flattest_nights, ranking = select_flattest_nights(
        df, sun_alt_min=-250, nb_flat_day=10, bounds=(21.3, 22.2)
    )
    # df_select_flattest_nights.to_csv(f"{export_dir}/test_export_best_df.csv")

    # statistiques toutes les nuits
    stats_value = calculate_stats_data(df)
    print_report(
        stats=stats_value,
        data_file_name=data_file,
        export_file_name=f"{nom_site}_toutes_nuits",
        nom_site=nom_site,
        export_dir=export_dir,
    )
    modal = stats_value["sqm_mag_mod"]
    
    # statistiques valeur > 21
    stats_value_upper_21 = calculate_stats_data(df.loc[df["sqm_mag"] >= 21].copy())
    print_report(
        stats=stats_value_upper_21,
        data_file_name=data_file,
        export_file_name=f"{nom_site}_upper_21",
        nom_site=nom_site,
        export_dir=export_dir,
    ) 

    # statistiques toutes les nuits sans lunes
    data_without_moon = df.loc[df["moon_alt"] < 0].copy()
    stats_value_without_moon = calculate_stats_data(data_without_moon)
    print_report(
        stats=stats_value_without_moon,
        data_file_name=data_file,
        export_file_name=f"{nom_site}_nuits_sans_lune",
        nom_site=nom_site,
        export_dir=export_dir,
    )
    modal_without_moon = stats_value_without_moon["sqm_mag_mod"]

    generate_graph_density(
        data_in=df_select_flattest_nights,
        nom_site=nom_site,
        sous_titre="Meilleures nuits",
        export_dir=export_dir,
    )

    # Génération d'un graphique par nuit selectionnée
    selected_night = (
        df_select_flattest_nights["night_date"].unique()
        if "night_date" in df_select_flattest_nights.columns
        else None
    )
    for n in selected_night:
        generate_graph_density(
            data_in=df_select_flattest_nights[
                df_select_flattest_nights["night_date"] == n
            ],
            nom_site=nom_site,
            sous_titre=f"Nuit {n.strftime("%d-%m-%Y")}",
            export_dir=f"{export_dir}/best_night",
        )
    generate_graph_density(
        data_in=df,
        nom_site=nom_site,
        sous_titre="Toutes les nuits",
        export_dir=export_dir,
    )
    generate_graph_density(
        data_in=data_without_moon,
        nom_site=nom_site,
        sous_titre="Nuits sans lune",
        export_dir=export_dir,
    )
    generate_graph_density_heatmap(
        data_in=data_without_moon,
        nom_site=nom_site,
        sous_titre=f"Nuits {year} sans lune",
        modal=modal_without_moon,
        export_dir=export_dir,
    )
    # Magnitude
    generate_graph_magnitude(
        data_in=data_without_moon,
        nom_site=nom_site,
        sous_titre=f"Nuits {year} sans lune",
        modal=modal_without_moon,
        export_dir=export_dir,
    )
    generate_graph_magnitude(
        data_in=df,
        nom_site=nom_site,
        sous_titre=f"Nuits {year}",
        modal=modal,
        export_dir=export_dir,
    )


data_to_process = [
    (
        "Lac des Pises",
        "donnees_exemples/",
        "ninox_measure.csv",
        "export/Lac des Pises",
    ), 
]

for nom_site, data_path, data_file, export_dir in data_to_process:
    year = 2025
    process_ninox_data(nom_site, data_file, data_path, export_dir, year)
```
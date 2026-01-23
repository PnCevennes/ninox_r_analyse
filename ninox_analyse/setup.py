from setuptools import setup, find_packages
from pathlib import Path


root_dir = Path(__file__).absolute().parent
with (root_dir / "VERSION").open() as f:
    version = f.read().strip()


setup(
    name="ninox_analyse",
    version=version,
    description="Analyse des données ninox issues des capteurs de darkskylab",
    author="Amandine SAHL",
    author_email="amandine.sahl@cevennes-parcnational.fr",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    include_package_data=True,
    install_requires=[open("requirements.txt", "r")],
    extras_require={
        "dev": [
            "black",
            "pytest",
            "pytest-flask",
            "pytest-benchmark",
            "pytest-cov",
        ],
        "doc": [
            "sphinx>=7.2",
            "sphinx-rtd-theme>=2.0",
            "myst-parser>=2.0",
            "sphinx-autodoc-typehints>=2.0",
        ],
    },
    python_requires=">=3.9",
)

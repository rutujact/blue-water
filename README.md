The Blue Water Potential Toolkit
================

**A socio-hydrological framework to scale water budgets from plot to
agrarian
system.**[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20665272.svg)](https://doi.org/10.5281/zenodo.20665272)

This repository contains the computational workflow and analytical
engine for the manuscript: \> **Hemingway, C. et al. (202X).**
*Accounting the unaccounted: a new framework for incorporating farm
social diversity in micro-regional water budgets* \[Journal Name\].
\[DOI\]

The **The Blue Water Potential Toolkit** provides a generalized R
framework for simulating Blue Water Potential (Gross Groundwater
Recharge minus Irrigation Requirement) at plot, farm, and
village/micro-regional scale. By coupling climate, soil and crop
information with socio-agricultural data, this toolkit computes and
displays water fluxes, revealing how different types of farms can be
either net consumers or net producers of blue water in a given climate.

------------------------------------------------------------------------

## 📂 Repository Structure

The codebase uses a strict, automated pipeline to separate user-provided
data from model-generated data.

``` text
project_root/
├── data/
│   ├── input/               <- REQUIRED: Place your 9 standardized static CSVs here
│   └── processed/           <- AUTOMATED: Derived model requirements (e.g., Kc curves) land here
├── code/
│   ├── 01_Prepare_Model_Inputs.qmd  <- Run 1st: Generates processed Kc curves (Phenology Engine)
│   ├── 02_Blue-Water.qmd            <- Run 2nd: Executes Water Budget & plots results (Analytical Engine)
│   └── functions/                   <- Core Calculation and figure functions
├── shiny_app/               <- The interactive Web Application UI
├── output/                  <- Generated water budget CSV summaries
├── figures/                 <- Generated visualization outputs
├── CONTRIBUTING.md          <- Guidelines for contributing to the codebase
├── LICENSE                  <- Main GNU GPLv3 License text
└── COPYING.LESSER           <- Additional GNU LGPL-3.0 License terms
```

------------------------------------------------------------------------

## 💻 Installation & Setup

To use this toolkit, you will need to run it locally on your machine.

1.  **Install R and RStudio:** Ensure you have
    [RStudio](https://posit.co/download/rstudio-desktop/) installed.
    This toolkit was developed and tested using **R version 4.2.2**. To
    guarantee full compatibility with the `renv` package library, we
    strongly recommend installing [R
    4.2.2](https://cran.r-project.org/src/base/R-4/), though newer
    versions may still work.

2.  **Install Required Packages:** Open RStudio, copy the following
    command, and paste it into your Console to manually install all
    necessary dependencies. *(If RStudio prompts you to compile from
    sources, it is generally safer to type “no” and use the pre-compiled
    binaries).*

``` r
install.packages(c("shiny", "bslib", "shinyWidgets", "tidyverse", "data.table", "lubridate", "stringr", "scales", "ggpubr", "patchwork", "RColorBrewer", "networkD3", "htmlwidgets", "parallel", "pbapply", "here", "DT"))
```

3.  Environment Stability

This project uses `renv` to manage package versions. This ensures that
the application remains stable and reproducible regardless of future
package updates.

- **To initialize:** After cloning the repository, open the project in
  RStudio. It will detect the `renv` environment. Run:

``` r
  renv::restore()
```

This will automatically install the precise versions of the packages
required to run the toolkit.

## 🚀 Two Ways to Interact with this Toolkit

You can interact with the underlying analytical engine in two different
ways, depending on your comfort level with R code.

### Method 1: The Interactive Shiny App (Recommended)

We have provided a fully interactive, point-and-click user interface
built on R Shiny.

1.  Open the `Blue-Water.Rproj` file in RStudio.
2.  In the R Console, type and run:

``` r
shiny::runApp("shiny_app")
```

3.  Use the **Data Setup** tab to load the default repository data or
    upload your own localized CSVs.

4.  Navigate through **Track 1** and **Track 2** to generate the water
    budgets and download a compiled PDF report.

### Method 2: The Quarto Notebooks (For Advanced Users)

For researchers, or users who want to deeply interrogate the underlying
code, the core workflows are preserved as reproducible Quarto markdown
scripts in the `code/` directory.

- `01_Prepare_Model_Inputs.qmd`: Demonstrates the step-by-step
  mathematical expansion of discrete crop parameters into continuous
  366-day daily curves.
- `02_Blue-Water.qmd`: Executes the heavy parallelized daily water
  budgets and generates visualizations.

------------------------------------------------------------------------

## 📊 Data Requirements

If applying this toolkit to a new region, you will need the following
localized datasets. Refer to the data preview tables in the Shiny App or
the Quarto HTML reports for strict formatting rules.

**Scale 0: Hydrological & Phenological Parameters**

1.  **Model Parameters (`10_model_parameters.csv`)**: The master
    configuration file. This contains essential hydrological assumptions
    (e.g., runoff Curve Number, bare-soil evaporation).

**Scale 1: Blue Water Potential for Cropping Systems.**

2.  **Climatic Forcings**: Multi-year Daily Precipitation and Reference
    Evapotranspiration (ET0).
3.  **Soil Properties**: Water Holding Capacity (WHC) and Readily
    Available Water (RAW).
4.  **Crop Parameters**: Rooting depths and whether the crop is
    irrigated or rainfed.
5.  **Crop Cycle**: Sowing date, Crop phenological phase duration and
    respective crop coefficients (Kc) according to FAO56 method (Allen
    et al., 1998).
6.  **Cropping Systems**: Crop rotation at the plot scale.

**Scale 2: Blue Water Potential for Production Systems.** 7. **Farm
production systems**: Aggregation of grazed and cropped area (ha) for
each farming system.

**Scale 3: Blue Water Potential for Agrarian Systems.** 8. **Agrarian
system**: Area (ha) of each farm production system (ha) in a given
village/micro-region.

------------------------------------------------------------------------

## 👥 Authors & Contributions

### Toolkit Development & Attribution

This repository represents the digital translation and scaling of a
micro-regional water accounting model: \* **Model Rationale, Data
Collection & Visual Concepts:** Developed in Excel by Charlotte
Hemingway & Laurent Ruiz. \* **R Translation, Framework Design, &
Documentation:** Developed by Rutuja Chitra-Tarak.

### Manuscript Author Statement (CRediT)

For the associated academic publication, contributions are defined as
follows: \* **Conceptualization / Methodology:** C. Hemingway, C.
Aubron, M. Vigne, L. Ruiz, R. Chitra-Tarak \* **Software:** R.
Chitra-Tarak, C. Hemingway \* **Validation:** C. Hemingway, R.
Chitra-Tarak \* **Formal Analysis:** C. Hemingway, L. Ruiz, R.
Chitra-Tarak \* **Investigation:** C. Hemingway \* **Data Curation:** C.
Hemingway, R. Chitra-Tarak \* **Visualization:** C. Hemingway, R.
Chitra-Tarak \* **Writing - Original Draft:** C. Hemingway, L. Ruiz \*
**Writing - Review & Editing:** C. Aubron, M. Vigne, L. Ruiz, G. Boulet,
S. Muddu, R. Chitra-Tarak \* **Funding Acquisition:** C. Aubron

------------------------------------------------------------------------

## 📢 Funding & Disclosure

- **R. Chitra-Tarak** contributed to this work as an independent
  volunteer researcher receiving no external funding. R. Chitra-Tarak is
  a long-COVID survivor and patient-advocate; this software contribution
  was developed intermittently over a period of 3 years as health
  permitted.
- **Other Authors:** Official funding statements and grant
  acknowledgments for the remaining research team are detailed in the
  published manuscript.

------------------------------------------------------------------------

## 📜 License & Citation

### Open-Source License

The computational workflow, functions, and codebase contained within
this repository are licensed under the **GNU Lesser General Public
License v3.0 (LGPL-3.0)**.

- **For Open-Source Use & Academics:** You are free to use, modify, and
  link against this toolkit in your own research or open-source
  projects.
- **For Modifications:** Any modifications, improvements, or forks made
  directly to the core Blue Water Toolkit codebase must be disclosed and
  shared openly under the same LGPL-3.0 license terms.
- **Commercial Inquiries:** If you are a commercial entity wishing to
  integrate this framework into closed-source, proprietary software or
  private environmental consulting tools without the copyleft
  constraints of the LGPL, please contact the author directly to discuss
  an alternative commercial license.

### Preferred Citation

If you use this toolkit, its underlying logic, or adapt the framework
for another watershed study, please cite both the software repository
and the primary manuscript:

- **Software:** Chitra-Tarak, R., Hemingway, C., Aubron, C., Boulet, G.,
  Vigne, M., Muddu, S., & Ruiz, L. (2026). *The Blue Water Potential
  Toolkit: a socio-hydrological framework to scale water budgets from
  plot to agrarian system (Version 1.0.0)*. Zenodo.
  <https://doi.org/10.5281/zenodo.20665272>

- **Manuscript:** Hemingway, C., Aubron, C., Chitra-Tarak, R., Boulet,
  G., Vigne, M., Muddu, S., & Ruiz, L. (202X). *Accounting the
  unaccounted: a new framework for incorporating farm social diversity
  in micro-regional water budgets*. \[Journal Name\]. \[DOI\]

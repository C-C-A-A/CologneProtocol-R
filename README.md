## Research compendium for "Approaching Prehistoric Demography: Proxies, Scales and Scope of the Cologne Protocol in European contexts"

### Compendium DOI:

**DOI will be created by figshare upon final publication**

The file linked above is connected to the publication and represents the version of the manual at that time. The files of this repository are the development versions and may have changed since the paper was published.

### Authors of this repository:

Manuel Broich (mbroich@smail.uni-koeln.de)

Robin Peters (mail@robinpeters.net)

### Published in:

Schmidt, I., Hilpert, J., Kretschmer, I., Peters, R., Broich, M., Schiesberg, S., Vogels, O., Wendt, K. P., Zimmermann, A., Maier, A., **submitted**. Approaching Prehistoric Demography: Proxies, Scales and Scope of the Cologne Protocol in European contexts. _Philosophical Transactions B_.

### Contents:

- One rmarkdown file (`EarlyNeolithic.Rmd`) that can be executed in RStudio. This file contains an example application of the **Cologne Protocol**

- The `code\` directory contains the script of the **Cologne Protocol**, which is organized in four `.R` files

- The `output\` directory contains a `csv` file with the statistical properties of the isolines of the sample data (Early Neolithic). The export of `shape` and `raster` files is included in `03_Visualisation_Export.R`. Due to their size they are not stored in this repository. 

### Overview:

The **Cologne Protocol** is a geostatistical approach for estimating prehistoric population size and density.  
This repository contains a manual on how to model Core Areas with `R`. This modelling approach constitutes the first of two successive tasks within the 'Cologne Protocol' to estimate past population sizes and densities, described in more detail elsewhere (Schmidt et al. 2020: S2.1. and S2.2.).
The manual outlines the technical implementation of working steps 1 to 12 (see Schmidt et al. 2020: Table S2): Firstly a GIS-analysis of site distributions and secondly the identification of the ODI. The working steps include the construction of Voronoi diagrams and "Largest Empty Circles", kriging, converting the kriging results into isolines and finally calculating the criteria to select the ODI.  
The aim of this repository is neither to explain the theoretical background nor the further steps of the **Cologne Protocol**. For these points please refer to the associated publication including the supplementary information.

### How to use

- If you already know the **Cologne Protocol** and if you are familiar with `R`, we recommend starting with the `.R` files in the `code\` directory. Change the variables in the first part of file `00_LEC.R` according to your research needs and run all code successively.  
- If you know a bit of the **Cologne Protocol** and programming with `R`, we recommend starting with the `EarlyNeolithic.Rmd` file.  
- If you don't know the **Cologne Protocol**, we recommend reading the above mentioned publication first.

### Other resources

There exist other manuals and recourses to fulfill the first two parts of the **Cologne Protocol**. The above mentioned publications is not only accompanied by an `R` manual but also by manuals for `QGIS`, `MapInfo` and `ArcGIS`. Please see the publication for more information.  

Additionally, we know two other implementations in `R`: one package called [lecAAR](https://github.com/ISAAKiel/lecAAR) by the Initiative for Statistical Analysis in Archaeology Kiel (ISAAK) and one [script](https://uni-tuebingen.de/en/research/core-research/collaborative-research-centers/sfb-1070/organisation/service-project-s/technical-notes/technical-note-1.html) by Ahlrichs, J., Henkner, J. and Schmidt, K.
This manual and these two implementations overlap partly because of the use of the same packages, but we think that the present manual provides for the first time a complete coverage of the first two parts of the **Cologne Protocol**.

### Licence:

Code: MIT (http://opensource.org/licenses/MIT) year: 2020, copyright holders: Manuel Broich and Robin Peters

### Dependencies:

The manual was developed under R version 3.6.2.

All necessary dependencies are documented in the `deps.yaml` file. They can be installed manually or with `automagic::install_deps_file()`. 

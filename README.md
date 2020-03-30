## Research compedium for "Approaching Prehistoric Demography: Proxies, Scales and Scope of the Cologne Protocol in European contexts"

### Compendium DOI:

**The DOI will be created by figshare upon final publication**

The files at the URL above are linked to the publication and represent the version of the manual at that time. The files of this repository are the developement versions and may have changed since the paper was pulished.

### Authors of this repository:

Manuel Broich (mbroich@smail.uni-koeln.de)

Robin Peters (mail@robinpeters.net)

### Published in:

Schmidt, I., Hilpert, J., Kretschmer, I., Peters, R., Broich, M., Schiesberg, S., Vogels, O., Wendt, K. P., Zimmermann, A., Maier, A., **submitted**. Approaching Prehistoric Demography: Proxies, Scales and Scope of the Cologne Protocol in European contexts. _Philosophical Transactions B_.

### Contents:

- One rmarkdown file (`EarlyNeolithic.Rmd`) that can be executed in RStudio. This file contains an example application of the **Cologne Protocol**

- The `code\` directory contains the script of the **Cologne Protocol**, which is organized in four `.R` files

- The `output\` directory contains`raster`, `shape` and `csv` files. These files contain the kriging results, the isolines and their statisical properties

### Overview:

The **Cologne Protocol** is a geostatistical approach for estimating prehistoric population size and density.  
This repository contains a manual on how to model Core Areas with R. More specifically, the technical implementation of the first two parts of the **Cologne Protocol** is explained. The first part is a GIS-analysis of site distrbutions and the second part deals with the identification of so-called Core Areas. These two parts can be devided further into several working steps, including the construction of voronoi diagrams and "Largest Empty Circles", kriging, converting the kriging results into isolines and finally calculating the needed criteria to select the Optimally Describing Isoline.
The aim of this repository is neither to explain the further steps of the **Cologne Protocol** nor the theoretical background. For these points please refer to the associated publication including the supplementary information.

### How to use

If you already know the **Cologne Protocol** and if you are familiar with `R`, we recommend starting with the `.R` files in the `code\` directory. Change the variables in the first part of file `00_LEC.R` according to your research needs and run all code successively.  
If you know a bit of the **Cologne Protocol** and programming with `R`, we recommend starting with the `EarlyNeolithic.Rmd` file.
If you don't know the **Cologne Protocol**, we recommend to read the above mentioned publication first.

### Other resources

There exsist other manuals and recourses to fulfill the first two parts of the **Cologne Protocol**. The above mentiond publications is not only accopanied by an `R` manual but also by manuals for `QGIS`, `MapInfo` and `ArcGIS`. Please see the publication for more information.  

Additionally, we know two other implementations in `R`: one package called [lecAAR](https://github.com/ISAAKiel/lecAAR) by the Initiative for Statistical Analysis in Archaeology Kiel (ISAAK) and one [script](https://uni-tuebingen.de/en/research/core-research/collaborative-research-centers/sfb-1070/organisation/service-project-s/technical-notes/technical-note-1.html) by Ahlrichs, J., Henkner, J. and Schmidt, K.
This manual and these two implementations overlap partly because of the use of the same packages, but we think that the present manual provides for the first time a complete coverage of the first two parts of the **Cologne Protocol**.

### Licence:

Code: MIT (http://opensource.org/licenses/MIT) year: 2020, copyright holders: Manuel Broich and Robin Peters

### Dependencies:

The manual was developed under R version 3.6.2.

All necessary dependencies are documented in the `deps.yaml` file. They can be installed manually or with `automagic::install_deps_file()`. 

# Strategic collection lights up global freshwater fish diversity darkspots with minimal cost

### **Significance Statement**

There are significant gaps in the description, mapping, and sequencing of freshwater fish species, hindering effective biodiversity assessment and conservation efforts. This study quantifies taxonomic, geographic, and genetic knowledge shortfalls across 3,364 global drainage basins, identifying hotspots of undocumented freshwater diversity and revealing their underlying socioeconomic drivers. By integrating multi-objective optimization, we pinpoint key regions,particularly large tropical rivers,where strategic data collection can have the greatest impact on biodiversity conservation at minimal cost. This approach provides a comprehensive blueprint to address global freshwater biodiversity gaps, balancing urgency, feasibility, and conservation value. Ultimately, our work contributes to advancing global biodiversity documentation and freshwater conservation efforts.

### **Code and Data**

The R code used in this study, along with data processing and manipulation files, is available on GitHub:

[GitHub Repository](https://github.com/Otoliths/FW_Fish_Darkspot)


### **Folder Descriptions**
- **code/**: Contains R scripts for data processing, analysis, and figure generation.
- **images/**: Stores images generated during the study, such as figures used in the manuscript.
- **data/**: Contains the datasets used in the analysis, including raw and processed data files.
- **FW_Fish_Darkspot.Rproj**: RStudio project file for project organization and workflow management.


This repository is organized as follows:

```text
/project_root
├── /code
│ ├── Figure1.R/Rmd # R/Rmd script for ploting Figure 1
│ ├── Figure2.R/Rmd # R/Rmd script for ploting Figure 2
│ ├── Figure3.R/Rmd # R/Rmd script for ploting Figure 3
│ ├── Figure4.R/Rmd # R/Rmd script for ploting Figure 4
│ ├── Figure1.html # R script + result1
│ ├── Figure2.html # R script + result2
│ ├── Figure3.html # R script + result3
│ ├── Figure4.html # R script + result4
│
├── /images
│ ├── Figure1.png # Image of Figure 1
│ ├── Figure2.png # Image of Figure 2
│ ├── Figure3.png # Image of Figure 3
│ ├── Figure4.png # Image of Figure 4
│
├── /data
│ ├── biogeographic_list.csv
│ ├── new_discovery.csv
│ ├── inland.rds
│ ├── darkspots.rds
│ ├── hotspots_2016.rds #Hoffman M. et al. [Zenodo](https://doi.org/10.5281/zenodo.3261807)
│ ├── posterior_summary.rds
│ ├── sdg_basin.rds
│ ├── cost_darkspot.rds
│ ├── shortfall_basin.rds
│ ├── shortfall_index.rds
│
└── FW_Fish_Darkspot.Rproj # RStudio project file for organizing the workflow
```

### **[Supporting Information](https://doi.org/10.6084/m9.figshare.29262098.v3)**

- **Table S1**: Comprehensive list of the 18,821 valid freshwater fish species included in this study (updated 24 November 2024).
- **Table S2**: List of 395 drainage basins identified as freshwater fish biodiversity darkspots across eight shortfall scenarios.

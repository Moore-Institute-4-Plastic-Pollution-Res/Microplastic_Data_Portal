# Initial Datasets to Develop Software and Database
## Manuscripts.xlsx & Samples.xlsx
Metadata descriptions for each of the files columns are embedded within the column name. Rows in the manuscripts file are unique manuscripts the data originated from. Rows in the samples file are unique samples within the manuscripts. Files can be linked using the DOI of the manuscript. Nested data is described with semicolons in cells. Where data was unavailable or unknown N/A is provided.

# Metadata
## Definitions for terms in database
### Manuscripts
DOI <- digital object identifier of manuscript being referenced

Sample_device_and_deployment_methods <- type and size of sample device used and how it was deployed

Digestion <- type of digestion used in preprocessing, if any

Filtration <- what type of filtration used

Filter_Size <- size(s) of filter(s)

Microplastic_Identification_Method <- type(s) of identification used, e.g. visual, fluorescent, SEM (scanning electron microscopy), light microscopy

Spectral_Analysis <- type of spectral analysis used in study, if any, e.g. GC/MS, FTIR, Raman

Controls <- type of controls used, if applicable

### Samples
DOI <- digital object identifier of manuscript being referenced

Sample_ID <- unique key for sample being referenced

Subsample_ID <- unique key for subsample being referenced, if applicable

Location <- most precise data possible for location where sample was collected

Source <- source of water in sample, either "tap water" or "bottled water"

Date <- date sample was collected

Concentration <- concentration of microplastics in sample

Concentration_Units <- units used in corresponding concentration

### Polymer
Sample_ID <- corresponding Sample ID from Sample sheet

Subsample_ID <- corresponding Subsample ID from Sample sheet

Polymer <- polymers found in sample

Percentage <- percentage of corresponding polymer if applicable

### Size
Sample_ID <- corresponding Sample ID from Sample sheet

Subsample_ID <- corresponding Subsample ID from Sample sheet

Size <- size ranges found in sample

Percentage <- percentage of corresponding polymer if applicable

### Morphology
Sample_ID <- corresponding Sample ID from Sample sheet

Subsample_ID <- corresponding Subsample ID from Sample sheet

Morphology <- morphologies found in sample

Percentage <- percentage of corresponding polymer if applicable

### Color
Sample_ID <- corresponding Sample ID from Sample sheet

Subsample_ID <- corresponding Subsample ID from Sample sheet

Color <- colors found in sample

Percentage <- percentage of corresponding polymer if applicable

# Policy Research Data
## current-full_gov_domains.csv, ca.gov-202207061909.xlsx, & california.gov-202207061913.xlsx
Data related to better understanding the policy landscape for open source software development in the state. 

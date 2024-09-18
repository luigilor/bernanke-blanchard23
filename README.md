# _What Caused U.S. Postpandemic Inflation? A Fiscal Extension of Bernanke and Blanchard (2023)_
### [Luigi Lorenzoni](mailto:luigi.lorenzoni@studbocconi.it?subject=[GitHub]%Bernanke%20Blanchard%20)
This repository provides codes related to [Bernanke and Blanchard (2023)](https://www.brookings.edu/wp-content/uploads/2023/04/bernanke-blanchard-conference-draft_5.23.23.pdf). Firstly, I provide a replication package in R for all the results of the paper. Secondly, I provide the code for a fiscal extension of their empirical model,which constitutes the core of my [undergraduate thesis](https://www.dropbox.com/scl/fi/b37gdpdzzeugwhygr2tqg/tesi_lorenzoni_def.pdf?rlkey=h1hlmgj1177jjxcz2m4cpgpr6&st=rxwyc3uq&dl=0). I suggest to download the whole [empirics](empirics) folder on your environment and open from there the scripts on your R machine. 

## Data
* [data_us.csv](empirics/data/data_us.csv) is the dataset I import into my code. It is drawn from the replication package provided by Bernanke and Blanchard with the addition of the fiscal variable
* [auxil](empirics/auxil) provides some useful auxiliary files for the output

## Code
* [bl_be_us.R](empirics/code/bl_be_us.R) provides the standard replication of the paper
* [bl_be_us_boot.R](empirics/code/bl_be_us_boot.R) provides the replication of model implied price inflation responses with confidence intervals
* [bl_be_us_fiscal.R](empirics/code/bl_be_us_fiscal.R) proposes a "fiscal extension" of the model
* [bl_be_us_fiscal_boot.R](empirics/code/bl_be_us_fiscal_boot.R) derives the extended model implied responses of price inflation with confidence intervals

## Paper

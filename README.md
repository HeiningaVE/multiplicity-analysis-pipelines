# multiplicity-analysis-pipelines
This project reruns the 2160 regressions from the following paper:
Heininga VE, Oldehinkel AJ, Veenstra R, Nederhof E (2015) I Just Ran a Thousand Analyses: Benefits of Multiple Testing in Understanding Equivocal Evidence on Gene-Environment Interactions. PLoS ONE 10(5): e0125383. https://doi.org/10.1371/journal.pone.0125383

It does so using the associated data being made available through: 
http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0125383.s002

The resulting regression coefficients from these analyses are subsequently used to investigate sources of variation of these coefficients within these datasets.

This is a work in progress and this code comes without any warranty.

## Using the files
### regressions.R
Run regressions.R to replicate the analyses run in the aforementioned paper. The input dataset is available through the above mentioned link but needs to be transformed from SPSS format to .csv format by the user!

### analysis.R
Run analysis.R to do some initial analysis on the resulting data.


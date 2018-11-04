# Refer to basejump.classes/R/AllClasses.R for documentation.
setClass(Class = "EggNOG", contains = "SimpleDataFrameList")
setClass(Class = "Ensembl2Entrez", contains = "DataFrame")
setClass(Class = "Gene2Symbol", contains = "DataFrame")
setClass(Class = "HGNC2Ensembl", contains = "DataFrame")
setClass(Class = "MGI2Ensembl", contains = "DataFrame")
setClass(Class = "PANTHER", contains = "DataFrame")
setClass(Class = "Tx2Gene", contains = "DataFrame")

# Need to convert this to runFeatureCounts() function


# Get counts from aligned SAM files using featureCounts

# install.packages("readxl")
# source("http://bioconductor.org/biocLite.R")
# biocLite("Rsubread")

library(readxl)
library(Rsubread)

samples <- read_excel("samples.xlsx", sheet = 1)

# FASTQ: ~/data/MODEL/rnaseq/DATASET/fastq
# GTF: ~/data/MODEL/genome/gtf

gtf <- getwd()
gtf <- gsub("/rnaseq/.+$", "", gtf, perl = TRUE)
gtf <- file.path(gtf, "genome/gtf")
gtf

if (!file.exists("featureCounts")) {
  dir.create("featureCounts")
}

# Check for paired end files
check <- dir("fastq", pattern="*_2.fastq")
if (identical(check, character(0))) {
  paired <- FALSE
} else {
  paired <- TRUE
}

print("featureCounts is running...")
sink("featureCounts/fc.log", append = FALSE, split = FALSE)
fc <- featureCounts(files = samples$output,
                    annot.ext = gtf,
                    isGTFAnnotationFile = TRUE,
                    nthreads = 12,
                    isPairedEnd = paired,
                    countMultiMappingReads = TRUE)
sink()
save(fc, file = "featureCounts/fc.rda")

source("format_results_functions.R")
library(reticulate)

reticulate::source_python("..//notebooks//globals.py")

#### Format mixed models. ####

pwidth <- 5; pheight <- 5 # Set the plots width and height.

res1.mixed <- summarize.iterations(DATASET_PATH, "mixed")

res2.mixed <- summarize.all(DATASET_PATH, "mixed")

pairwise.occurrences(DATASET_PATH, "mixed", pwidth, pheight)

plot.confusion.matrix(DATASET_PATH, "mixed", pwidth, pheight)

latex.summary(DATASET_PATH, "mixed")

#### Format ud models. ####

res0.ud <- summarize.users(DATASET_PATH, "ud")

res1.ud <- summarize.it.method(DATASET_PATH, "ud")

res2.ud <- summarize.all(DATASET_PATH, "ud")

pairwise.occurrences(DATASET_PATH, "ud", pwidth, pheight)

plot.confusion.matrix(DATASET_PATH, "ud", pwidth, pheight)

latex.summary(DATASET_PATH, "ud")

#### Format ui models. ####

res0.ui <- summarize.users(DATASET_PATH, "ui")

res1.ui <- summarize.it.method(DATASET_PATH, "ui")

res2.ui <- summarize.all(DATASET_PATH, "ui")

pairwise.occurrences(DATASET_PATH, "ui", pwidth, pheight)

plot.confusion.matrix(DATASET_PATH, "ui", pwidth, pheight)

latex.summary(DATASET_PATH, "ui")

#### Format uc models. ####

res0.uc <- summarize.users(DATASET_PATH, "uc")

res1.uc <- summarize.it.method(DATASET_PATH, "uc")

res2.uc <- summarize.all(DATASET_PATH, "uc")

pairwise.occurrences(DATASET_PATH, "uc", pwidth, pheight)

plot.confusion.matrix(DATASET_PATH, "uc", pwidth, pheight)

latex.summary(DATASET_PATH, "uc")

#### Format General results. ####

boxplot.pairwise(DATASET_PATH)

lolliplot.pairwise(DATASET_PATH)

#### Generate graph plots ####

graph.occurrences(DATASET_PATH, "mixed", pwidth, pheight)

graph.occurrences(DATASET_PATH, "ud", pwidth, pheight)

graph.occurrences(DATASET_PATH, "ui", pwidth, pheight)

graph.occurrences(DATASET_PATH, "uc", pwidth, pheight)

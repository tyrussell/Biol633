setwd("~/Documents/Deer_SNP_Research/HybridPop")
#
MD_M <- read.delim("MD_M.txt", stringsAsFactors = TRUE)
MD_F <- read.delim("MD_F.txt", stringsAsFactors = TRUE)
WT_M <- read.delim("WT_M.txt", stringsAsFactors = TRUE)
WT_F <- read.delim("WT_F.txt", stringsAsFactors = TRUE)
#
cols <- c("Cell", "Sex")
#
MD_M[cols] <- lapply(MD_M[cols], factor)
MD_F[cols] <- lapply(MD_F[cols], factor)
WT_M[cols] <- lapply(WT_M[cols], factor)
WT_F[cols] <- lapply(WT_F[cols], factor)
#
library(dplyr)
MD_M_sample <- MD_M %>% group_by(Cell) %>% sample_n(size = 1) %>% ungroup()
MD_F_sample <- MD_F %>% group_by(Cell) %>% sample_n(size = 1) %>% ungroup()
WT_M_sample <- WT_M %>% group_by(Cell) %>% sample_n(size = 1) %>% ungroup()
WT_F_sample <- WT_F %>% group_by(Cell) %>% sample_n(size = 1) %>% ungroup()
#all xx_x_sample dataframes contain one random row of each cell
#
total_samples <- rbind(MD_M_sample, MD_F_sample, WT_M_sample, WT_F_sample)
str(total_samples)
#
total_samples <- total_samples[order(total_samples$Cell),]
#
counts <- total_samples %>% group_by(Cell) %>% summarise(no_rows = length(Cell))
#114 cells with all 4 categories of deer = 456 individuals
#
write.table(total_samples, file = "total_samples.txt", sep = "\t", quote = FALSE, row.names = FALSE)
#
#remove 13 cells to make room on plate for a priori hybrids
droprows <- c(20,22,31,38,39,40,41,54,63,118,126,127,138)
final_samples <- total_samples[ ! total_samples$Cell %in% droprows, ]
#
write.table(final_samples, file = "deer_samples.txt", sep = "\t", quote = FALSE, row.names = FALSE)
#
#
###some samples were unable to be located. Need to resample some rows
deersubsampler <- function(x, Block){
  whichcell <- subset(x, Cell==Block)
  result <- whichcell[sample(nrow(whichcell), 1),]
  return(result)
}
#will return one random row from specified df and block
##must repeat for each missing sample
deersubsampler(MD_M, 129)
deersubsampler(MD_F, 110)
deersubsampler(WT_M, 143)
deersubsampler(WT_F, 131)

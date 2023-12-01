library(foreign)

# Path to .arff file.
srcPath <- "C://smartphone_activities//WISDM_ar_v1.1_transformed.arff"

# Path where formatted dataset will be saved.
destPath <- "../data/wisdm/"


df <- read.table(srcPath, 
                 skip = 50, 
                 header = F,
                 sep = ",",
                 comment.char = "", 
                 stringsAsFactors = F)

# Remove columns with corrupted data.
df <- df[,-c(1,33,36,37,38)]

# Select everything but first and last columns. (userid, label)
tmp <- df[,-c(1,ncol(df))]

# Append userid and label at the beginning.
dataset <- cbind(df$V2, df$V46, tmp)
names(dataset)[1:2] <- c("userid","label") 

# Only keep users that performed all activities.
t <- as.matrix(table(dataset$userid, dataset$label))
completeUsers <- as.integer(which(apply(t, 1, function(row){length(which(row != 0))}) == 6))
idxs <- which(dataset$userid %in% completeUsers)
dataset <- dataset[idxs,]

# Only keep users that have at least 5 instances per class.
exclude <- c(6,24) # Users 6 and 24 do not comply.
idxs <- which(!(dataset$userid %in% exclude))
dataset <- dataset[idxs,]

# Save the formatted dataset.
write.csv(dataset, paste0(destPath,"data.csv"), row.names = F, quote = F)

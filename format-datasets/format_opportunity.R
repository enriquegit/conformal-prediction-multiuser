library(data.table)


f.meanDif <- function(x){
  acum <- 0.0
  for(i in 2:length(x)){
    acum <- acum + (x[i] - x[i-1])
  }
  mean(acum)
}

movingAverage <- function(x, n){
  #applies moving average filter to x with window of size n
  N <- length(x); s <- numeric(length=N)
  for(i in n:N){s[i] <- sum(x[(i-n+1):i]) / n}
  for(i in 1:(n-1)){s[i] <- sum(x[i:(i+n-1)]) / n}
  return(s)
}

opp.processUser <- function(path, id, w){
  
  df <- NULL
  
  for(i in 1:5){
    f <- paste0(path,"S",id,"-ADL",i,".dat")
    tmp <- fread(f, sep = " ", header = F)
    df <- rbind(df, tmp)
  }
  
  setkey(df, V244)
  #38, 39, 40 BACK inertial sensor acceleration
  #41, 42, 43 BACK gyroscope
  #125, 126, 127 R-SHOE acceleration
  #64,, 65, 66 RLA inertial sensor acceleration
  x <- 38; y <- 39; z <- 40
  x2 <- 125; y2 <- 126; z2 <- 127
  
  #30 Hz
  # From the documentation, labels are 1,2,4,5
  labels <- c(1,2,4,5)
  userdata <- NULL
  
  for(l in labels){
    tmp <- df[list(l)]
    tmp[[x]] <- movingAverage(tmp[[x]], 10)
    tmp[[y]] <- movingAverage(tmp[[y]], 10)
    tmp[[z]] <- movingAverage(tmp[[z]], 10)
    
    tmp[[x2]] <- movingAverage(tmp[[x2]], 10)
    tmp[[y2]] <- movingAverage(tmp[[y2]], 10)
    tmp[[z2]] <- movingAverage(tmp[[z2]], 10)
    
    starts <- seq(1,nrow(tmp),w)
    starts <- starts[-length(starts)]
    
    for(i in starts){
      block <- tmp[i:(i+w-1), ]
      
      meanX <- mean(block[[x]]); meanY <- mean(block[[y]]); meanZ <- mean(block[[z]])
      sdX <- sd(block[[x]]); sdY <- sd(block[[y]]); sdZ <- sd(block[[z]])
      maxX <- max(block[[x]]); maxY <- max(block[[y]]); maxZ <- max(block[[z]])
      corXY <- cor(block[[x]],block[[y]]); corXZ <- cor(block[[x]],block[[z]]); corYZ <- cor(block[[y]],block[[z]])
      magnitude <- sqrt(block[[x]]^2 + block[[y]]^2 + block[[z]]^2)
      meanMagnitude <- mean(magnitude)
      sdMagnitude <- sd(magnitude)
      auc <- sum(magnitude)
      meanDif <- f.meanDif(magnitude)
      
      #more
      meanX2 <- mean(block[[x2]]); meanY2 <- mean(block[[y2]]); meanZ2 <- mean(block[[z2]])
      sdX2 <- sd(block[[x2]]); sdY2 <- sd(block[[y2]]); sdZ2 <- sd(block[[z2]])
      maxX2 <- max(block[[x2]]); maxY2 <- max(block[[y2]]); maxZ2 <- max(block[[z2]])
      corXY2 <- cor(block[[x2]],block[[y2]]); corXZ2 <- cor(block[[x2]],block[[z2]]); corYZ2 <- cor(block[[y2]],block[[z2]])
      
      magnitude2 <- sqrt(block[[x2]]^2 + block[[y2]]^2 + block[[z2]]^2)
      meanMagnitude2 <- mean(magnitude2)
      sdMagnitude2 <- sd(magnitude2)
      auc2 <- sum(magnitude2)
      meanDif2 <- f.meanDif(magnitude2)
      
      
      res <- data.frame(userid=as.integer(id), label=l, 
                        meanX=meanX, 
                        meanY=meanY, 
                        meanZ=meanZ,
                        sdX=sdX, 
                        sdY=sdY, 
                        sdZ=sdZ,
                        maxX=maxX,
                        maxY=maxY,
                        maxZ=maxZ,
                        corXY=corXY,
                        corXZ=corXZ, 
                        corYZ=corYZ,
                        meanMagnitude=meanMagnitude,
                        sdMagnitude=sdMagnitude,
                        auc=auc, 
                        meanDif=meanDif, 
                        meanX2=meanX2,
                        meanY2=meanY2,
                        meanZ2=meanZ2, 
                        sdX2=sdX2,
                        sdY2=sdY2,
                        sdZ2=sdZ2,
                        maxX2=maxX2,
                        maxY2=maxY2,
                        maxZ2=maxZ2, 
                        corXY2=corXY2,
                        corXZ2=corXZ2,
                        corYZ2=corYZ2, 
                        meanMagnitude2=meanMagnitude2,
                        sdMagnitude2=sdMagnitude2,
                        auc2=auc2, 
                        meanDif2=meanDif2)
      
      userdata <- rbind(userdata, res)
    }
    
  }
  return(userdata)
}

opp.processAll <- function(path,users,w){
  
  dataset <- NULL
  for(u in users){
    print(paste0("processing user: ", u))
    tmp <- opp.processUser(path,u,w)
    dataset <- rbind(dataset,tmp)
  }
  
  dataset <- dataset[complete.cases(dataset),]
  
  return(dataset)
}

# Path where de .dat files are located.
rawPath <- "C://data//raw//OpportunityUCIDataset//dataset//"

# Choose which users to process.
opp.users <- 1:4

# Process the dataset.
dataset <- opp.processAll(rawPath, opp.users, 120)

# map integer labels to strings.
dataset$label[dataset$label==1] <- "Stand"
dataset$label[dataset$label==2] <- "Walk"
dataset$label[dataset$label==4] <- "Sit"
dataset$label[dataset$label==5] <- "Lie"

table(dataset$userid, dataset$label)

# Write resulting dataset.
write.csv(dataset, "../data/opportunity/data.csv", quote = F, row.names = F)

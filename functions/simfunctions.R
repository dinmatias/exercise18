evolvePop <- function(numGen, popSize1, iniFreq1,
                      selReg, relFitness1, relFitness2,
                      mig = FALSE,
                      popSize2, iniFreq2,
                      gf12, gf21){
  
  genoFreq1 <- vector("list", length = (numGen + 1))
  
  if(mig){
    
    genoFreq2 <- vector("list", length = (numGen + 1))
    
  }
    
  withProgress(message = 'Simulating', detail = "Generation 0", value = 0, min = 0, max = numGen, {
    
  for(generation in 1:numGen){
    
    if(generation == 1){
      
      parentalPool1 <- iniPop(freqA = iniFreq1, popSize = popSize1)
      
      genoFreq1[[generation]] <- parentalPool1
      
      if(mig){
        parentalPool2 <- iniPop(freqA = iniFreq2, popSize = popSize2)
        
        genoFreq2[[generation]] <- parentalPool2
      }
      
      
    }
    
    # simulate migration prior to mating
    if(mig){
      
      parentalPool1 <- sapply(1:popSize1,
                              function(x){ 
                                migParent(gf = gf21,
                                          setPool = parentalPool1,
                                          migPool = parentalPool2,
                                          setpopSize = popSize1)
                                })
      
      parentalPool2 <- sapply(1:popSize2, 
                              function(x){
                                migParent(gf = gf12, 
                                          setPool = parentalPool2, 
                                          migPool = parentalPool1,
                                          setpopSize = popSize2)
                                })
      
    }
    
    # mate
    if(mig){
      
      parentalPool1 <- nextGeneration(popSize = popSize1,
                                     parentalPool = parentalPool1,
                                     selReg = selReg,
                                     relFitness = relFitness1)
      
      parentalPool2 <- nextGeneration(popSize = popSize2,
                                      parentalPool = parentalPool2,
                                      selReg = selReg,
                                      relFitness = relFitness1)
    }else{
      
      parentalPool1 <- nextGeneration(popSize = popSize1,
                                      parentalPool = parentalPool1,
                                      selReg = selReg,
                                      relFitness = relFitness1)
    }
    
    if(mig){
      
      genoFreq1[[generation + 1]] <- parentalPool1
      
      genoFreq2[[generation + 1]] <- parentalPool2
      
    }else{
      
      genoFreq1[[generation + 1]] <- parentalPool1
      
    }
    
    incProgress(1, detail = paste("Generation", (generation - 1)))
    
    
    
  } # END LOOP
  })
  
  if(mig){
    
    resultPop1 <- lapply(genoFreq1, function(x) table(x))
    
    resultPop1 <- as.data.frame.matrix(do.call(rbind, resultPop1))
    
    resultPop1$Generation <- 0:numGen
    
    resultPop1$freqA <- apply(resultPop1, 1, function(x) ((x[1] * 2) + x[2]) / (2 * sum(x[1:3])))
    
    resultPop2 <- lapply(genoFreq2, function(x) table(x))
    
    resultPop2 <- as.data.frame.matrix(do.call(rbind, resultPop2))
    
    resultPop2$Generation <- 0:numGen
    
    resultPop2$freqA <- apply(resultPop2, 1, function(x) ((x[1] * 2) + x[2]) / (2 * sum(x[1:3])))
    
    result <- data.frame(Generation = resultPop1[ , 4],
                         Pop1AA = resultPop1[ , 1], Pop1Aa = resultPop1[ , 2], 
                         Pop1aa = resultPop1[ , 3], Pop1freqA = resultPop1[ , 5],
                         Pop2AA = resultPop2[ , 1], Pop2Aa = resultPop2[ , 2], 
                         Pop2aa = resultPop2[ , 3], Pop2freqA = resultPop2[ , 5])
    
    return(result)
    
  }else{

    resultPop1 <- lapply(genoFreq1, function(x) table(x))
    
    resultPop1 <- as.data.frame.matrix(do.call(rbind, resultPop1))
    
    resultPop1$Generation <- 0:numGen
    
    resultPop1$freqA <- apply(resultPop1, 1, function(x) ((x[1] * 2) + x[2]) / (2 * sum(x[1:3])))
    
    result <- data.frame(Generation = resultPop1[ , 4],
                         Pop1AA = resultPop1[ , 1], Pop1Aa = resultPop1[ , 2], 
                         Pop1aa = resultPop1[ , 3], Pop1freqA = resultPop1[ , 5])
    
    return(result)
    
  }
  
}

selection <- function(popSize, parentalPool, relFitness = c(1, 1, 1)){
  
  accGeno <- factor(levels = levels(parentalPool))
  
  iniPoolIndex <- 1:popSize
  
  numPar = 1
  
  while(numPar <= 2){
    
    indexPar <- sample(x = iniPoolIndex, size = 1, replace = FALSE)
    
    parGeno <- parentalPool[indexPar]
    
    if(relFitness[parGeno] > runif(n = 1, min = 0, max = 1)){
      
      iniPoolIndex <- iniPoolIndex[-indexPar]
      accGeno[numPar] <- parGeno
      numPar = numPar + 1
      
    }
    
  }
  
  return(accGeno)
  
}


chooseParent <- function(popSize, parentalPool, selReg = FALSE, relFitness = c(1, 1, 1)){
  
    # Selection
    if(selReg){
      
      accGeno <- selection(popSize = popSize, parentalPool = parentalPool, 
                           relFitness = relFitness)

    }else{
      
      indexPar <- sample(x = 1:popSize, size = 2, replace = FALSE)
      
      accGeno <- parentalPool[indexPar]
      

    }

  return(accGeno)
  
}

produceOffspring <- function(parentGeno){
  
  alleles <- sapply(strsplit(as.character(parentGeno), ""),
                    function(x) sample(x, 1))
  
  offspring <- paste(sort(alleles, decreasing = TRUE), collapse = "")
  
  return(offspring)
  
}

iniPop <- function(freqA, popSize){
  
  x <- sapply(1:round(popSize), 
              function(x) { sample(x = c("A", "a"), size = 2, 
                                   prob = c(freqA, (1- freqA)), 
                                   replace = TRUE) })
  
  x <- t(x)
  
  x <- paste(x[ , 1], x[ , 2], sep = "")
  
  x <- ifelse(x == "aA", "Aa", x)
  
  x <- factor(x, levels = c("AA", "Aa", "aa"))
  
  levels(x) <- c("AA", "Aa", "aa")
  
  return(x)
  
}


nextGeneration <- function(popSize, parentalPool, selReg, relFitness = c(1, 1, 1)){
  
  x <- sapply(1:popSize, function(x) {
    
    parentGeno <- chooseParent(popSize = popSize,
                               parentalPool = parentalPool, 
                               selReg = selReg, 
                               relFitness = relFitness)
    
    x <- produceOffspring(parentGeno = parentGeno)
    
    return(x)
    
  })
  
  x <- factor(x, levels = c("AA", "Aa", "aa"))
  
  return(x)
  
}


migParent <- function(gf, setPool, migPool, setpopSize){
  
  if(runif(1, 0, 1) > gf/setpopSize){
    
    parent <- sample(setPool, 1)
    
  }else{
    
    parent <- sample(migPool, 1)
    
  }
  
  return(parent)
  
  
}


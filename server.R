
library(shiny)

source("functions/simfunctions.R")

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(df = NULL)
  
  hover <- reactiveValues(x = NULL,  y = NULL)
  # dataFile <- 
  observeEvent(input$sim, {
    
    # Define Inputs:
    numGen <- as.numeric(input$numGen)
    popSize1 <- as.numeric(input$PopSize1)
    iniFreq1 <- as.numeric(input$InFreq1)
    
    selReg <- input$selcheckbox
    
    if(selReg){
      relFitness1 <- as.numeric(c(input$fitAA, input$fitAa, input$fitaa))
      relFitness2 <- as.numeric(c(input$fitAA, input$fitAa, input$fitaa))
    }else{
      relFitness1 <- relFitness1 <- c(1,1,1)
    }
    
    
    mig <- input$gfcheckbox
    
    if(mig){
      popSize2 <- as.numeric(input$popSize2)
      iniFreq2 <- as.numeric(input$InFreq2)
      gf12 <- as.numeric(input$gfmig12)
      gf21 <- as.numeric(input$gfmig21)
    }else{
      popSize2 <- iniFreq2 <- gf12 <- gf21 <- NULL
    }
    
    bottleneck <- input$botcheckbox
    
    if(bottleneck){
      
      botPost <- as.numeric(input$botPost)
      botPercent <- as.numeric(input$botPercent)
      priorbot <- numGen - botPost
      preBot <- evolvePop(numGen = priorbot,
                          popSize1 = popSize1, iniFreq1 = iniFreq1,
                          mig = mig, popSize2 = popSize2, iniFreq2 = iniFreq2,
                          gf12 = gf12, gf21 = gf21,
                          selReg = selReg, relFitness1 = relFitness1, relFitness2 = relFitness2)
      
      freqPost <- tail(preBot$Pop1freqA, n = 1)
      
      postBot <- evolvePop(numGen = botPost,
                           popSize1 = (popSize1 * botPercent), iniFreq1 = freqPost,
                           mig = mig, popSize2 = popSize2, iniFreq2 = iniFreq2,
                           gf12 = gf12, gf21 = gf21,
                           selReg = selReg, relFitness1 = relFitness1, relFitness2 = relFitness2)
      
      postBot <- postBot[-1, ]
      
      postBot$Generation <- postBot$Generation + priorbot
      
      outfile <- rbind(preBot, postBot)
      
    }else{
      
      outfile <- evolvePop(numGen = numGen,
                           popSize1 = popSize1, iniFreq1 = iniFreq1,
                           mig = mig, popSize2 = popSize2, iniFreq2 = iniFreq2,
                           gf12 = gf12, gf21 = gf21,
                           selReg = selReg, relFitness1 = relFitness1, relFitness2 = relFitness2)
      
      
    }
    
    
    
    
    tempHolder <- as.data.frame(matrix( nrow = 0, ncol = ncol(outfile)),
                                stringsAsFactors = F)
    
    names(tempHolder) <- names(outfile)
    
    values$df <- tempHolder
    values$results <- outfile
    outfile
    
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = "Simulations Complete")
    
    showNotification(ui = "Simulation Complete", duration = NULL)
    
    
  })
  
  
  output$frequencyPlot <- renderPlot({
    
    if(!is.null(values$results)){
      # x <- dataFile()
      x <- values$results
      
      colr <- rgb(red = 1, green = 0, blue = 0, alpha = 0.05, names = NULL, maxColorValue = 1)
      
      plot(x = x$Generation, y = x$Pop1freqA,
           pch = 20, col = colr, ylim = c(0,1),
           ylab = "Frequency Allele A ", xlab = "Generations",
           cex = 1.5, cex.lab = 1.5)
      
      lines(x = x$Generation, y = x$Pop1freqA, col = "red", lwd = 2)
      
      # abline(v = input$botPrior, lwd = 2, lty = 2, col = "grey")
      
      if(input$gfcheckbox){
        colrpop2 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.05, names = NULL, maxColorValue = 1)
        points(x = x$Generation, y = x$Pop2freqA, col = colrpop2, pch = 20, cex = 1.5)
        lines(x = x$Generation, y = x$Pop2freqA, col = "blue")
      }
      if(input$botcheckbox){
        abline(v = as.numeric(input$botPost), lty = 2, lwd = 1.5, col = "grey")
      }
      
    }else{
      
      
      colr <- rgb(red = 1, green = 0, blue = 0, alpha = 0.05, names = NULL, maxColorValue = 1)
      
      plot(x = 1:100, y = rep(0.5, 100),
           pch = 20, col = colr, ylim = c(0,1),
           ylab = "Frequency Allele A ", xlab = "Generations",
           cex = 1.5, cex.lab = 1.5, type = "n")
      
      
    }
    
  })
  
  output$value <- renderPrint({ input$driftcheckbox })
  
  
  sampleList <- eventReactive(input$plot_brush, {
    
    # resTable <- dataFile()
    resTable <- values$results
    
    res <- brushedPoints(resTable[ , c(1,5)], input$plot_brush, "Generation", "Pop1freqA")
    
    if (nrow(res) == 0)
      return()
    
    if(nrow(res) > 10){
      subsamp <- 10
    }else{
      subsamp <- nrow(res)
    }
    
    censusList <- res[1:subsamp, ]
    censusList
    
  })
  
  # output$plot_brushedpoints <- renderTable({
  #   x <- sampleList()
  #   data.frame(Generation = x$Generation)
  # })
  
  output$plot_brushedpoints <- renderText({
    x <- sampleList()
    paste(x$Generation, collapse = ",")
  })
  
  observeEvent(input$censusList, {
    
    temp <- values$df
    
    # alldata <- dataFile()
    alldata <- values$results
    
    subData <- sampleList()
    
    x <- alldata[alldata$Generation %in% subData$Generation, ]
    
    temp <- rbind(temp, x)
    
    temp <- temp[order(temp$Generation),]
    temp <- temp[!duplicated(temp$Generation), ]
    values$df <- temp
    
    
  })
  
  output$censusResults <- DT::renderDataTable({
    showTable <- NULL
    tempHolder <- values$df    
    
    rownames(tempHolder) <- NULL
    
    if(ncol(tempHolder) > 6){
      
      showTable <- data.frame(Generation = tempHolder$Generation,
                              AA = apply(tempHolder[ , c(2, 6)], 1, sum),
                              Aa = apply(tempHolder[ , c(3, 7)], 1, sum),
                              aa = apply(tempHolder[ , c(4, 8)], 1, sum),
                              stringsAsFactors = FALSE)
      
      
    }else{
      
      showTable <- tempHolder[ , 1:4]
      names(showTable)[2:4] <- c("AA", "Aa", "aa")
      
    }
    
    DT::datatable(showTable, options = list(sDom  = '<"top">rt<"bottom">ip'),
                  rownames = FALSE)
    
    
  })
  
  output$plot_hoverinfox <- renderText({
    
    if(is.null(input$plot_hover)){
      x <- NA
    }else{
      x <- round(input$plot_hover$x, 0)
    }
    
    x
    
    
  })
  
  output$plot_hoverinfoy <- renderText({
    
    if(is.null(input$plot_hover)){
      y <- NA
    }else{
      y <- round(input$plot_hover$y, 2)
    }
    
    y
    
  })

})

require(shiny)
require(googleVis)
require(knitr)
require(ggplot2)
# devtools::install_github("wesanderson","karthik")
require(wesanderson)


load("data/Ant.rda")
load("data/Birds.rda")
load("data/Seedlings.rda")
load("data/Spider.rda")

source("sub/subfun.R")
source("sub/entropyFunction.R")

shinyServer(function(input, output) {
  tempRD2 <- paste(tempfile(), ".RData", sep="")
  
  loadPaste <- reactive({
    if (input$source == 'import') {
      if (input$datatype == "abu") {
        text <- input$copyAndPaste_abu
      } else {
        text <- input$copyAndPaste_inc
      }
    } else {
      if (is.null(input$files)) {
        text <- "Not_uploaded"
      } else {
        fileName <- input$files$datapath
        
        if (ncol(read.csv(fileName)) == 1) {
          temp <- readChar(fileName, file.info(fileName)$size)
          text <- gsub(pattern="\r", replacement=" ", temp)
        } else {
          da <- read.csv(fileName, header=F)
          
          txt <- character()
          for(i in 1:ncol(read.csv(fileName))) {
            temp <- as.character(da[, i])
            txt[i] <- paste(temp,collapse=" ")
          }
          
          for(i in 2:ncol(read.csv(fileName))) {
            txt[i] <- paste0(" \n", txt[i])
          }
          text <- paste0(txt, collapse=" ") 
        }
      }
    }
    
    ##  把文字檔轉成數個vector而成的list
    Fun <- function(e){
      #  text change to character
      temp <- lapply(readLines(textConnection(text)), function(x) scan(text = x, what='char'))
      out <- list()
      out.name <- 0
      for(i in seq_along(temp)){
        out.name[i] <- temp[[i]][1]
        out[[i]] <- as.numeric(temp[[i]][-1])
      }
      names(out) <- t(data.frame(out.name))
      out
    }
    tryCatch(Fun(e), error=function(e){return()})
  })
  
  #Get Input data name list
  getDataName <- reactive({
    Fun <- function(e){
      out <- loadPaste()
      out.name <- names(out)
      if(is.na(names(out)[1]) == TRUE) {
        dat <- paste("No data")
        dat
      } else {
        dat <- out
        ##  把list裡面的vector取出來!
        for(i in seq_along(out)){
          dat[[i]] <- out.name[i]
        }
        dat        
      }    
    }
    tryCatch(Fun(e), error=function(e){return()})
  })
  
  selectedData <- reactive({
    out <- loadPaste()
    selected <- 1
    dataset <- list()
    for(i in seq_along(input$dataset)){
      selected[i] <- which(names(out)==input$dataset[i])
    }
    for(i in seq_along(selected)){
      k <- selected[i]
      dataset[[i]] <- out[[k]]
    }
    names(dataset) <- input$dataset
    return(dataset)    
  })
  
  
  #Select data
  output$choose_dataset <- renderUI({
    dat <- getDataName()
    selectInput("dataset", "Select dataset:", choices = dat, selected = dat[1], multiple = TRUE, selectize=FALSE)
  })
  
  mymethod <- reactive({
    if (input$datatype == "abu")
      out <- input$method1
    if (input$datatype == "inc")
      out <- input$method2
    return(out)
  })
  
  output$data_summary <- renderPrint({
    dataset <-   selectedData()
    if (input$datatype == "abu") {
      summ <- lapply(dataset, function(x) {
        gvisTable(BasicInfoFun_Ind(x, input$nboot), options=list(width='90%', height='50%', sort='disable'))
      })
      for (i in seq_along(dataset)) {
        summ[[i]]$html <- summ[[i]]$html[-c(3:4)]
      } 
    }
    
    if (input$datatype == "inc") {
      summ <- lapply(dataset, function(x) {
        gvisTable(BasicInfoFun_Sam(x, input$nboot), options=list(width='90%', height='50%', sort='disable'))
      })
      for (i in seq_along(dataset)) {
        summ[[i]]$html <- summ[[i]]$html[-c(3:4)]
      }
    }
    return(summ)
  })
  
  
  computation <- reactive({
    dataset <- selectedData()
    out <- lapply(dataset, function(x) {
      temp <- ChaoEntropyOnline(data=x, datatype=input$datatype, method=mymethod(),
                                nboot=input$nboot, conf=input$conf)
      temp <- round(temp, 3)
            
      ##  Google Vis Table
      output <- as.data.frame(temp)
      tab <- cbind(Methods=rownames(output), output)
      rownames(tab) <- NULL
      tab$Estimator <- sprintf("%1.3f", tab$Estimator)
      tab$'Bootstrap s.e.' <- sprintf("%1.3f", tab$'Bootstrap s.e.')
      tab$'95 % Lower' <- sprintf("%1.3f", tab$'95 % Lower')
      tab$'95 % Upper' <- sprintf("%1.3f", tab$'95 % Upper')
      tab$Estimator <- sprintf("<center>%s</center>", tab$Estimator)
      tab$'Bootstrap s.e.' <- sprintf("<center>%s</center>", tab$'Bootstrap s.e.')
      tab$'95 % Lower' <- sprintf("<center>%s</center>", tab$'95 % Lower')
      tab$'95 % Upper' <- sprintf("<center>%s</center>", tab$'95 % Upper')
      
      gis <- gvisTable(tab, options=list(width='90%', height='60%', allowHtml=TRUE))
      gis$html <- gis$html[-c(3:4)]
      return(list(temp, gis))
    })
    out
  })
   
  output$est <- renderPrint({
    dataset <- selectedData()
    out <- computation()
    excl <- list()
    gtab <- list()
    for (i in seq_along(dataset)) {
      excl[i] <- list(out[[i]][[1]])
      gtab[i] <- list(out[[i]][[2]])
    }
    names(gtab) <- input$dataset
    names(excl) <- input$dataset
    saveRDS(excl, tempRD2)
    return(gtab)
  })
  
  ##  Picture
  output$visualization <- renderPlot({
    dataset <- selectedData()
    out <- computation()
    pic <- list()
    for (i in seq_along(dataset)) {
      temp <- out[[i]][[1]]
      index <- letters[1:nrow(temp)]
      df <- data.frame(index, rownames(temp), temp)
      rownames(df) <- NULL
      colnames(df) <- c("id", "Methods", "Estimator", "SE", "Lower", "Upper")
      p <- ggplot(df, aes(id, Estimator, ymin=Lower, ymax=Upper, colour=id))
      pout <- p + geom_errorbar(width = 0.5, size=2) + geom_point(size=6) + labs(title=names(dataset[i]), x="Methods") + 
        scale_color_manual(values=c(wes.palette(5, "Darjeeling"), 1), name="Methods", breaks=index, labels=rownames(temp)) + 
        scale_x_discrete(breaks=index, labels=rownames(temp))  
      pic[i] <- list(pout)
    }
    print(multiplot4shiny(pic, cols=1))
  })
  
  #Download ChaoEntropy output 
  output$dlest <- downloadHandler(
    filename = function() { paste('output_', Sys.Date(), '_[ChaoEntropy].csv', sep='') },
    content = function(file) { 
      out <- readRDS(tempRD2)
      saveList2csv(out, file)
    }
  )
})

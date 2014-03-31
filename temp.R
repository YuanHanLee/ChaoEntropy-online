require(shiny)
require(shinyapps)
runApp()
Sys.setlocale(locale="en_US.UTF-8")
shinyapps::setAccountInfo(name="yuanhan", token="4881B64B620AD253874E56C55F3E30A6", secret="QfoTdUh2d/n58/uInqLbF6QAD9se40X7vibPrSyB")
deployApp(appName="ChaoEntropy-beta")


## revise picture
text <- "Spider 46 22 17 15 15  9  8  6  6  4  2  2  2  2  1  1  1  1  1  1  1  1  1  1  1  1  \nBirds 752 276 194 126 121 97  95  83  72  44  39  0  16  15  0  13  9  9  9  8  7  4  0  0  2  2  1  1  1"
temp <- lapply(readLines(textConnection(text)), function(x) scan(text = x, what='char'))
out <- list()
out.name <- 0
for(i in seq_along(temp)){
  out.name[i] <- temp[[i]][1]
  out[[i]] <- as.numeric(temp[[i]][-1])
}
names(out) <- t(data.frame(out.name))
selected <- 1
dataset <- list()
input <- list(dataset=names(out))
for(i in seq_along(input$dataset)){
  selected[i] <- which(names(out)==input$dataset[i])
}
for(i in seq_along(selected)){
  k <- selected[i]
  dataset[[i]] <- out[[k]]
}
names(dataset) <- input$dataset
out <- lapply(dataset, function(x) {
  temp <- ChaoEntropyOnline(data=x)
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


pic <- list()
i <- 1
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



## ChaoEntropy online

####Interactive data analytics using <a href="http://www.r-project.org/" target="_blank">R</a> and <a href="http://www.rstudio.com/shiny/" target="_blank">Shiny</a> by <a href="http://www.rstudio.com/" target="_blank">Rstudio</a>.####

- Author: Anne Chao ; Y. H. Lee ; K. S. Tseng ; Y. T. Wang 
- Maintainer: Anne Chao chao@stat.nthu.edu.tw
- URL: http://chao.stat.nthu.edu.tw/blog/


### Run the development version of the web app in the browser window
- Required: A modern browser (e.g., Chrome, Firefox, or Safari). Internet Explorer is not supported.
  
  <a href="http://spark.rstudio.com/mikelee/ChaoEntropy/" target="_blank">Click here link to ChaoEntropy web App</a>

### Run the development version of the web app locally

- Required: `R`
- Required: A modern browser (e.g., `Chrome`, `Firefox`, or `Safari`). :warning: Internet Explorer may not supported.
- Suggested: `Rstudio`

Start R(studio) and copy-and-paste the commands below:

```coffee
if (!require("shiny"))
    install.packages("shiny")
shiny::runGitHub('ChaoEntropy-online', 'YuanHanLee')
```

  


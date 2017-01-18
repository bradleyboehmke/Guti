################ RDA DEPLOYMENT ################
# RDA support is still under development.
#
# From any directory you can build an RDA from your project by running:
# source(<this file>)
# createRDA(<your app directory>, <bdp version>)
# Supported BDP versions are:
#   2.3.5
#   2.3.6
#   2.3.8
#   2.3.9
#
# This will an RDA file that can be loaded via deplicon or the RDA deployer.
# build/<your app directory>_rda.zip           - The RDA which can be loaded into Deplicon for deployment.
################################################

require('packrat')

pkgsInBDP2.3.5 <- c('BH', 'Cairo', 'DBI', 'DT', 'NLP', 'R6', 'RColorBrewer', 'RCurl', 'RJSONIO', 'RSQLite', 'Rcpp', 'RcppArmadillo', 'RgoogleMaps', 'assertthat', 'base64enc', 'bcp', 'bitops', 'brew', 'caTools', 'chron', 'colorspace', 'curl', 'd3heatmap', 'data.table', 'dendextend', 'devtools', 'dichromat', 'digest', 'dplyr', 'evaluate', 'fields','forecast', 'formatR', 'fracdiff', 'geosphere', 'ggmap', 'ggplot2', 'ggvis', 'git2r', 'googleVis', 'gsubfn', 'gtable','highr', 'htmltools', 'htmlwidgets', 'httpuv', 'httr', 'jpeg', 'jsonlite', 'knitr', 'labeling', 'lazyeval', 'leaflet', 'lubridate', 'magrittr', 'mapproj', 'maps', 'maptools', 'markdown', 'memoise', 'mime', 'munsell', 'networkD3', 'plyr','png', 'proto', 'quadprog', 'rCharts', 'rChartsCalendar', 'rJava', 'raster', 'reshape2', 'rgl', 'rjson', 'rmarkdown', 'roxygen2', 'rstudioapi', 'rversions', 'rworldmap', 'scales', 'shiny', 'shinyBS', 'shinyRGL', 'shinySignals', 'shinydashboard', 'slam', 'sp', 'spam', 'sqldf', 'stringi', 'stringr', 'timeDate', 'tseries', 'whisker', 'xml2','xtable', 'yaml', 'zoo')

pkgsInBDP2.3.6 <- c("abind","acepack","ape","assertthat","base64enc","bcp","bdsmatrix","BH","bit","bit64","bitops","brew","broom","car","caret","caTools","chron","circlize","classInt","coda","colorspace","corrplot","covr","crayon","curl","d3heatmap","DAAG","data.table","date","DBI","deldir","dendextend","dendextendRcpp","DendSer","devtools","dichromat","digest","doBy","doParallel","dplyr","dynamicTreeCut","e1071","estimability","evaluate","evd","expsmooth","fastmatch","fields","filehash","fma","FNN","foreach","forecast","formatR","Formula","fpp","fracdiff","fts","gclus","gdata","geosphere","GGally","ggmap","ggplot2","ggplot2movies","ggthemes","ggvis","git2r","GlobalOptions","goftest","googleVis","gplots","gridBase","gridExtra","gstat","gsubfn","gtable","gtools","hexbin","highlight","highr","Hmisc","htmltools","htmlwidgets","httpuv","httr","ibdreg","igraph","inline","intervals","irlba","iterators","its","jpeg","jsonlite","kernlab","knitr","labeling","Lahman","latticeExtra","lazyeval","leaflet","LearnBayes","lintr","lme4","lmtest","loa","longmemo","lsmeans","lubridate","magrittr","mapdata","mapproj","maps","maptools","markdown","MASS","MatrixModels","maxLik","memoise","microbenchmark","mime","minqa","miscTools","mnormt","mondate","multcomp","munsell","mvtnorm","networkD3","nloptr","NLP","NMF","nycflights13","openssl","pbkrtest","PBSmapping","permute","pkgKitten","pkgmaker","plm","plotly","plyr","png","polyclip","praise","profdpm","proto","pryr","psych","pvclust","qap","quadprog","quantreg","R6","raster","rasterVis","rbenchmark","R.cache","rCharts","rChartsCalendar","RColorBrewer","Rcpp","RcppArmadillo","RcppEigen","RCurl","readr","registry","reshape","reshape2","rex","rgl","RgoogleMaps","RH2","rJava","RJDBC","rjson","RJSONIO","rmarkdown","R.methodsS3","rngtools","R.oo","roxygen2","R.rsp","RSclient","RSQLite","rstudioapi","RUnit","R.utils","rversions","rworldmap","rworldxtra","sandwich","scales","seriation","shape","shiny","shinyBS","shinydashboard","slam","sp","spacetime","spam","SparseM","spatstat","spdep","sphet","splm","sqldf","stringdist","stringi","stringr","strucchange","svUnit","tensor","testit","testthat","TH.data","tidyr","tikzDevice","timeDate","timeSeries","tis","truncdist","tseries","TSP","tufte","urca","vcd","vegan","viridis","whisker","withr","XML","xml2","xtable","xts","yaml","zoo","base","boot","class","cluster","codetools","compiler","datasets","DT","foreign","graphics","grDevices","grid","KernSmooth","lattice","MASS","Matrix","methods","mgcv","nlme","nnet","parallel","rpart","spatial","splines","stats","stats4","survival","tcltk","tools","utils")

pkgsInBDP2.3.9 <- c(pkgsInBDP2.3.6, "c3Services")

createRDA <- function(appDirectory, bdpVersion = "2.3.6") {
  if (bdpVersion == "2.3.9") {
    createRDA2.3.9(appDirectory)
    return()
  }
  oldWD <- getwd()
  buildDir <- file.path(oldWD, "build")
  tryCatch ({

    # Check the validity of the app
    message("Verifying app.")
    appFile <- file.path(appDirectory,"/app.R")
    if (!file.exists(appDirectory) || !file.exists(appFile)) {
      stop(paste("Invalid app directory (", appDirectory, "). Please provide a directory with a valid app.R file."))
    }

    message("Checking dependencies.")
    pkgsInBDP <- switch (bdpVersion,
    "2.3.5" = pkgsInBDP2.3.5,
    "2.3.6" = pkgsInBDP2.3.6,
    "2.3.8" = pkgsInBDP2.3.6)

    pkgsInAnalytic <- packrat:::appDependencies(appDirectory)
    unincludedPkgs <- setdiff(pkgsInAnalytic, c(pkgsInBDP,'packrat'))
    if (length(unincludedPkgs) != 0) {
      message(paste("Warning: This project contains R libraries that are not included in the target BDP version (",
      bdpVersion,
      "). Your project may not function when deployed. ",
      paste(c("Missing libraries:", unincludedPkgs), collapse="\n")))
    }


    if (!dir.exists(buildDir))
    dir.create(buildDir)
    #unlink(buildDir, recursive = T, force = T)

    file.copy(appDirectory, buildDir, recursive=TRUE)

    appName <- basename(appDirectory)
    cat("config$isRunningInProduction <- TRUE", file=file.path(buildDir, appName, "config.R"), append = TRUE, sep = "\n")
    # set appDirectory to build location
    appDirectory <- file.path(buildDir, appName)


    # This will throw errors if the app does not load up in the users workspace.
    # It is a quick check to ensure that we are building a valid RDA, and it provides us with the config from the app.
    setwd(appDirectory)
    system2('touch', 'restart.txt', stdout = F, stderr = F)
    source("config.R", TRUE)

    if (!exists('config') || !('analyticName' %in% names(config))) {
      stop(paste("Invalid app directory (", appDirectory, "). Please provide a directory with a valid app.R file."))
    }


    # Create the shiny app tarball
    appName <- basename(appDirectory)
    message("Creating RDA.")
    rdaDir <- file.path(buildDir, paste(appName, "_rda", sep=""))
    if (dir.exists(rdaDir))
    unlink(rdaDir, recursive = T, force = T)
    dir.create(rdaDir)

    rdaBaseDir <- basename(rdaDir)

    tarName <- paste(appName, ".tar.gz", sep="")
    tar(file.path(rdaDir, tarName), ".", compression = 'gzip', tar="tar")


    # Create the RDA manifest
    version <- if ('analyticVersion' %in% names(config)) {
      config$analyticVersion
    } else "1.0"
    title <- if ('title' %in% names(config)) {
      config$title
    } else config$analyticName
    fileName <- file.path(rdaBaseDir, tarName)

    manifest <- createRDAManifest(title, config$analyticName, version, fileName)

    rdaFile <- file.path(rdaDir, "ShinyApp.rda")
    file.create(rdaFile)
    write(manifest, rdaFile)

    # Move back to the origional directory to ensure that the zip file uses a local path for the RDA directory
    setwd(buildDir)
    zip(paste(rdaBaseDir,".zip",sep=""), rdaBaseDir)
  }, warning = function(w) {
    message(paste("Warning: ",w))
  }, error = function(e) {
    message(paste("Error creating RDA: ",geterrmessage()))
  }, finally = {
    message(paste("Removing temp directory : ", buildDir))
    unlink(file.path(buildDir, appName), recursive = T, force =T)
    message(paste("Removing temp directory : ", rdaDir))
    unlink(rdaDir, recursive = T, force =T)
    setwd(oldWD)
  })

}

createRDAManifest <- function(descriptiveName, deploymentName, version, fileLocation) {
  manifest <-  sprintf('{
    "name": "%s",
    "appKey": "%s",
    "version": "%s",
    "deployments": [
      {
        "name": "%s",
        "target": "shiny",
        "configuration": {
          "appKey": "%s",
          "shinyApps": [
            "%s"
          ]
        }
      }
    ]
  }', descriptiveName, deploymentName, version, descriptiveName, deploymentName, fileLocation)
}

createRDA2.3.9 <- function(appDirectory, bdpVersion = "2.3.9") {
  oldWD <- getwd()
  buildDir <- file.path(oldWD, "build")
  tryCatch ({

    # Check the validity of the app
    message("Verifying app.")
    appFile <- file.path(appDirectory,"/app.R")
    if (!file.exists(appDirectory) || !file.exists(appFile)) {
      stop(paste("Invalid app directory (", appDirectory, "). Please provide a directory with a valid app.R file."))
    }

    message("Checking dependencies.")
    pkgsInBDP <- pkgsInBDP2.3.9

    pkgsInAnalytic <- packrat:::appDependencies(appDirectory)
    unincludedPkgs <- setdiff(pkgsInAnalytic, c(pkgsInBDP,'packrat'))
    if (length(unincludedPkgs) != 0) {
      message(paste("Warning: This project contains R libraries that are not included in the target BDP version (",
      bdpVersion,
      "). Your project may not function when deployed. ",
      paste(c("Missing libraries:", unincludedPkgs), collapse="\n")))
    }


  message(" - Creating temp directories ")
  if (!dir.exists(buildDir))
  dir.create(buildDir)
  #unlink(buildDir, recursive = T, force = T)

  APP_FOLDER <- "rshiny"
  appName <- basename(appDirectory)

    rdaDir <- file.path(buildDir, paste(appName, "_rda", sep=""))
    if (dir.exists(rdaDir))
    unlink(rdaDir, recursive = T, force = T)
    dir.create(rdaDir)

    rdaBaseDir <- basename(rdaDir)

    message(paste("appDirectory: ",appDirectory))
    message(paste("rdaDir: ",rdaDir))
    message(paste("buildDir: ",buildDir))

    # Move analytic to the standard shiny RDA directory structure.
    file.copy(appDirectory, rdaDir, recursive=TRUE)
    message(paste("renaming ", file.path(rdaDir, appName), " -> ", file.path(rdaDir, APP_FOLDER)))
    file.rename(file.path(rdaDir, appName), file.path(rdaDir, APP_FOLDER))

    cat("config$isRunningInProduction <- TRUE", file=file.path(rdaDir, APP_FOLDER, "config.R"), append = TRUE, sep = "\n")
    appDirectory2 <- file.path(rdaDir, APP_FOLDER)

    # This will throw errors if the app does not load up in the users workspace.
    # It is a quick check to ensure that we are building a valid RDA, and it provides us with the config from the app.
    setwd(appDirectory2)
    source("config.R", TRUE)

    if (!exists('config') || !('analyticName' %in% names(config))) {
      stop(paste("Invalid app directory (", appDirectory, "). Please provide a directory with a valid app.R and config.R file."))
    }

    # Ensure we restart the app on re-deployments
    system2('touch', 'restart.txt', stdout = F, stderr = F)

    # Create the RDA manifest
    message("creating manifest")
    manifest <- createRDAManifest2.3.9(APP_FOLDER, config)
    message(manifest)

    rdaFile <- file.path(rdaDir, "ShinyApp.rda")
    file.create(rdaFile)
    write(manifest, rdaFile)

    # Move back to the origional directory to ensure that the zip file uses a local path for the RDA directory
    setwd(buildDir)
    zip(paste(rdaBaseDir,".zip",sep=""), rdaBaseDir)

    message(paste("RDA created: ", paste(buildDir,"/",rdaBaseDir,".zip",sep="")))
  }, warning = function(w) {
    message(paste("Warning: ",w))
  }, error = function(e) {
    message(paste("Error creating RDA: ",geterrmessage()))
  }, finally = {
    message(paste("Removing temp directory : ", file.path(buildDir, appName)))
    unlink(file.path(buildDir, appName), recursive = T, force =T)
    message(paste("Removing temp directory : ", rdaDir))
    unlink(rdaDir, recursive = T, force =T)
    setwd(oldWD)
  })

}

createRDAManifest2.3.9 <- function(app_folder, config) {

  descriptiveName <- if ('title' %in% names(config)) {
    config$title
  } else config$analyticName

  description <- if ('description' %in% names(config)) {
    config$description
  } else ""

  version <- if ('analyticVersion' %in% names(config)) {
    config$analyticVersion
  } else "1.0"

  manifest <-  sprintf('{
  "name": "%s",
  "description": "%s",
  "appKey": "%s",
  "version": "%s",
  "components": [{
    "type": "rshiny",
    "name": "%s",
    "description": "%s",
    "properties": {
      "public": {
        "rshiny.url": "{{{bdp:proxy.url}}}/%s",
        "rshiny.data.fields": "%s",
        "rshiny.data.types": "%s",
        "rshiny.data.tags": "%s"
      }
    },
    "config": {
      "context": "/%s",
      "files": [{
        "src": "%s/*",
        "template": true
      }]
    }
  }]
}', descriptiveName, description, config$analyticName, version,
  # Components
  descriptiveName, description,
    # properties
      config$analyticName,
      paste(config$dataFields, sep=",", collapse = ","),
      paste(config$dataTypes, sep=",", collapse = ","),
      paste(config$tags, sep=",", collapse = ","),
    # config
    config$analyticName, app_folder)
}

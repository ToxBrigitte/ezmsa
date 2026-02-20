server <- function(input, output, session) {
  global <- reactiveValues()
  
  #Opens the file uploaded to the app and stores it in a reactive variable
  observe({
    req(input$data)
    
    file_ext <- tools::file_ext(input$data$name)
    
    if (file_ext %in% c("xls", "xlsx")){
      df <- read_excel(input$data$datapath, col_names = input$header)
      df <- as.matrix(df)
    }else{
      df <- read.csv(input$data$datapath, sep = input$sep, header = input$header, dec = input$dec)
    }
    df <- df[, colSums(is.na(df)) == 0]
    
    global$rawData <- df
    global$nCal <- nrow(df)
    global$nRep <- ncol(df) - 1
    global$matSize <- nrow(df) * (ncol(df) - 1)
    
  })
  
  #Checks which type of weighting to use and stores it in a reactive variable
  observeEvent(input$start, {
    global$w <- input$weight
  })
  
  #Checks which type of regression to use and stores it in a reactive variable
  observeEvent(input$start, {
    global$order <- input$reg
  })
  
  #Checks what are the unit of measurement and stores it in a reactive variable
  observeEvent(input$start, {
    if (input$unit == "") {
      unit <- "No unit"
    } else{
      unit <- input$unit
    }
    global$unit <- unit
  })
  
  #Checks which p-value to use in the statistical test and stores it in a reactive variable
  observeEvent(input$start, {
    global$pvalue <- input$pvalue
  })
  
  #Checks which ucrm to use in the statistical test and stores it in a reactive variable
  observeEvent(input$start, {
    global$ucrm <- input$ucrm
  })
  
  #Input of additionnal specifications to add to the report
  addSpec <- reactive({
    add <- input$add
    return(add)
  })
  
  #Create a matrix for the spiked concentration,
  #with the concentration repeated for the number of replicates
  observeEvent(input$start, {
    nRep <- global$nRep
    vectY <- global$rawData[, ncol(global$rawData)]
    global$Y <- matrix(rep(vectY, each = nRep), ncol = 1)
  })
  
  #Create the X matrix, includes quadratic by default
  observeEvent(input$start, {
    nRep <- global$nRep
    nCal <- global$nCal
    
    vect1 <- matrix(rep(1, global$matSize), ncol = 1)
    singleX <-
      matrix(as.vector(t(global$rawData[, 1:nRep])), ncol = 1)
    sqrX <- singleX * singleX
    
    global$X <- matrix(cbind(vect1, singleX, sqrX), ncol = 3)
  })
  
  observeEvent(input$start, {
    w <- global$w
    matSize <- global$matSize
    
    #Create a diagonal matrix with the right weight
    if (w == 1) {
      global$wMat <- diag(1, matSize, matSize)
    } else if (w == 2) {
      global$wMat <- diag(1/global$X[, 2], matSize, matSize)
    } else if (w == 3 | (w == 5 & global$nRep < 3)) {
      global$wMat <- diag(1/global$X[, 3], matSize, matSize)
    } else {
      varX <- apply(global$rawData[, 1:global$nRep], 1, var)
      varX <- rep(varX, each = global$nRep)
      global$wMat <- diag(1/varX, matSize, matSize)
    }
  })
  
  observeEvent(input$start, {
    xl <- as.matrix(global$X[, 1:2])
    xq <- global$X
    wmat <- global$wMat
    y <- as.matrix(global$Y)
    matsize <- global$matSize
    nrep <- global$nRep
    
    bL <- solve(t(xl) %*% wmat %*% xl) %*% t(xl) %*% wmat %*% y
    bQ <- solve(t(xq) %*% wmat %*% xq) %*% t(xq) %*% wmat %*% y
    uknwL <- abs(bL[1, 1])
    uknwQ <- abs(bQ[1, 1])
    
    yL <- xl %*% bL
    yQ <- xq %*% bQ
    
    yyL <- (y - yL) ^ 2
    yyQ <- (y - yQ) ^ 2
    
    se2L <- sum(wmat %*% yyL) / (matsize - 2)
    se2Q <- sum(wmat %*% yyQ) / (matsize - 3)
    
    vbL <- se2L * (solve(t(xl) %*% wmat %*% xl))
    vbQ <- se2Q * (solve(t(xq) %*% wmat %*% xq))
    
    tTestL <-
      qt(1 - (global$pvalue / 2), matsize - 2, lower.tail = T)
    tTestQ <-
      qt(1 - (global$pvalue / 2), matsize - 3, lower.tail = T)
    
    ucL <- sqrt(vbL[1, 1] + ((global$ucrm) ^ 2))
    ucQ <- sqrt(vbQ[1, 1] + ((global$ucrm) ^ 2))
    
    confIntL <- tTestL * ucL
    confIntQ <- tTestQ * ucQ
    
    b2confInt <-  tTestQ * sqrt(vbQ[3, 3])
    
    global$bL <- bL
    global$bQ <- bQ
    global$ubL <- sqrt(vbL[1, 1])
    global$ubQ <- sqrt(vbQ[1, 1])
    global$ucL <- ucL
    global$ucQ <- ucQ
    global$errorL <- confIntL
    global$errorQ <- confIntQ
    global$uknwL <- abs(uknwL)
    global$uknwQ <- abs(uknwQ)
    global$intUknwL <- c(uknwL - confIntL, uknwL + confIntL)
    global$intUknwQ <- c(uknwQ - confIntQ, uknwQ + confIntQ)
    global$b2CI <- c(bQ[3, 1] - b2confInt, bQ[3, 1] + b2confInt)
  })
  
  observeEvent(input$start, {
    if ((global$b2CI[1] <= 0 &
         global$b2CI[2] >= 0) | global$order == 2) {
      global$best <- 1
    } else{
      global$best <- 2
    }
    
  })
  
  pretty_print_eq <- function(order, b0, b1, b2) {
    # Create formatted string for each term
    term_b2 <-
      if (b2 == 0)
        ""
    else if (b2 == 1)
      "x^2"
    else if (b2 == -1)
      "-x^2"
    else
      paste0(b2, "x^2")
    term_b1 <-
      if (b1 == 0)
        ""
    else if (b1 == 1)
      "x"
    else if (b1 == -1)
      "-x"
    else
      paste0(b1, "x")
    term_b0 <- if (b0 == 0)
      ""
    else
      as.character(b0)
    
    # Combine terms, handling signs
    if (order == 1) {
      terms <- c(term_b1, term_b0)
    } else{
      terms <- c(term_b2, term_b1, term_b0)
    }
    
    result <- "y ="
    
    for (term in terms) {
      if (term != "") {
        if (substr(term, 1, 1) != "-" && nchar(result) > 3) {
          result <- paste(result, "+", term)
        } else {
          result <- paste(result, term)
        }
      }
    }
    
    # If all coefficients are 0
    if (result == "y =")
      result <- "y = 0"
    
    return(result)
    
  }
  
  #Plots the standard calibration plot
  observeEvent(input$start, {
    dataX <- global$X
    dataY <- global$Y
    nRep <- global$nRep
    
    colRep <- paste("Replicate ", c(1:nRep), sep = "")
    
    data <- data.frame(X = dataX[, 2],
                       Y = dataY,
                       Replicates = colRep)
    
    ymax <- max(dataX[, 2])
    xmax <- max(dataY)
    
    #Plots the base of the graph
    base <- ggplot(data, aes(Y, X)) +
      labs(
        title = "Regular Standard Addition Calibration Curve",
        x = paste0("Concentration (", global$unit, ")"),
        y = "Measurement"
      ) +
      xlim(-xmax / 2, xmax + xmax / 2) +
      ylim(-ymax / 2, 2 * ymax)
    
    #If the best type or selected type is quadratic
    if (global$order == 3 | global$best == 2) {
      bQ <- global$bQ
      b0 <- bQ[1, 1]
      b1 <- bQ[2, 1]
      b2 <- bQ[3, 1]
      
      eq <- function(x) {
        racine <- sqrt(b1 ^ 2 - (4 * b2 * (b0 - x)))
        under <- 2 * b2
        
        return((-b1 + racine) / under)
      }
      
      global$plot3 <- base + geom_point(
        aes(fill = Replicates),
        colour = "black",
        pch = 21,
        size = 5,
        alpha = 0.5
      ) +
        geom_function(fun = eq) +
        geom_hline(yintercept =  0, linetype = "dashed") +
        geom_vline(xintercept =  0, linetype = "dashed") +
        annotate(
          geom = "text",
          x = xmax / 2,
          y = ymax,
          label = paste(
            c(
              "y = (- ",
              round(b1, 3),
              " - sqrt( ",
              round(b1 * b1, 3),
              " - (",
              round(4 * b2),
              " * (",
              round(b0, 3),
              " - x)))) / ",
              round(2 * b2, 3)
            )
            ,
            sep = "",
            collapse = ""
          )
        )
      #If the best type or selected type is linear
    } else{
      bL <- global$bL
      b0 <- bL[1, 1]
      b1 <- bL[2, 1]
      
      if (b0 < 0) {
        labelEq <- paste(c("y = (x + ", round(b0, 3) * -1, ") / ", round(b1, 3)),
                         sep = "",
                         collapse = "")
      } else if (b0 == 0) {
        labelEq <- paste(c("y = x / ", round(b1, 3)),
                         sep = "",
                         collapse = "")
      } else{
        labelEq <- paste(c("y = (x - ", round(b0, 3), " ) / ", round(b1, 3)),
                         sep = "",
                         collapse = "")
      }
      
      eq <- function(x) {
        (x - b0) / b1
      }
      
      global$plot3 <- base + geom_point(
        aes(fill = Replicates),
        colour = "black",
        pch = 21,
        size = 5,
        alpha = 0.5
      ) +
        geom_function(fun = eq) +
        geom_hline(yintercept =  0, linetype = "dashed") +
        geom_vline(xintercept =  0, linetype = "dashed") +
        annotate(
          geom = "text",
          x = xmax / 2,
          y = ymax,
          label = labelEq
        )
      
    }
    
  })
  
  #Plots the inverted calibration curve
  observeEvent(input$start, {
    dataX <- global$X
    dataY <- global$Y
    nRep <- global$nRep
    
    colRep <- paste("Replicate ", c(1:nRep), sep = "")
    
    data <- data.frame(X = dataX[, 2],
                       Y = dataY,
                       Replicates = colRep)
    
    ymax <- max(dataY)
    xmax <- max(dataX[, 2])
    
    #Plots the base of the graph
    base <- ggplot(data, aes(X, Y)) +
      labs(
        title = "Inverted Standard Addition Calibration Curve",
        y = paste0("Concentration (", global$unit, ")"),
        x = "Measurement"
      ) +
      xlim(-xmax / 2, xmax + xmax / 2) +
      ylim(-ymax / 2, 2 * ymax)
    
    #If the best type or selected type is quadratic
    if (global$order == 3 | global$best == 2) {
      bQ <- global$bQ
      b0 <- bQ[1, 1]
      b1 <- bQ[2, 1]
      b2 <- bQ[3, 1]
      
      eq <- function(x) {
        b2 * x ^ 2 + b1 * x + b0
      }
      
      global$plot4 <- base + geom_point(
        aes(fill = Replicates),
        colour = "black",
        pch = 21,
        size = 5,
        alpha = 0.5
      ) +
        geom_function(fun = eq) +
        geom_hline(yintercept =  0, linetype = "dashed") +
        geom_vline(xintercept =  0, linetype = "dashed") +
        annotate(
          geom = "text",
          x = xmax / 2,
          y = ymax,
          label =  pretty_print_eq(2, round(b0, 3), round(b1, 3), round(b2, 3))
        )
      
      #If the best type or selected type is linear
    } else{
      bL <- global$bL
      b0 <- bL[1, 1]
      b1 <- bL[2, 1]
      
      eq <- function(x) {
        b1 * x + b0
      }
      
      global$plot4 <- base + geom_point(
        aes(fill = Replicates),
        colour = "black",
        pch = 21,
        size = 5,
        alpha = 0.5
      ) +
        geom_function(fun = eq) +
        geom_hline(yintercept =  0, linetype = "dashed") +
        geom_vline(xintercept =  0, linetype = "dashed") +
        annotate(
          geom = "text",
          x = xmax / 2,
          y = ymax,
          label = pretty_print_eq(1, round(b0, 3), round(b1, 3), round(0, 3))
        )
    }
  })
  
  
  #Checks how many checkbox are checked for the data to include in the report
  nbCheck <- reactive({
    check1 <- length(input$regReport)
    check2 <- length(input$mesReport)
    check3 <- length(input$unkReport)
    
    toReturn <- check1 + check2 + check3
    
    return(toReturn)
  })
  
  #If all the checkbox are checked for the data to include in the report,
  #checks the "select all" box, or unchecks it if not
  observeEvent(nbCheck(), {
    if (nbCheck() == 10) {
      updateCheckboxInput(inputId = "all",
                          value = T)
    } else{
      updateCheckboxInput(inputId = "all",
                          value = F)
    }
    
  })
  
  #If the "select all" box is checked, checks everything, else unchecks everything
  observeEvent(input$all, {
    if (input$all) {
      updateCheckboxGroupInput(inputId = "regReport", selected = c(1:4))
      updateCheckboxGroupInput(inputId = "mesReport", selected = c(1:3))
      updateCheckboxGroupInput(inputId = "unkReport", selected = c(1:3))
    }
  })
  
  #Pretty print the input raw data
  prettyRawData <- reactive({
    col <- paste0("replicate ", 1:global$nRep)
    col <-
      paste(c(col, paste(
        "Spiked Concetration (", global$unit, ")"
      )), sep = "")
    
    df <- data.frame(global$rawData)
    colnames(df) <- col
    
    ordre <- c(global$nRep + 1, 1:global$nRep)
    
    df <- df[, ordre]
    
    return(df)
  })
  
  #Set the list of data to include in the report
  params <- reactive({
    if (global$order == 3 | global$best == 2) {
      conc <- (round(global$uknwQ, 5))
    } else{
      conc <- (round(global$uknwL, 5))
    }
    
    #Checks if the user wants the raw data in the report or not
    if (input$raw_data_report) {
      raw <- prettyRawData()
    } else{
      raw <- ""
    }
    
    #Check which plot to add to the report and adds it
    if (input$graph_report == 1) {
      stdPlot <- global$plot3
      invPlot <- global$plot4
      boolStd <- T
      boolInv <- T
      
    } else if (input$graph_report == 2) {
      stdPlot <- global$plot3
      invPlot <- ""
      boolStd <- T
      boolInv <- F
    } else if (input$graph_report == 3) {
      stdPlot <- ""
      invPlot <- global$plot4
      boolStd <- F
      boolInv <- T
    }
    
    #Checks if the user wants to include the weight in the report
    if (1 %in% input$regReport) {
      weightRep <- global$w
    } else{
      weightRep <- ""
    }
    
    
    #checks if the user wants to include the confidence limits on b2 in the report
    if (2 %in% input$regReport) {
      confb2 <-
        paste(c(
          "[",
          round(global$b2CI[1], 5),
          " ; ",
          round(global$b2CI[2], 5),
          "]"
        ),
        sep = "",
        collapse = "")
    } else{
      confb2 <- ""
    }
    
    
    #Checks if the user wants to include the order of regression to the report
    regOrder <- ""
    if (3 %in% input$regReport) {
      if (global$order == 3 | global$best == 2) {
        regOrder <- 2
      } else{
        regOrder <- 1
      }
    }
    
    
    #Checks if the user want to include the equation of the regression to the report
    b2 <- ""
    b1 <- ""
    b0 <- ""
    equa <- FALSE
    if (4 %in% input$regReport) {
      if (global$order == 3 | global$best == 2) {
        bQ <- global$bQ
        b2 <- round(bQ[3, 1], 3)
        b1 <- round(bQ[2, 1], 3)
        b0 <- round(bQ[1, 1], 3)
      } else{
        bL <- global$bL
        b2 <- ""
        b1 <- round(bL[2, 1], 3)
        b0 <- round(bL[1, 1], 3)
      }
      equa <- TRUE
    }
    
    #Checks if the user wants to include the ucrm to the report
    if (1 %in% input$mesReport) {
      ucrm <- global$ucrm
    } else{
      ucrm <- ""
    }
    
    #Checks if the user wants to include the ub0 to the report
    ub0 <- ""
    if (2 %in% input$mesReport) {
      if (global$order == 3 | global$best == 2) {
        ub0 <-  round(global$ubQ, 5)
      } else{
        ub0 <-  round(global$ubL, 5)
      }
    }
    
    
    #Checks if the user wants to include de uc to the report
    uc <- ""
    if (3 %in% input$mesReport) {
      if (global$order == 3 | global$best == 2) {
        uc <-  round(global$ucQ, 5)
      } else{
        uc <-  round(global$ucL, 5)
      }
    }
    
    
    #Checks if the user wants to include the confidence level to the report
    if (3 %in% input$unkReport) {
      conf <-
        paste(c((1 - global$pvalue) * 100, "%"), sep = "", collapse = "")
    } else{
      conf <- ""
    }
    
    
    #checks if the user wants to include the expend uncertainty to the report
    expU <- ""
    if (1 %in% input$unkReport) {
      if (global$order == 3 | global$best == 2) {
        expU <- round(as.numeric(global$errorQ), 5)
      } else{
        expU <- round(as.numeric(global$errorL), 5)
      }
    }
    
    
    #Checks if the user wants to include de limits of the confidence level
    #of the calculated concentration
    evalConf <- F
    low <- ""
    upp <- ""
    if (2 %in% input$unkReport) {
      if (global$order == 3 | global$best == 2) {
        low <- round(global$intUknwQ[1], 5)
        upp <- round(global$intUknwQ[2], 5)
      } else{
        low <- round(global$intUknwL[1], 5)
        upp <- round(global$intUknwL[2], 5)
      }
      evalConf <- T
    }
    
    params <- list(
      "docTitle" = input$reportName,
      "docDate" = Sys.Date(),
      "unit" = global$unit,
      "rawData" = prettyRawData(),
      "stdPlot" = stdPlot,
      "invPlot" = invPlot,
      "evalStd" = boolStd,
      "evalInv" = boolInv,
      "concentration" = conc,
      "addSpec" = addSpec(),
      "weight" = weightRep,
      "confb2" = confb2,
      "order" = regOrder,
      "b2" = b2,
      "b1" = b1,
      "b0" = b0,
      "equation" = equa,
      "ucrm" = ucrm,
      "ub0" = ub0,
      "uc" = uc,
      "confLev" = conf,
      "expUnc" = expU,
      "conLow" = low,
      "conUpp" = upp,
      "confLim" = evalConf
    )
    return(params)
  })
  
  #At the click of the start button, displays the result of the analysis
  observeEvent(input$start, {
    output$stdPlot <- renderPlot({
      global$plot3
    })
    
    output$invPlot <- renderPlot({
      global$plot4
    })
    
    output$weightRes <- renderText({
      #Select the weight used in the analysis
      if (global$w == 1) {
        weightOut <-  "1 (no weight)"
      } else if (global$w == 2) {
        weightOut <-  "1/x"
      } else if (global$w == 3 |
                 (global$w == 5 & global$nRep < 3)) {
        weightOut <- "1/x^2"
      } else {
        weightOut <- "1/variance"
      }
      
      return(weightOut)
      
    })
    
    output$confb2Res <- renderText({
      conIntb2 <-
        paste(c(
          "[",
          round(global$b2CI[1], 5),
          " ; ",
          round(global$b2CI[2], 5),
          "]"
        ),
        sep = "",
        collapse = "")
      
      return(conIntb2)
      
    })
    
    output$orderRes <- renderText({
      if (global$order == 3 | global$best == 2) {
        regOrder <- "2 (Quadratic)"
      }
      else{
        regOrder <- "1 (Linear)"
      }
      
      return(regOrder)
    })
    
    output$eqRes <- renderText({
      if (global$order == 3 | global$best == 2) {
        bQ <- global$bQ
        
        regEq <-
          pretty_print_eq(2, round(bQ[1, 1], 3), round(bQ[2, 1], 3), round(bQ[3, 1], 3))
      }
      else{
        bL <- global$bL
        regEq <-
          pretty_print_eq(1, round(bL[1, 1], 3), round(bL[2, 1], 3), 0)
      }
      
      return(regEq)
    })
    
    output$ucrmRes <- renderText(global$ucrm)
    
    output$b0Res <- renderText({
      if (global$order == 3 | global$best == 2) {
        ub0 <- round(global$ubQ, 5)
      }
      else{
        ub0 <- round(global$ubL, 5)
      }
      
      return(ub0)
    })
    
    output$stduncRes <- renderText({
      if (global$order == 3 |
          global$best == 2) {
        stderror <- paste0(round(global$ucQ, 5), " ", global$unit)
      }
      else{
        stderror <- paste0(round(global$ucL, 5), " ", global$unit)
      }
      
      return(stderror)
    })
    
    output$expRes <- renderText({
      if (global$order == 3 |
          global$best == 2) {
        toRet <- round(as.numeric(global$errorQ), 5)
      }
      else{
        toRet <- round(as.numeric(global$errorL), 5)
      }
      expUnc <- sprintf("%f %s", toRet, global$unit)
      
      return(expUnc)
    })
    
    output$lowRes <- renderText({
      if (global$order == 3 |
          global$best == 2) {
        low <- paste(round(global$intUknwQ[1], 5), " ", global$unit)
      }
      else{
        low <- paste(round(global$intUknwL[1], 5), " ", global$unit)
      }
      
      return(low)
      
    })
    
    output$uppRes <- renderText({
      if (global$order == 3 |
          global$best == 2) {
        upp <- paste(round(global$intUknwQ[2], 5), " ", global$unit)
      }
      else{
        upp <- paste(round(global$intUknwL[2], 5), " ", global$unit)
      }
      
      return(upp)
      
    })
    
    output$calcRes <- renderText({
      if (global$order == 3 |
          global$best == 2) {
        calConc <- paste(round(global$uknwQ, 5), " ", global$unit)
      }
      else{
        calConc <- paste(round(global$uknwL, 5), " ", global$unit)
      }
      
      return(calConc)
    })
    
    output$confRes <-
      renderText(paste(c((
        1 - global$pvalue
      ) * 100, "%"),
      sep = "",
      collapse = ""))
    
    output$dataOut <- renderTable({
      prettyRawData()
    })
    
  })
  
  #Generate and download the report in html format
  output$reportHTML <-
    downloadHandler(
      filename = function() {
        paste(input$reportName, ".html", sep = "")
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = T)
        
        #Render from the report from the report.Rmd template file with the selected parameters
        #from the user
        withProgress(message = "Generating report...", {
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = params(),
            output_format = "html_document" ,
            envir = new.env(parent = globalenv())
          )
        })
      }
      ,
      contentType = "text/html"
    )
  
  #Generate and download the report in pdf format
  output$reportPDF <-
    downloadHandler(
      filename = {
        paste(input$reportName, ".pdf", sep = "")
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = T)
        
        #Render from the report from the report.Rmd template file with the selected parameters
        #from the user
        withProgress(message = "Generating report...", {
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = params(),
            output_format = "pdf_document" ,
            envir = new.env(parent = globalenv())
          )
        })
      }
      ,
      contentType = "application/pdf"
    )
  
  #Generate and download the report in the word format
  output$reportDOCX <-
    downloadHandler(
      filename = {
        paste(input$reportName, ".docx", sep = "")
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = T)
        
        withProgress(message = "Generating report...", {
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = params(),
            output_format = "word_document" ,
            envir = new.env(parent = globalenv())
          )
        })
      }
      ,
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
  
  
}
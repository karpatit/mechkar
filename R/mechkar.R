
############################################################################
#####   Package check                                                   ####
#####   Description: Checks if a package exists before install          ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-23                                       ####
############################################################################


pkgtest <- function(pkg) {
  if (!require(pkg, character.only=TRUE))
  {
    machon.install(pkg)
    if (!require(pkg, character.only=TRUE)) stop(paste(pkg, "could not be loaded"))
  }
}


############################################################################
#####   DATA VISUALIZATION                                              ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2014-03-12                                       ####
############################################################################

###########  Functions   ##############################################

exploreData <- function(y=NULL, data=data, factorSize=10, dir="report", ...) {
  #require("outliers")
  ################## Prepare for the report ###################
  #mydir <- getwd()
  #mydir <- "~/"
  #report <- paste(mydir,"/report",sep="")
  report <- dir
  if (!file.exists(report)) {
    dir.create(report)
  }
  fig <- paste(report,"/fig",sep="")
  if (!file.exists(fig)) {
    dir.create(fig)
  }

  # determine which columns are integer
  int_col <- which(sapply(data, is.integer))
  mi <- vector()
  # find only those integers with less than 10 unique values and convert to factor
  #tmp <- data.frame(lapply(ckd[int_col], factor))  ###ckd2
  for (li in int_col) {
    if (length(unique(data[,li])) < factorSize) {
      mi <- c(mi,li)
      if (is.factor(data[,li]) == FALSE) {
        data[,li] <- factor(data[,li])
      }
    }
  }

  str_col <- which(sapply(data, is.character))
  mi <- vector()
  # find only those integers with less than 10 unique values and convert to factor
  #tmp <- data.frame(lapply(ckd[int_col], factor))  ###ckd2
  for (li in str_col) {
    mi <- c(mi,li)
    data[,li] <- factor(data[,li])
  }

  # create the html report page
  myhtml <- paste(report,"/report.html",sep="")
  cat("<head>
      <title>Data Visualization</title>
      <meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />
      <link rel='stylesheet' href='file://clalit/dfs$/Docs/Institute/software/R/jquery.mobile-1.4.5.min.css'>

      <script src='file://clalit/dfs$/Docs/Institute/software/R/jquery-1.10.2.js'></script>
      <script>
      $(function() {
      $('.origimg').click(function(e) {
      $('#popup_img').attr('src',$(this).attr('src'));
      $('#myContainer').hide();
      var pos = $(document).scrollTop();
      $('#myContainer').css({'top':pos+20,'left':250, 'position':'absolute', 'border':'1px solid black', 'padding':'0px'});
      $('#myContainer').show();
      });
      $('#myContainer').click(function(e) {
      $('#myContainer').hide();
      });
      });
      </script>
      <style>
      td {padding':'2px';}
      </style>
      <style>
      td {
      padding: 10px;
      font-family: Georgia, serif;
      }
      </style>
      </head>

      <body>
      <div id='pageone' data-role='main' class='ui-content'>
      ", file = myhtml, sep='\n',append=FALSE)

  html <- paste("<p><p><h1> Data Visualization & Exploration </h1>")
  cat(html, file = myhtml, sep='\n', append=TRUE)

  # begin table
  alt1 <- ifelse(is.null(y)== TRUE, "", "<th> Dependent <br> Variable <br> Distribution </th>")
  html <- paste("<p><p><table border='1'><tbody>
                <tr><th> Variable </th><th> Distribution </th>
                <th>Descriptive <br> Statistics</th><th> Outliers </th>", alt1, "</tr>")
  cat(html, file = myhtml, sep='\n', append=TRUE)

  # Check for the statistics of each variable
  nm <- names(data)
  for (x in nm) {
    # begin a table row
    html <- paste("<tr><td><b>", x ,"</b></td>")
    cat(html, file = myhtml, sep='\n', append=TRUE)
    # determine the type of data
    dt <- ifelse(is.factor(data[[x]])==TRUE | is.character(data[[x]])==TRUE | inherits(data[[x]], "Date")==FALSE, 1, 2)
    # first create a histogram / bars
    if (dt == 2 & inherits(data[[x]],"Date")==FALSE & is.na(data[[x]])==FALSE & is.null(data[[x]])==FALSE) {
      imgname = paste(fig,"/",x, "_1.png",sep="")
      imgsrc = paste("fig/",x, "_1.png",sep="")
      if (is.numeric(data[[x]])==TRUE) {
      ###
        png(imgname)
        d=density(data[[x]], kernel = "gaussian",na.rm=TRUE)
        breakstar=(max(data[[x]],na.rm=TRUE) -min(data[[x]],na.rm=TRUE))/d$bw
        h=hist(data[[x]], breaks=breakstar)
        plot(h,main="",xlab=x)
        yfit<-seq(min(data[[x]],na.rm=TRUE),max(data[[x]],na.rm=TRUE),length=40)
        ffit<-dnorm(yfit,mean=mean(data[[x]],na.rm=T),sd=sd(data[[x]],na.rm=T))
        ffit <- ffit*diff(h$mids[1:2])*length(data[[x]])
        lines(yfit, ffit, col="blue", lwd=2)
        dev.off()
      } else if (dt==1 & is.na(data[[x]])==FALSE & is.null(data[[x]])==FALSE) {
      ###
        png(imgname)
        hist(data[[x]],breaks=10)
        dev.off()
      }
      html <- paste("<td><img class='origimg'  src='",imgsrc,"' height='150' width='150'></img><br></td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
    } else if (dt == 1 & is.na(data[[x]])==FALSE & is.null(data[[x]])==FALSE) {
      imgname = paste(fig,"/",x, "_1.png",sep="")
      imgsrc = paste("fig/",x, "_1.png",sep="")
      png(imgname)
      plot(data[[x]])
      dev.off()
      html <- paste("<td><img class='origimg' src='",imgsrc,"' height='150' width='150'></img><br></td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
    } else {
      html <- paste("<td><center>NO GRAPHIC AVAILABLE</center></td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
    }
    # second, show the statistics
    N <- length(data[[x]])
    n <- length(data[[x]][which(is.na(data[[x]])==FALSE)])
    pct <- formatC(n/N * 100)
    nmiss <- length(data[[x]][which(is.na(data[[x]])==TRUE)])
    npct <- formatC(nmiss/N *100)
    # show descriptive stats
    if (dt == 2 & inherits(data[[x]],"Date")==FALSE) {
      ma <- mean(data[[x]], na.rm=TRUE)
      s <- sd(data[[x]], na.rm=TRUE)
      me <- formatC(median(data[[x]], na.rm=TRUE))
      q1 <- formatC(quantile(data[[x]],1/4, na.rm=TRUE))
      q3 <- formatC(quantile(data[[x]],3/4, na.rm=TRUE))
      mn <- formatC(min(data[[x]], na.rm=TRUE))
      mx <- formatC(max(data[[x]], na.rm=TRUE))
      html <- paste("<td> <u>Data type</u>: Continuous <p> <u>Data length</u>: ",n ,"/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                    nmiss, " (", npct, "%)<p> <u>Mean</u>: ", formatC(ma), "\t <u>StdDev</u>: ", formatC(s), "<br><u>Median</u>: ",me,
                    "\t <u>IQR</u>: ", q1, "-", q3, "<br><u>Min</u>: ", mn, "\t <u>Max</u>: ", mx, "</td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
      #stats <- list(c("Mean: ", ma, "StdDev: ", s), c("Median: ", me, "IQR: ", q1, q3), c("Min",mn,"Max",mx))
    } else if(dt == 1 & inherits(data[[x]],"Date")==FALSE) {
      l <- levels(data[[x]])
      s <- summary(data[[x]])
      htm <- "<ul>"
      if (length(l) < 5) {
        for (lv in l) {
          htm <- paste(htm, "<li><u>", lv, "</u>: ", s[[lv]], "</li>")
        }
        htm <- paste(htm,"</ul>")
      }
      html <- paste("<td> <u>Data type</u>: Categorical Data <p> <u>Data length</u>: ",n, "/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                    nmiss, " (", npct, "%) <p> <u>Number of levels</u>: ", length(l), "<br>", htm, "</td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
      #stats <- c("Categorical data: ", l[1], s[l[1]], l[2], s[l[2]])
    } else {
      #l <- levels(data[[x]])
      s <- summary(data[[x]])
      html <- paste("<td> <u>Data type</u>: Date <p> <u>Data length</u>: ",n, "/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                    nmiss, " (", npct, "%) <p> <u>Min date</u>: ", min(data[[x]], na.rm=T), "<br><u>Max date</u>:",min(data[[x]], na.rm=T) , "</td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)

    }

    # third, determine the outliers
    if (dt==2 & inherits(data[[x]],"Date")==FALSE) {
      #xtrm <- outlier(data[[x]])
      bp <- boxplot(data[[x]],plot=FALSE)
      if (length(unique(bp$out)) > 10) {
        xtrm <- paste("There are ", length(unique(bp$out)), " outlier values")
      } else if (length(unique(bp$out)) == 0) {
        xtrm <- "No outlier values found"
      } else {
        xtrm <- paste(formatC(order(unique(bp$out))), collapse=', ' )
      }
      imgname = paste(fig,"/",x, "_2.png",sep="")
      imgsrc = paste("fig/",x, "_2.png",sep="")
      png(imgname)
      mod <- try(
        if (is.null(y)==FALSE) {
          cl <- as.numeric(data[[y]])
          scatter.smooth(data[[x]],col=cl)
        } else {
          scatter.smooth(data[[x]])
        }
      )
      if(inherits(mod, "try-error")) {
        next
      } else {
        abline(h=ma-(3*s), col="red", lty=2)
        abline(h=ma+(3*s), col="red", lty=2)
      }
      dev.off()
      html <- paste("<td><img class='origimg' src='",imgsrc,"' height='150' width='250'></img><br> <u>Outlier values</u>: <br> ", xtrm, "</td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
    } else {
      html <- paste("<td></td>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
    }

    # fourth, if y is assigned, make a corresponding plot
    if (is.null(y)==FALSE) {
      if (dt==2 & is.numeric(data[[y]])==TRUE) {
        imgname = paste(fig,"/",x, "_3.png",sep="")
        imgsrc = paste("fig/",x, "_3.png",sep="")
        png(imgname)
        scatter.smooth(data[[x]] ~ data[[y]])
        dev.off()
        html <- paste("<td><img class='origimg' src='",imgsrc,"' height='150' width='150'></img><br></td>")
        cat(html, file = myhtml, sep='\n', append=TRUE)
      } else if (dt==1 & is.na(data[[x]])==FALSE & is.null(data[[x]])==FALSE) {
        imgname = paste(fig,"/",x, "_3.png",sep="")
        imgsrc = paste("fig/",x, "_3.png",sep="")
        png(imgname)
        plot(data[[x]] ~ data[[y]])
        #boxplot(data[[x]] ~ data[[y]])
        dev.off()
        html <- paste("<td><img class='origimg' src='",imgsrc,"' height='150' width='150'></img><br></td>")
        cat(html, file = myhtml, sep='\n', append=TRUE)
      } else {
        html <- paste("<td>NO GRAPHIC AVAILABLE</td>")
        cat(html, file = myhtml, sep='\n', append=TRUE)
      }
    }
    # now print all
    #res <- list(c("Complete values: ", n, pct), c("Missing values: ", nmiss, npct), c("Outliers: ", xtrm), stats)
    #print(res)
  }
  #### finish the report
  # end table
  html <- paste("
                <div data-role='popup' id='myContainer' style='display: none;'>
                <img id='popup_img' src='' />
                </div>
                </div>
                </div>
                </body></html>
                ")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  if(.Platform$OS.type == "unix") {
    system(paste("start /b ", myhtml))
  } else {
    shell(paste("explorer ", gsub("/", "\\\\", myhtml) ), intern=TRUE)
  }
}

###################### END exploreData ###############

############################################################################
#####   TEST & TRAIN DATASET GENERATION                                                       ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-17                                       ####
############################################################################


train_test <- function(data=NULL,train=NULL,test=NULL,prop=NULL,seed=123)
{
  ## set the seed to make your partition reproductible
  set.seed(seed)
  smp_size <- floor(prop * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  assign(train, data[train_ind, ], envir=globalenv())
  assign(test, data[-train_ind, ], envir=globalenv())
  return("created a train and test datasets")
}

######################### END train_test ###############


############################################################################
#####   TABLE 1                                                         ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-03-09                                       ####
#####   Last Modified: 2016-07-01                                       ####
############################################################################


####################  FUNCTIONS  ###########################################
#### Usage:
####   x: character vector with the name of the variables
####   y: the name of the strata variable (optional)
####   rn: character vector with the text we want to replace the variable names
####   data: the dataset to be used
####   miss: include missing statistics: [0=none, 1=only for categorical variables, 2=for all variables]
####   excel: export the table to excel [0=no, 1=yes]
####   excel_file: the name of the excel file we want to save the table (optional)
####
###################

Table1 <- function (x=NULL, y=NULL, rn=NULL, data=NULL, miss=1, excel=0, excel_file=NULL) {
  ### define sub-functions
  g1 <- function(var)c(Mean=mean(var,na.rm=TRUE), SD=sd(var,na.rm=TRUE))
  g2 <- function(var)c(Median=median(var,na.rm=TRUE), IQR=quantile(var,c(0.25,0.75),na.rm=TRUE))
  getSimpleTable <- function(x=x, rn=rn, data=data, miss=miss) {
    if (is.null(rn)==TRUE) { rn <- x}
    tableaaaa <- cbind(V1="Variables",V2="Categories",n="n","Population")
    q <- 1
    n <- 1
    for (v in x)
    {
      print(v)
      if (is.numeric(data[[v]])==T) {
        ## report mean and standard deviation
        t_n <- g1(data[[v]])
        tp <- paste(format(round(t_n[1],1),nsmall=1,big.mark=",")," (", format(round(t_n[2],1),nsmall=1,big.mark=","),")",sep="")
        tbl1 <- cbind(rn[q],"Mean (SD)",n=1, tp)
        tableaaaa <- rbind(tableaaaa,tbl1)
        ## report median and Interquartile ranges (25%,75%)
        t_n <- g2(data[[v]])
        tp <- paste(format(round(t_n[1],1),nsmall=1,big.mark=",")," (", format(round(t_n[2],1),nsmall=1,big.mark=","),"-", format(round(t_n[3],1),nsmall=1,big.mark=","), ")",sep="")
        tbl1 <- cbind(rn[q],"Median (IQR)",n=2, format(tp,big.mark=","))
        tableaaaa <- rbind(tableaaaa,tbl1)
        ## report number and percent of missing
        if (miss >= 1) {
          datams <- subset(data,is.na(data[[v]])==T)
          if (nrow(datams)>0) {
            data$cnt <- 1
            datams$cnt <- 1
            t_n <- table(data$cnt)
            t_m <- sum(datams$cnt)
            tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,1),nsmall=1,big.mark=","),"%)",sep="")
            tbl1 <- cbind(rn[q],"Missing (%)",n=3, tp)
          } else {
            tbl1 <- cbind(rn[q],"Missing (%)",n=3, " -- ")
          }
          tableaaaa <- rbind(tableaaaa,tbl1)
        }
      } else {
        t_n <- table(data[[v]])
        ttotal <- sum(t_n)
        nm <- row.names(t_n)
        for (f in 1:length(nm)) {
          tp <- t_n[f] / ttotal * 100
          pct <- paste(format(round(t_n[f],1),nsmall=0,big.mark=",")," (", format(round(tp,1),nsmall=1,big.mark=","), "%)",sep="")
          tbl1 <- cbind(rn[q],nm[f],n=f, pct)
          tableaaaa <- rbind(tableaaaa,tbl1)
        }
        if (miss >= 2) {
          datams <- subset(data,is.na(data[[v]])==T)
          if (nrow(datams)>0) {
            data$cnt <- 1
            datams$cnt <- 1
            t_n <- table(data$cnt)
            t_m <- sum(datams$cnt)
            tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,1),nsmall=1,big.mark=","),"%)",sep="")
            tbl1 <- cbind(rn[q],"Missing (%)",n=f+1, tp)
          } else {
            tbl1 <- cbind(rn[q],"Missing (%)",n=f+1, " -- ")
          }
          tableaaaa <- rbind(tableaaaa,tbl1)
        }
      }
      q <- q + 1
    }
    #row.names(tableaaaa) <- rn
    table1 <- data.frame(tableaaaa)
    return(tableaaaa)

  }
  pvals <- function(x=x,y=y,rn=rn,data=data) {
    if (is.null(rn)==TRUE | length(rn)<2) {rn <- x}
    require(car)
    q <- 1
    ptab <- cbind(V="Variables",pval="pval", n="n")
    for (v in x) {
      print(v)
      if (is.numeric(data[[v]])==TRUE & length(unique(data[[v]])) > 1) {
        ### first check for homoskedasticity
        if (bartlett.test(data[[v]],data[[y]])[3] > 0.05) {
          ### homoskedasticity
          pval <- round(as.numeric(Anova(lm(data[[v]]~data[[y]]))[1,4]),3)
        } else {
          pval <- round(as.numeric(Anova(lm(data[[v]]~data[[y]]),white.adjust=TRUE)[1,3]),3)
        }
      } else if (length(unique(data[[v]]))==1) {
        pval <- NA
      } else {
        pval <- round(as.numeric(kruskal.test(data[[v]]~data[[y]])[3]),3)
      }
      ptab <- rbind(ptab,cbind(rn[q],pval,2))
      q <- q + 1
    }
    return(ptab)
  }
  ####################### Begin analysis
  ##### if y is null then make a simple table
  require("sqldf")
  tabaaa1 <- getSimpleTable(x=x, rn=rn, data=data, miss=miss)
  ##### if y has two levels, then make a compound comparison
  if (exists("y")==TRUE & is.null(y)==FALSE) {
    if (length(levels(data[[y]])) >= 2) {
      for (lv in levels(data[[y]])) {
        dtsub <- subset(data, data[[y]]==lv)
        tab <- getSimpleTable(x=x, rn=rn, data=dtsub, miss=miss)
        tab[1,4] <- lv
        tabaaa1 <- cbind(tabaaa1,tab[,4])
      }
      tabaaa1 <- data.frame(tabaaa1)
      ptab <- data.frame(pvals(x=x,y=y,rn=rn,data=data))
      ### calculate the p-value
      tabaaa1 <- sqldf("SELECT a.*,
                       CASE
                       WHEN (b.pval IS NULL) THEN ('')
                       WHEN (b.pval < 0.001) THEN ('<0.001')
                       ELSE (b.pval) END AS pval
                       FROM tabaaa1 a
                       LEFT OUTER JOIN ptab b ON a.V1 = b.V AND a.n=b.n
                       WHERE a.V2 NOT IN ('No','0','None') AND V4 <> ' -- '")
      tabaaa1$n <- NULL
      #return(tabaaa1)
    }
  }
  ##### Join the tables...
  if (excel==1) {
    pkgtest("xlsx")
    wb <- createWorkbook()
    sheet1 <- createSheet(wb, sheetName="Table 1")
    addDataFrame(tabaaa1,sheet1)
    #### save and close the workbook
    saveWorkbook(wb, excel_file)
    return(tabaaa1)
  } else {
    return(tabaaa1)
  }
}

########################## END Table1 ###############


############################################################################
#####   CREATE A SLQ REPRESENTATION FROM A SOME MODELS                  ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   EXTRACTED FROM THE INTERNET - IT IS ALPHA-NEEDS QA              ####
#####   https://github.com/jasoncapehart/genSQL/blob/master/genSQL.R    ####
#####   Creation date: 2016-08-17                                       ####
############################################################################

genSQL <- function(model, digits = .Machine$sizeof.longdouble, outcome.name = NULL, file=NULL) {
  model.class <- class(model)[1]
  # Error handling
  sql.statement <- "No model has been returned..."
  if (("randomForest" %in% class(model))) { model.class <- "randomForest" }

  if (any(model.class == c("glm", "rpart", "lm", "cpart","coxph","randomForest")) == FALSE) {
    warning(model.class, " is not a supported model type")
    stop()
  }

  # Model function selection
  if (model.class == "lm") {
    sql.statement <- lm2sql(lm.model = model, digits)
  } else if (model.class == "glm") {
    sql.statement <- glm2sql(glm.model = model, digits)
  } else if (model.class == "coxph") {
    sql.statement <- coxph2sql(cox.model = model, digits)
  } else if (model.class == "rpart") {
    sql.statement <- tree2sql(dtree=model, outcome.name)
  } else if (model.class == "cpart") {
    sql.statement <-ctree2sql(dtree=model, outcome.name)
  } else if (model.class == "randonForest") {
    if (is.null(file)==TRUE) {
      sql.statement <- ("Please add the name of a file to write the SQL script \n
           randomForest generate too many tree leafs and the size \n
           of the resulting string may exced the maximal allowed \n
           sting length (2^31-1 bytes)")
    } else {
      rf2sql(model, file)
      sql.statement <- (paste("The SQL file was generated as: ",file,
                   "\n Your current directory is: ",getwd()))
    }

  }
  return(sql.statement)
}

############################################################################
#####   CREATE A SLQ REPRESENTATION FROM A LM MODEL                     ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   EXTRACTED FROM THE INTERNET - IT IS ALPHA-NEEDS QA              ####
#####   https://github.com/jasoncapehart/genSQL/blob/master/genSQL.R     ####
#####   Creation date: 2016-08-17                                       ####
############################################################################
#----------
# interaction.check
#   Checks to see if any of the specified model terms are interactions
# Input: sql.statement -
# Output: a sql statement with the interactions correctly specified
#------------
interaction.correction <- function(sql.statement) {
  corrected.statement <- gsub(pattern=":", replacement="*", x=sql.statement)
  return(corrected.statement)
}

lm2sql <- function(lm.model, digits) {
  # Get the coefficients
  coeff <- summary(lm.model)$coefficients[, "Estimate"]
  coeff <- round(coeff, digits)
  # Store the variable names
  var.names <- rownames(summary(lm.model)$coefficients)
  # Concatenate the coefficients and variable names
  sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
  # Concatentate the SQL syntax and add the intercept
  sql.statement <- paste("SELECT", sql.var, coeff[1])
  # Apply any interaction correction
  sql.statement <- interaction.correction(sql.statement)

  return(sql.statement)
}

############################################################################
#####   CREATE A SLQ REPRESENTATION FROM A GLM MODEL                    ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   EXTRACTED FROM THE INTERNET - IT IS ALPHA-NEEDS QA              ####
#####   https://github.com/jasoncapehart/genSQL/blob/master/genSQL.R     ####
#####   Creation date: 2016-08-17                                       ####
############################################################################

glm2sql <- function(glm.model, digits) {
  # Error checking
  if (glm.model$family$family != "binomial") {
    warning("Only binomial logistic regression supported")
    stop()
  }
  # Get the coefficients
  coeff <- summary(glm.model)$coefficients[, "Estimate"]
  coeff <- round(coeff, digits)
  # Store the variable names
  var.names <- rownames(summary(glm.model)$coefficients)
  # Concatenate the coefficients and variable names
  sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
  # Concatenate the SQL syntax and add the intercept
  sql.statement <- paste("SELECT 1 / (1 + exp(", sql.var, coeff[1], "))")
  # Apply any interaction correction
  sql.statement <- interaction.correction(sql.statement)

  return(sql.statement)
}

############################################################################
#####   CREATE A SLQ REPRESENTATION FROM A COX MODEL (coxph R package)  ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   EXTRACTED FROM THE INTERNET - IT IS ALPHA-NEEDS QA              ####
#####   https://github.com/jasoncapehart/genSQL/blob/master/genSQL.R     ####
#####   Creation date: 2016-08-17                                       ####
############################################################################

# coxph2sql <- function(cox.model, digits) {
#   # Get the coefficients
#   coeff <- summary(cox.model)$coefficients[, "coef"]
#   coeff <- round(coeff, digits)
#   # Store the variable names
#   var.names <- rownames(summary(cox.model)$coefficients)
#   # Concatenate the coefficients and variable names
#   sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
#   # Concatenate the SQL syntax and add the intercept
#   sql.statement <- paste("SELECT 1 / (1 + exp(", sql.var, coeff[1], "))")
#   # Apply any interaction correction
#   sql.statement <- interaction.correction(sql.statement)
#
#   return(sql.statement)
# }

coxph2sql <- function(cox.model, digits) {
  # Get the coefficients
  coeff <- cox.model$coefficients
  coeff <- round(coeff, digits)
  # Store the variable names
  var.names <- names(cox.model$coefficients)
  # Concatenate the coefficients and variable names
  sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
  # Concatenate the SQL syntax and add the intercept
  sql.statement <- paste("SELECT 1 / (1 + exp(", sql.var, coeff[1], "))")
  # Apply any interaction correction
  sql.statement <- interaction.correction(sql.statement)
  sql.statement <- paste(sql.statement, " AS prediction")
  return(sql.statement)
}

#############################################################################
#####   CREATE A SLQ QUERY FROM A RPART TREE MODEL (rpart R package)     ####
#####   Author: Tomas Karpati M.D.                                       ####
#####   EXTRACTED FROM THE INTERNET - IT IS ALPHA-NEEDS QA               ####
#####   https://github.com/jasoncapehart/genSQL/blob/master/genSQL.R     ####
#####   Creation date: 2016-08-23                                        ####
#############################################################################


#------------------
# Tree to SQL Translation Function
# Input: dtree - a decision tree from rpart()
#        outcome.name - the name for the predicted value
# Output: An ANSI SQL description of the model
# Required: tree.stack(), node.pred(), tree.parse()
#----------------

tree2sql <- function(dtree, outcome.name) {
  #---------------------------------
  # Tree Parse Function
  # Input: dtree - a decision tree from rpart()
  # Output: the parsed rule set
  #-----------------------------------

  tree.parse <- function(dtree) {
    # Grab the rule stack
    rule.stack <- capture.output(dtree)
    # Remove the data to the right of the rule
    match.right <- regexpr(pattern="[[:digit:]]+[[:space:]]{2}[[:digit:]].*", text=rule.stack)
    sub.result <- unlist(regmatches(x = rule.stack, m=match.right, invert = TRUE))
    # Remove the rule number and spaces to the left of the rule
    match.left <- regexpr(pattern="[[:space:]]*[[:digit:]]+)[[:space:]]", text = sub.result)
    final.result <- unlist(regmatches(x = sub.result, m=match.left, invert=TRUE))
    final.result <- final.result[final.result != ""][-1:-4]

    return(final.result)
  }

  #--------------
  # Node Prediction Function
  # Input: dtree - a decision tree from rpart()
  # Output: a vector of the probabilities/predictions for each node
  #-------------

  node.pred <- function(dtree) {
    pred.df <- NA

    if (dtree$method == "anova") { pred <- dtree$frame$yval
    } else if (dtree$method == "class") { pred <- dtree$frame$yval2[, 4] }

    leaf <- ifelse(dtree$frame$var == "<leaf>", yes=1, no = 0)

    pred.df <- data.frame("node" = rownames(dtree$frame), "pred" = pred, "leaf" = leaf)

    return(pred.df)
  }

  #--------------------
  # Tree Stack Function
  # Input: dtree - a decision tree from rpart()
  # Output: a rule stack data frame
  # Required: node.pred(), tree.parse()
  #------------------

  tree.stack <- function(dtree) {
    df <- node.pred(dtree)
    node.rule <- c("root", tree.parse(dtree))
    df <- data.frame(df, "rule" = node.rule)

    return(df)
  }

  # Initialize
  stack <- tree.stack(dtree)
  sql.statement <- NULL
  curr.state <- 0
  branch.depth <- 0

  # Loop through each rule in the stack
  for (i in 2:dim(stack)[1]) {
    # Setup
    #-----------
    # Current and Next State
    if (i == 2) { curr.state <- 0
    } else { curr.state <- stack[i-1, "leaf"]}
    next.state <- stack[i, "leaf"]
    # Current Rule
    rule.i <- stack[i, "rule"]
    # Tab number
    tabs <- paste(rep(" ", times=branch.depth), collapse='')

    # Logic: Translates stack -> tree
    #-------------
    # Node -> Node
    if (curr.state == 0 & next.state == 0) {
      # Write statement
      statement.i <- paste(" ", tabs, "CASE WHEN", rule.i, "THEN")
      sql.statement <- paste(sql.statement, statement.i)
      # Increment branch depth
      branch.depth <- branch.depth + 1
      next()
    }
    # Node -> Leaf Transition
    if (curr.state == 0 & next.state == 1) {
      # Write statement
      pred.i <- stack[i, "pred"]
      statement.i <- paste(" ", tabs, "CASE WHEN", rule.i, "THEN", pred.i)
      sql.statement <- paste(sql.statement, statement.i)
      # Increment branch depth
      branch.depth <- branch.depth + 1
      next()
    }
    # Leaf -> Node Transition
    if (curr.state == 1 & next.state == 0) {
      # Write statement
      statement.i <- paste(" ", tabs, "WHEN", rule.i, "THEN")
      sql.statement <- paste(sql.statement, statement.i)
      next()
    }
    # Leaf -> Leaf Transition
    if (curr.state == 1 & next.state == 1) {
      # Write statement
      pred.i <- stack[i, "pred"]
      statement.i <- paste(" ", tabs, "WHEN", rule.i, "THEN", pred.i)
      sql.statement <- paste(sql.statement, statement.i)
      # Close 1 open CASE statement
      sql.statement <- paste(sql.statement, " ", tabs, "END")
      # Decrement the branch.depth
      branch.depth <- branch.depth - 1
      next()
    }
  }
  # END any hanging CASE clauses
  termination <- paste(rep(" END", times = branch.depth), collapse='')
  # Throw on the outcomes name
  sql.statement <- paste("SELECT", sql.statement, termination, "AS", outcome.name)
  return(sql.statement)
}


#############################################################################
#####   CREATE A SLQ REPRESENTATION FROM A CTREE MODEL (party R package) ####
#####   Author: Tomas Karpati M.D.                                       ####
#####   EXTRACTED FROM THE INTERNET - IT IS ALPHA-NEEDS QA               ####
#####   Creation date: 2016-08-23                                        ####
#############################################################################


ctree2sql <- function(mod, out) {
  #########################
  ##### Modified from the code of :Andrew Ziem
  # Copyright (C) 2011 Andrew Ziem
  # Licensed under the GNU General Public License version 2 or later <https://www.gnu.org/licenses/gpl-2.0.html>

  # get node ID for left child
  btree_left <- function(mytree, parent_id)
  {
    nodes(mytree, parent_id)[[1]]$left$nodeID
  }

  # get right child
  btree_right <- function(mytree, parent_id)
  {
    nodes(mytree, parent_id)[[1]]$right$nodeID
  }

  # get prediction for this node
  btree_prediction <- function(mytree, node_id)
  {
    p <- nodes(mytree, node_id)[[1]]$prediction
    if (2 == length(p)) {
      return(p[2])
    }
    return (p)

  }

  # criteria for this node as a string
  btree_criteria <- function(mytree, node_id, left)
  {
    if (nodes(mytree, node_id)[[1]]$terminal)
    {
      return("(error: terminal node)");
    }
    if (nodes(mytree, node_id)[[1]]$psplit$ordered)
    {
      sp <- nodes(mytree, node_id)[[1]]$psplit$splitpoint
      vn <- nodes(mytree, node_id)[[1]]$psplit$variableName
      if (left) {
        op <- '<='
      } else {
        op <- '>'
      }
      return(paste(vn, op, sp))
    } else {
      psplit <- nodes(mytree, node_id)[[1]]$psplit
      if (left){
        l <- as.logical(psplit$splitpoint)
      } else {
        l <- as.logical(!psplit$splitpoint)
      }

      r <- paste(attr(psplit$splitpoint, 'levels')[l], sep='', collapse="','")
      return(paste(psplit$variableName, " in ('", r,"')", sep=''))
    }
  }

  walk_node <- function(mytree, node_id = 1, parent_criteria = character(0))
  {
    if (nodes(mytree, node_id)[[1]]$terminal) {
      prediction <- btree_prediction(mytree, node_id)
      sprediction <- paste('WHEN ', parent_criteria, 'THEN ',prediction )
      return (sprediction)
    }

    left_node_id <- btree_left(mytree, node_id)
    right_node_id <- btree_right(mytree, node_id)

    if (is.null(left_node_id) != is.null(right_node_id)) {
      print('left node ID != right node id')
    }
    sprediction <- character(0)
    if (!is.null(left_node_id)) {
      new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, T), sep=' END ')
      if (1 == node_id)
        new_criteria <- btree_criteria(mytree, node_id, T)
      sprediction <- walk_node(mytree, left_node_id, new_criteria)
    }
    if (!is.null(right_node_id)) {
      new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, F), sep=' END ')
      if (1 == node_id)
        new_criteria <- btree_criteria(mytree, node_id, F)
      sprediction <- paste(sprediction, walk_node(mytree, right_node_id, new_criteria), sep=' ')
    }
    return(sprediction)
  }
  sql <- paste("CASE", walk_node(mod), "END AS ",out)
  return(sql)
}



#############################################################################
#####   CREATE A SLQ REPRESENTATION FROM A RANDOM FOREST MODEL           ####
#####   Author: Tomas Karpati M.D.                                       ####
#####   EXTRACTED FROM THE INTERNET - IT IS ALPHA-NEEDS QA               ####
#####   Creation date: 2016-08-23                                        ####
#############################################################################

#################### RF

rf2sql <- function (model, file) {
  require (randomForest, quietly=TRUE)

  if (is.null(file)==TRUE) {
    return("Please add the name of a file to write the SQL script \n
           randomForest generate too many tree leafs and the size \n
           of the resulting string may exced the maximal allowed \n
           sting length (2^31-1 bytes)")
  }

  if (!("randomForest" %in% class(model))) {
    stop ("Expected a randomForest object")
    return
  }

  sink(file, type="output")

  for (tree.num in 1:(model$ntree)) {
    recurse.rf <- function(model, tree.data, tree.row.num, ind=0) {
      tree.row <- tree.data[tree.row.num,]
      indent.str <- paste(rep("\t", ind), collapse="")
      split.var <- as.character(tree.row[,"split var"])
      split.point <- tree.row[,"split point"]
      if(tree.row[,"status"] != -1) {  # splitting node
        if(is.numeric(unlist(model$forest$xlevels[split.var]))) {
          cat(paste("\n",indent.str,"CASE WHEN", gsub("[.]","_",split.var), "IS NULL THEN NULL",
                    "\n",indent.str,"WHEN", gsub("[.]","_",split.var), "<=", split.point, "THEN "))
          recurse.rf(model, tree.data, tree.row[,"left daughter"], ind=(ind+1))
          cat("\n",indent.str,"ELSE ")
          recurse.rf(model, tree.data, tree.row[,"right daughter"], ind=(ind+1))
          cat("END ")
        } else {  # categorical
          # function to convert from binary coding to the category values it represents
          conv.to.binary <- function (ncat, num.to.convert) {
            ret <- numeric()
            if((2^ncat) <= num.to.convert) {
              return (NULL)
            } else {
              for (x in (ncat - 1):0) {
                if (num.to.convert >= (2^x)) {
                  num.to.convert <- num.to.convert - (2^x)
                  ret <- c(ret, 1)
                } else {
                  ret <- c(ret, 0)
                }
              }
              return(ret)
            }
          }
          categ.bin <- conv.to.binary(model$forest$ncat[split.var], split.point)
          categ.flags <- (categ.bin[length(categ.bin):1] == 1)

          categ.values <- unlist(model$forest$xlevels[split.var])
          cat(paste("\n",indent.str,"CASE WHEN ", gsub("[.]","_",split.var), " IN ('",
                    paste(categ.values[categ.flags], sep="", collapse="', '"),  #FIXME replace quotes dependant on var type
                    "') THEN ", sep=""))
          recurse.rf(model, tree.data, tree.row[,"left daughter"], ind=(ind+1))
          cat(paste("\n",indent.str,"WHEN ", gsub("[.]","_",split.var), " IN ('",
                    paste(categ.values[!categ.flags], sep="", collapse="', '"),
                    "') THEN ", sep=""))
          recurse.rf(model, tree.data, tree.row[,"right daughter"], ind=(ind+1))
          cat(paste("\n", indent.str,"ELSE NULL END ", sep="")) #FIXME: null or a new category
        }
      } else { # terminal node
        if (is.numeric(tree.data$prediction)) {
          cat(paste(tree.row[,"prediction"], " ", sep=""))
        } else {
          cat(paste("'", tree.row[,"prediction"], "' ", sep=""))
        }
      }
    }
    recurse.rf(model, getTree(model,k=tree.num,labelVar=TRUE), 1)
    #cat(paste("as tree",tree.num,"\nFROM ",input.table,";\n\n", sep=""))
    cat(paste("as tree rfPred;\n\n", sep=""))
  }
  # close the file
  sink()

  #if (model$type == "classification")
  }

##################
recurse.rf <- function(model, tree.data, tree.row.num, ind=0) {
  tree.row <- tree.data[tree.row.num,]
  indent.str <- paste(rep("\t", ind), collapse="")
  split.var <- as.character(tree.row[,"split var"])
  split.point <- tree.row[,"split point"]
  if(tree.row[,"status"] != -1) {  # splitting node
    if(is.numeric(unlist(model$forest$xlevels[split.var]))) {
      cat(paste("\n",indent.str,"CASE WHEN", gsub("[.]","_",split.var), "IS NULL THEN NULL",
                "\n",indent.str,"WHEN", gsub("[.]","_",split.var), "<=", split.point, "THEN "))
      recurse.rf(model, tree.data, tree.row[,"left daughter"], ind=(ind+1))
      cat("\n",indent.str,"ELSE ")
      recurse.rf(model, tree.data, tree.row[,"right daughter"], ind=(ind+1))
      cat("END ")
    } else {  # categorical
      # function to convert from binary coding to the category values it represents
      conv.to.binary <- function (ncat, num.to.convert) {
        ret <- numeric()
        if((2^ncat) <= num.to.convert) {
          return (NULL)
        } else {
          for (x in (ncat - 1):0) {
            if (num.to.convert >= (2^x)) {
              num.to.convert <- num.to.convert - (2^x)
              ret <- c(ret, 1)
            } else {
              ret <- c(ret, 0)
            }
          }
          return(ret)
        }
      }
      categ.bin <- conv.to.binary(model$forest$ncat[split.var], split.point)
      categ.flags <- (categ.bin[length(categ.bin):1] == 1)

      categ.values <- unlist(model$forest$xlevels[split.var])
      cat(paste("\n",indent.str,"CASE WHEN ", gsub("[.]","_",split.var), " IN ('",
                paste(categ.values[categ.flags], sep="", collapse="', '"),  #FIXME replace quotes dependant on var type
                "') THEN ", sep=""))
      recurse.rf(model, tree.data, tree.row[,"left daughter"], ind=(ind+1))
      cat(paste("\n",indent.str,"WHEN ", gsub("[.]","_",split.var), " IN ('",
                paste(categ.values[!categ.flags], sep="", collapse="', '"),
                "') THEN ", sep=""))
      recurse.rf(model, tree.data, tree.row[,"right daughter"], ind=(ind+1))
      cat(paste("\n", indent.str,"ELSE NULL END ", sep="")) #FIXME: null or a new category
    }
  } else { # terminal node
    if (is.numeric(tree.data$prediction)) {
      cat(paste(tree.row[,"prediction"], " ", sep=""))
    } else {
      cat(paste("'", tree.row[,"prediction"], "' ", sep=""))
    }
  }
}
############################################################################
#####   TABLE 2                                                         ####
#####   Description: calculates the Odds/Hazard ratios and their        ####
#####     confidence intervals from a given model
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-03-09                                       ####
#####   Last Modified: 2016-07-01                                       ####
############################################################################

Table2 <- function (mod, rv=NULL)
{
  if (mod$method=="glm.fit" )  {
    if (mod$family[1]=="binomial" | mod$family[1]=="poisson") {
      pv <- "Pr(>|z|)"
    } else {
      pv <- "Pr(>|t|)"
    }
  } else if (mod$method=="efron") {
    pv <- "Pr(>|z|)"
  } else {pv <- "Pr(>|t|)"}
  exp_coef <- round(exp(coef(mod)),2)
  pv
  dd <- exp(confint(mod, level=0.95))
  dd1 <- round(dd[,1],2)
  dd2 <- round(dd[,2],2)
  p_value <- round(summary(mod)$coef[,pv],2)
  tb <- data.frame(cbind(exp_coef,'2.5'=dd1,'97.5'=dd2,p_value))
  if (is.null(rv)==FALSE) {
    row.names(tb) <- rv
  }
  return(tb)
}

############################################################################
#####   CALCULATE CONFIDENCE INTERVALS FOR MEANS                        ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-24                                       ####
############################################################################

MeanCI <- function(x,round=3) {
  m <- mean(x,na.rm=TRUE)
  s <- sd(x,na.rm=TRUE)
  ci <- 1.96 * sqrt(s/length(x))
  CImin <- m - ci
  CImax <- m + ci
  return(c(mean=round(m,round),CImin=round(CImin,round),CImax=round(CImax,round)))
}

############################################################################
#####   CALCULATE CONFIDENCE INTERVALS FOR PROPORTIONS                  ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-24                                       ####
############################################################################

PropCI <- function(x,round=3,multi=100,ref=2) {
  recode <- function(x,ref) {
    y <- x
    if (ref==2) {
      y[which(x==min(x))] <- 0
      y[which(x==max(x))] <- 1
    } else {
      y[which(x==max(x))] <- 0
      y[which(x==min(x))] <- 1
    }
    return(y)
  }
  #if (ref==0) {ref <- 2} else {ref <- 1}
  if (is.factor(x)==TRUE && length(levels(x))==2) {
    p <- levels(x)[ref]
    y <- recode(as.numeric(x),ref)
  } else if (is.numeric(x)==TRUE && length(levels(factor(x)))==2) {
    p <- ifelse(ref==2,max(x),min(x))
    y <- recode(x,ref)
  } else if (length(levels(factor(x)))==2) {
    p <- levels(factor(x))[ref]
    y <- recode(as.numeric(factor(x)),ref)
  } else {
    return("The variable must be dichotomic")
  }
  freq <- (mean(y,na.rm=TRUE))
  CI <- 1.96 * sqrt((freq * (1-freq))/length(y))
  CImin <- (freq - CI)*multi
  CImax <- (freq + CI)*multi
  return(c(var=p,freq=round(freq*multi,round),CImin=round(CImin,round),CImax=round(CImax,round)))
}

############################################################################
#####   GENERATE A TABLE WITH VALIDITY TESTS                            ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-08-17                                       ####
############################################################################

################# Validity Test #########################################
#
#                    Observed
#  	                 +  	      -
#          -----------------------------------------
# Predicted   +    TP	      FP        |     PPV
#		               a   	    b         |  e (e1-e2)
#	                		                |
#                 		                |
#	            -    FN       TN        |     NPV
#	            	   c        d         |  f (f1-f2)
#	         ----------------------------------------
#
#	           Sensitivity Specificity  |  Prevalence
#	             g (g1-g2)  h (h1-h2)   |  i (i1-i2)
#
#
#       Chi-square
#       Corrected Chi-square
#       Error: (FP+FN)/(TP+FP+FN+TN)
#       Accuracy: (TP+TN)/(TP+FP+FN+TN)
#       Precision: TP/(TP+FP)
#       Recall: TP/(TP+FN)
#
#       Harmonic mean of precision and recall (F1-Score):
#        f1-Score: 2 * (Precision * Recall)/(Precision + Recall)
#
#######################################################################

ValidityTest <- function(a,b,c,d,multi=100,caption="Validity of the Model/Screening") {
  proportionCI <- function(p,n,ci=.95,dec=2,multi=100) {
    se = sqrt((p*(1-p))/n)
    ci = 1.96*se
    pci = paste(round(p*multi,dec),
                " (",
                round((p-ci)*multi,dec),
                "-",
                round((p+ci)*multi,dec),
                ")",
                sep=""
    )
    return(pci)
  }
  ### add chi-square!!!
  ppv <- proportionCI(a/(a+b), a+b,multi)
  npv <- proportionCI(d/(c+d), c+d,multi)
  sensit <- proportionCI(a/(a+c), c+d,multi)
  specif <- proportionCI(d/(b+d), b+d,multi)
  prev <- proportionCI((a+c)/(a+b+c+d), (a+b+c+d),multi)
  er <- proportionCI((b+c)/(a+b+c+d),(a+b+c+d),multi)
  acc <- proportionCI((a+d)/(a+b+c+d),(a+b+c+d),multi)
  prec <- proportionCI(a/(a+b),(a+b),multi)
  recall <- proportionCI(a/(a+c),(a+c),multi)
  f1 <- proportionCI(2 * ((a/(a+b)) * (a/(a+c)))/((a/(a+b)) + (a/(a+c))),((a/(a+b)) + (a/(a+c))),multi)
  x <- matrix(c(a,b,c,d), byrow=TRUE,2,2)
  xsq <- as.numeric(round(chisq.test(x)$statistic,3))
  tryCatch({
    warning(pval <- round(chisq.test(x)$p.value,3))
  }, warning=function(w) {
    message("Using simulated p-value! - ", conditionMessage(w))
    pval <- paste(round(chisq.test(x,simulate.p.value=TRUE)$p.value,3), "* [Yates correction]")
  })

  #x2 <- chisq.test(c(a,c),c(b,d))

  vars <- cbind("","Observed","","")
  vars <- rbind(vars,cbind("","+","-",""))
  vars <- rbind(vars,cbind("Expected","(TP)","(FP)","PPV"))
  vars <- rbind(vars,cbind("+",a,b,ppv))
  vars <- rbind(vars,cbind("","(FN)","(TN)","NPV"))
  vars <- rbind(vars,cbind("-",c,d,npv))
  vars <- rbind(vars,cbind("","Sensitivity","Specificity","Prevalence"))
  vars <- rbind(vars,cbind("",sensit,specif,prev))
  vars <- rbind(vars,cbind("","","",""))
  vars <- rbind(vars,cbind("Chi-square (p-value)",paste(xsq," (",pval,")",sep=""),"",""))
  vars <- rbind(vars,cbind("Error",er,"",""))
  vars <- rbind(vars,cbind("Accuracy",acc,"(Same as PPV)",""))
  vars <- rbind(vars,cbind("Precision",prec,"(Same as Sensitivity)",""))
  vars <- rbind(vars,cbind("Recall",recall,"",""))
  vars <- rbind(vars,cbind("F1-Score",f1,"(Harmonic mean of","precision and recall)"))
  return(vars)
}

############################################################################
#####   Model Validity                                                  ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-12-01                                       ####
############################################################################
modelValidity <- function(data,model,class) {
  require("ROSE")
  require("pROC")
  require("ResourceSelection")
  require("InformationValue")
  require("sjmisc")
  if(model$call[1]=="glm()") {
    pred <- predict(model, newdata=data, type="response")
  } else {
    pred <- predict(model, newdata=data, type="prob")[,2]
  }
  roc1 <- roc(data[,class], as.numeric(pred))
  acc <- accuracy.meas(data[,class], pred)
  hl <- hoslem.test(model$y, fitted(model), g=10)$p.value
  cm <- table(actual = data[,class], fitted = ifelse(pred>=0.5,1,0))
  mmce <- 1 - (sum(diag(cm))/sum(cm))
  d <- cod(model)$cod
  vld <- cbind(auc=roc1$auc,cimin=ci(roc1)[1],cimax=ci(roc1)[3],
               SRME=sqrt((sum((as.numeric(data[,class])-pred)^2))/nrow(data)), # error
               precision=acc$precision, recall=acc$recall, fscore=acc$F,
               NPV=npv(data[,class],pred),D=d,mmce=mmce,Hosmer_Lemeshow=hl)
  vld <- round(vld,3)
  return(vld)
}


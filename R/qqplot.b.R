
# This file is a generated template, your changes will not be overwritten

qqplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "qqplotClass",
    inherit = qqplotBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            # Compute the size according to facet
            if (userWidth * userHeight == 0) {
                width <- 400
                height <- 400
                if (!is.null(self$options$group))
                    width <- width + 75
            }
            if (userWidth >0)
                width = userWidth
            if (userHeight >0)
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },
        .run = function() {
            depVar <- self$options$dep
            groupVar <- self$options$group

            if (is.null(depVar))
                return(FALSE)

            data <- jmvcore::select(self$data, c(depVar,groupVar))
            data[[depVar]] <- jmvcore::toNumeric(data[[depVar]])

            data <- jmvcore::naOmit(data)

            if (self$options$transLog)
                data[[depVar]] <- log(data[[depVar]])

            if (self$options$standardize) {
                data[[depVar]] <- (data[[depVar]] - mean(data[[depVar]]))/sd(data[[depVar]])
            }
            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            depVar <- self$options$dep
            groupVar <- self$options$group

            if (is.null(depVar))
                return(FALSE)
            else
                depVar <- ensym(depVar)

            if( !is.null(groupVar) )
                groupVar <- ensym(groupVar)

            distrib <- self$options$distrib

            if (self$options$detrend)
                detrend = TRUE
            else
                detrend = FALSE

            identity = ifelse(self$options$refType == "identity", TRUE, FALSE)

            plotData <- image$state

            varMin <- min(plotData[[depVar]])
            varMax <- max(plotData[[depVar]])

            # Check data compatibility
            errMes <- NULL
            if (self$options$transLog &  !is.finite(varMin))
                errMes <- "Natural Log Tranform requires positive (>0) data."
            if (varMin <= 0 & distrib %in% c("lnorm", "chisq", "f", "gamma", "weibull"))
                errMes <- paste(private$.corresp(distrib), "distribution requires positive (>0) data.")
            if (varMin < 0 & distrib == "exp")
                errMes <- "Exponential distribution requires non-negative (≥0) data."
            if (distrib == "beta" & (varMin <=0 | varMax >= 1))
                errMes <- "Beta distribution requires data between 0 and 1."

            if (!is.null(errMes)) {
                self$results$paramTable$setVisible(FALSE)
                self$results$ErrorMessage$setVisible(TRUE)
                self$results$ErrorMessage$setContent(errMes)
                return(TRUE)
            } else {
                self$results$ErrorMessage$setVisible(FALSE)
                self$results$ErrorMessage$setContent("")
            }

            # Parameter estimations
            try(
                MASS::fitdistr(x = plotData[[depVar]], densfun = private$.corresp(distrib), start = private$.initVal(distrib))$estimate -> params ,
                silent = TRUE
            ) -> tryResult

            if (class(tryResult) == "try-error") {
                self$results$paramTable$setVisible(FALSE)
                self$results$ErrorMessage$setVisible(TRUE)
                self$results$ErrorMessage$setContent("Unable to estimate the distribution parameters")
                return(TRUE)
            }

            #options(scipen=999, digits=3)
            #self$results$ParameterValues$setContent(params)

            # Populate the Parameters table
            self$results$paramTable$getColumn('var')$setTitle(private$.distTitleName(distrib))
            for(i in seq_along(params)) {
                self$results$paramTable$addColumn(names(params[i]), type = 'number', format = 'zto')
            }
            self$results$paramTable$setRow(rowNo=1, params)
            self$results$paramTable$setCell(rowNo=1, col = 1, self$options$dep)


            plotTitle <- paste(
                            ifelse(detrend, "Detrended", "") ,
                            private$.distTitleName(distrib),
                            ifelse(self$options$type == "PP", "P-P", "Q-Q"),
                            "Plot of",
                            self$options$dep)

            #self$results$plot$setTitle(plotTitle)

            if (is.null(groupVar))
                plot <- ggplot(data = plotData, mapping = aes(sample = !!depVar))
            else
                plot <- ggplot(data = plotData, mapping = aes(sample = !!depVar, color = !!groupVar, fill = !!groupVar))

            if (self$options$band) {
                if (self$options$type == "PP") {
                    plot <- plot + qqplotr::stat_pp_band(distribution = distrib, bandType = self$options$methodPP, identity = identity, detrend = detrend, alpha=0.5)
                } else {
                    plot <- plot + qqplotr::geom_qq_band(distribution = distrib, bandType = self$options$methodQQ, identity = identity, detrend = detrend, alpha=0.5)
                }
            }

            if (self$options$type == "PP") {
                if (self$options$refLine) {
                    if (detrend)
                        plot <- plot + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "darkgrey")
                    else
                        plot <- plot + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "darkgrey")
                }
                plot <- plot + qqplotr::stat_pp_point(distribution = distrib, detrend = detrend, key_glyph = draw_key_rect)
            } else {
                if (self$options$refLine) {
                    if (is.null(groupVar))
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, identity = identity, detrend = detrend, color = "darkgrey")
                    else
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, identity = identity, detrend = detrend)
                }
                plot <- plot + qqplotr::stat_qq_point(distribution = distrib, identity = identity, detrend = detrend, key_glyph = draw_key_rect)
            }

            if (self$options$type == "PP")
                plot <- plot + labs(x = "Theoretical Probabilities", y = ifelse(detrend, "Sample Probability Deviation", "Sample Probabilities"))
            else
                plot <- plot + labs(x = "Theoretical Quantiles", y = ifelse(detrend, "Sample Quantile Deviation", "Sample Quantiles"))

            plot <- plot + ggtheme + guides(fill = guide_legend(override.aes = list(alpha = 1))) + labs(title = plotTitle)

            #plot <- plot + geom_hline(yintercept = 2)

            return (plot)
        },

        # Mod from https://github.com/aloy/qqplotr/blob/master/R/stat_pp_point.R
        .corresp = function(distName) {
            switch(
                distName,
                beta = "beta",
                cauchy = "cauchy",
                chisq = "chi-squared",
                exp = "exponential",
                f = "f",
                gamma = "gamma",
                lnorm = "log-normal",
                logis = "logistic",
                norm = "normal",
                weibull = "weibull",
                NULL
            )
        },

        .initVal = function(distName) {
            switch(
                distName,
                beta = list(shape1 = 1, shape2 = 1),
                chisq = list(df = 1),
                f = list(df1 = 1, df2 = 2),
                NULL
            )
        },

        .distTitleName = function(distName) {
            switch(
                distName,
                beta = "Beta",
                cauchy = "Cauchy",
                chisq = "Chi-Squared",
                exp = "Exponential",
                f = "F",
                gamma = "Gamma",
                lnorm = "Log-Normal",
                logis = "Logistic",
                norm = "Normal",
                weibull = "Weibull",
                NULL
            )
        }


        )
)

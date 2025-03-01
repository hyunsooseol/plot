
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

            if (is.null(self$options$dep)) {
                self$results$paramTable$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
                self$results$helpMessage$setVisible(TRUE)
                private$.showHelp()
            } else {
                self$results$helpMessage$setVisible(FALSE)
            }
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

            # Check data compatibility with distribution
            varMin <- min(plotData[[depVar]])
            varMax <- max(plotData[[depVar]])
            errMes <- NULL
            if (self$options$transLog && !is.finite(varMin))
                errMes <- "Natural Log Tranform requires positive (>0) data."
            if (varMin <= 0 & distrib %in% c("lnorm", "chisq", "f", "gamma", "weibull"))
                errMes <- paste(private$.corresp(distrib), "distribution requires positive (>0) data.")
            if (varMin < 0 & distrib == "exp")
                errMes <- "Exponential distribution requires non-negative (≥0) data."
            if (distrib == "beta" && (varMin <=0 || varMax >= 1))
                errMes <- "Beta distribution requires data between 0 and 1."

            if (!is.null(errMes)) {
                self$results$paramTable$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
                self$results$ErrorMessage$setVisible(TRUE)
                self$results$ErrorMessage$setContent(errMes)
                return(TRUE)
            }

            # Parameter estimations
            if (self$options$paramMethod == "paraEstimate") {
                if (self$options$paramEstMethod == "mle") {
                    try(
                        MASS::fitdistr(x = plotData[[depVar]], densfun = private$.corresp(distrib), start = private$.initVal(distrib))$estimate -> params,
                        silent = TRUE
                    ) -> tryResult
                } else { # paramEstMethod == "mme"
                    try(
                        private$.distParameters(distrib, plotData[[depVar]]) -> params,
                        silent = TRUE
                    ) -> tryResult
                }
                if (class(tryResult) == "try-error" || is.null(params)) {
                    self$results$paramTable$setVisible(FALSE)
                    self$results$plot$setVisible(FALSE)
                    self$results$ErrorMessage$setVisible(TRUE)
                    self$results$ErrorMessage$setContent("Unable to estimate the distribution parameters")
                    return(TRUE)
                }
            } else {
                params <- private$.userParams(distrib, as.numeric(self$options$param1), as.numeric(self$options$param2))
                if (is.null(params)) {
                    self$results$paramTable$setVisible(FALSE)
                    self$results$plot$setVisible(FALSE)
                    self$results$ErrorMessage$setVisible(TRUE)
                    self$results$ErrorMessage$setContent("Wrong parameter values.")
                    return(TRUE)
                }
            }

            # Everthing is OK
            self$results$ErrorMessage$setVisible(FALSE)
            self$results$ErrorMessage$setContent("")
            self$results$paramTable$setVisible(TRUE)
            self$results$plot$setVisible(TRUE)

            # Define the title of the plot (it will be set at the end)
            plotTitle <- jmvcore::format(.("{detrendStr}{distribStr} {typeStr} Plot of {varStr}"),
                                         detrendStr = ifelse(detrend, .("Detrended "), "") ,
                                         distribStr = .(private$.distTitleName(distrib)),
                                         typeStr = ifelse(self$options$type == "PP", .("P-P"), .("Q-Q")),
                                         varStr = ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep)
                                    )

            # Populate the Parameters table
            self$results$paramTable$getColumn('var')$setTitle(private$.distTitleName(distrib))
            for(i in seq_along(params)) {
                self$results$paramTable$addColumn(names(params[i]), type = 'number', format = 'zto')
            }
            self$results$paramTable$setRow(rowNo=1, params)
            self$results$paramTable$setCell(rowNo=1, col = 1, ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep))
            if (self$options$standardize)
                self$results$paramTable$setNote("1", .("Standardized values"), init=TRUE)
            if (self$options$paramMethod == "paraEstimate")
                self$results$paramTable$setTitle(paste0(.("Parameter Estimates"),ifelse(self$options$paramEstMethod == "mle", " (MLE)", " (MME)")))
            else
                self$results$paramTable$setTitle("Parameter Values")

            # Do the plot
            if (is.null(groupVar))
                plot <- ggplot(data = plotData, mapping = aes(sample = !!depVar))
            else
                plot <- ggplot(data = plotData, mapping = aes(sample = !!depVar, color = !!groupVar, fill = !!groupVar))

            if (self$options$type == "QQ") {
                if (self$options$band)
                    plot <- plot + qqplotr::geom_qq_band(distribution = distrib, dparams = params, bandType = self$options$methodQQ,
                                                         identity = identity, detrend = detrend, alpha=0.5)
                if (self$options$refLine) {
                    if (is.null(groupVar))
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, dparams = params, identity = identity,
                                                             detrend = detrend, color = "darkgrey")
                    else
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, dparams = params, identity = identity,
                                                             detrend = detrend)
                }
                plot <- plot + qqplotr::stat_qq_point(distribution = distrib, identity = identity, dparams = params,
                                                      detrend = detrend, key_glyph = draw_key_rect)
                if (detrend)
                    ylabStr <- jmvcore::format(.("{stdStr}Sample Quantiles Deviation"), stdStr = ifelse(self$options$standardize, .("Standardized "),""))
                else
                    ylabStr <- jmvcore::format(.("{stdStr}Sample Quantiles"), stdStr = ifelse(self$options$standardize, .("Standardized "),""))
                plot <- plot + labs(x = .("Theoretical Quantiles") , y = ylabStr)
            } else { # PP
                if (self$options$band)
                    plot <- plot + qqplotr::stat_pp_band(distribution = distrib, dparams = params, bandType = self$options$methodPP,
                                                         identity = identity, detrend = detrend, alpha=0.5)
                if (self$options$refLine) {
                    if (detrend)
                        plot <- plot + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "darkgrey")
                    else
                        plot <- plot + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "darkgrey")
                }
                plot <- plot + qqplotr::stat_pp_point(distribution = distrib, dparams = params,  detrend = detrend,
                                                      key_glyph = draw_key_rect)
                plot <- plot + labs(x = .("Theoretical Probabilities"), y = ifelse(detrend, .("Sample Probability Deviation"), .("Sample Probabilities")))
            }

            plot <- plot + ggtheme + guides(fill = guide_legend(override.aes = list(alpha = 1))) + labs(title = plotTitle)

            plot <- plot + theme(text=element_text(size=as.numeric(self$options$textSize)))

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
                chisq = "Chi-squared",
                exp = "Exponential",
                f = "F",
                gamma = "Gamma",
                lnorm = "Log-normal",
                logis = "Logistic",
                norm = "Normal",
                t = "Student",
                unif = "Uniform",
                weibull = "Weibull",
                NULL
            )
        },

        .userParams = function(distName, p1, p2) {
            params <- NULL
            if (distName == "beta" && p1 > 0 && p2 > 0) {
                params <- list(shape1 = p1, shape2 = p2)
            } else if (distName == "cauchy" && p2 > 0) {
                params <- list(location = p1, scale = p2)
            } else if (distName == "chisq" && p1 > 0) {
                params <- list(df = p1)
            } else if (distName == "exp" && p1 > 0) {
                params <- list(rate = p1)
            } else if (distName == "f" && p1 > 0 && p2 > 0) {
                params <- list(df1 = p1, df2 = p2)
            } else if (distName == "gamma" && p1 > 0 && p2 > 0) {
                params <- list(shape = p1, rate = p2)
            } else if (distName == "lnorm" && p2 > 0) {
                params <- list(meanlog = p1, sdlog = p2)
            } else if (distName == "logis" && p2 > 0) {
                params <- list(location = p1, scale = p2)
            } else if (distName == "norm" && p2 > 0) {
                params <- list(mean = p1, sd = p2)
            } else if (distName == "t" && p1 > 0) {
                params <- list(df = p1)
            } else if (distName == "unif" && p1 < p2) {
                params <- list(min = p1, max = p2)
            } else if (distName == "weibull" && p1 > 0 && p2 > 0) {
                params <- list(shape = p1, scale = p2)
            }
            return(params)
        },
        .distParameters = function(distName, aVar) {
            m <- mean(aVar)
            s <- sd(aVar)
            if (distName == "beta") {
                shape1 = m*(m*(1-m)/s**2 - 1)
                shape2 = (1-m)*(m*(1-m)/s**2 - 1)
                params <- list(shape1 = shape1, shape2 = shape2)
            } else if (distName == "exp") {
                rate <- 1/m
                params <- list(rate = rate)
            } else if (distName == "gamma") {
                shape = m**2/s**2
                rate = m/s**2
                params <- list(shape = shape, rate = rate)
            } else if (distName == "lnorm") {
                meanlog <- mean(log(aVar))
                sdlog <- sd(log(aVar))
                params <- list(meanlog = meanlog, sdlog = sdlog)
            } else if (distName == "logis") {
                location <- m
                scale <- sqrt(3)*s/pi
                params <- list(location = location, scale = scale)
            } else if (distName == "norm") {
                params <- list(mean = m, sd = s)
            } else if (distName == "unif") {
                params <- list(min = min(aVar), max = max(aVar))
            } else if (distName %in% c("cauchy","chisq", "f", "weibull","t")) {
                params <- NULL
            }
            return(params)
        },


        .showHelp = function(){
            self$results$helpMessage$setContent('
            	<style>
					.block {
  						border: 2px solid gray;
  						border-radius: 15px;
  						background-color: WhiteSmoke;
  						padding: 0px 20px;
  						text-align: justify;
					}
				</style>
            <div class="block">
            <p><strong>QQ & PP Plots Help</strong></p>

            <p>This module uses <a href = "https://CRAN.R-project.org/package=qqplotr" target="_blank">qqplotr R package<a/>
            by Alexandre Almeida, Adam Loy and Heike Hofmann. In-depth information can be found
            in the package documentation on CRAN site.</p>

            <p><strong>Reference line:</strong> Draws either the <em>identity line</em>
            	(y = x) or the commonly-used <em>Q-Q line</em> that intercepts two data quantiles
            	(Q<sub>0.25</sub> and Q<sub>0.75</sub>). P-P plot only supports identity line.</p>

            <p><strong>Confidence band:</strong> Draws a confidence band around the reference line.
			qqplotr package provides several methods to compute the confidence band :
			<ul>
            	<li><strong><em>Pointwise</em></strong> constructs pointwise confidence bands based on Normal confidence intervals;</li>
				<li><strong><em>Bootstrap</em></strong> creates pointwise confidence bands based on a parametric bootstrap;</li>
				<li><strong><em>Kolmogorov-Smirnov</em></strong> band is based on the Kolmogorov-Smirnov test;</li>
				<li><strong><em>Tail-Sensitive</em></strong> constructs a tail-sensitive confidence bands but is only implemented
				for Normal Q-Q plots;</li>
				<li><strong><em>Equal Local Levels (ELL)</em></strong> constructs simultaneous bands using the equal local levels.</li>
			</ul>
			P-P plots only support "ELL" and "Bootstrap" methods.
            </p>

            <p><strong>Detrended plot:</strong> The objects are <em>detrended</em>
            according to the reference line. This procedure may help reducing visual bias
            caused by the orthogonal distances from the points to the reference line.
            </p>

            <p><strong>Parameter values:</strong> The distribution parameters can be estimated from data
            using
            <ul>
            <li><strong>Maximum Likelihood Method</strong> (using MASS package) : it should work with all distributions but "t" and "uniform";</li>
            <li><strong>Method of Moments</strong> for moment based parameters : it should work with "normal", "log-normal", "Beta", "Exponential",
            "Gamma", "Logistic", "Uniform";</li>
            </ul>
            or entered by user:
            <ul>
            <li><strong>Parameter 1:</strong> : mean (Normal), meanlog (Log-normal), shape1 (Beta), location (Cauchy, Logistic),
            df (Chi-squared, Student), df1 (F), rate (Exponential), shape (Gamma, Weibull) and min (Uniform);</li>
            <li><strong>Parameter 2:</strong> : sd (normal), sdlog (Log-normal), shape2 (Beta), scale (Cauchy, Logistic),
            df2 (F), rate (Gamma, Weibull) and max (Uniform).</li>
            </ul>
            </p>

            </div>')
            }


        )
)

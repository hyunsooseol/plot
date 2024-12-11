
# This file is a generated template, your changes will not be overwritten

linechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linechartClass",
    inherit = linechartBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            width <- 600
            height <- 400
            # Compute the size according to options
            if (userWidth * userHeight == 0) {
                if (!is.null(self$options$group) || length(self$options$vars) > 1)
                        width <- 650
                if (self$options$rotateLabels)
                        height <- 450
            }
            if (userWidth >0)
                width = userWidth
            if (userHeight >0)
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },
        .run = function() {
            timeVar <- self$options$timeVar
            depVars <- self$options$vars
            groupVar <- self$options$group
            varNames <- c(timeVar, depVars, groupVar)
            if (length(depVars) == 0 || is.null(timeVar))
                return()
            data <- jmvcore::select(self$data, varNames)
            # Delete row with missing time
            data <- subset(data, !is.na(data[timeVar]))
            # Be sure dep var are numeric
            for (aVar in depVars)
                data[[aVar]] <- jmvcore::toNumeric(data[[aVar]])
            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            timeVar <- self$options$timeVar
            groupVar <- self$options$group
            depVars <- self$options$vars
            if (length(depVars) == 0 || is.null(timeVar))
                return(FALSE)

            plotData <- image$state
            timeVar <- ensym(timeVar)
            if (!is.null(groupVar))
                groupVar <- ensym(groupVar)

            # Time format
            timeVarIsDate <- self$options$isDate
            if (timeVarIsDate) {
                if (self$options$dateFormat == "auto") {
                    timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "iso")
                    if (is.null(timeVarAsDate)) {
                        timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "us")
                        if (is.null(timeVarAsDate)) {
                            timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "eu")
                        }
                    }
                } else {
                    timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], self$options$dateFormat)
                }

                if (!is.null(timeVarAsDate)) {
                    plotData[[timeVar]] <- timeVarAsDate
                } else {
                    jmvcore::reject(paste0(self$options$timeVar, .(" has not a valid date format.")))
                    timeVarIsDate <- FALSE
                }
            }

            dotSize <- self$options$dotSize
            lineWidth <- self$options$lineWidth

            if (is.null(groupVar))
                plot <- ggplot(plotData, aes(x = !!timeVar, group = 1))
            else
                plot <- ggplot(plotData, aes(x = !!timeVar, group = !!groupVar))

            for (varName in depVars) {
                aVar <- ensym(varName)

                if (is.null(groupVar))
                    plot <- plot + geom_line(aes(y = !!aVar, color = !!varName), size = lineWidth) #, linetype = as.numeric(self$options$lineType))
                else
                    if (length(depVars) > 1) {
#                        plot <- plot + geom_line(aes(y = !!aVar, color = interaction(!!varName, !!groupVar, sep=' : ') ), size = 1, linetype = as.numeric(self$options$lineType))
                        plot <- plot + geom_line(aes(y = !!aVar, color = !!varName, linetype = !!groupVar), size = lineWidth)
                    } else {
                        plot <- plot + geom_line(aes(y = !!aVar, color = !!groupVar ), size = lineWidth) #, linetype = as.numeric(self$options$lineType))
                    }

                if (self$options$showPoint)
                    plot <- plot + geom_point(aes(y = !!aVar), size = dotSize)
            }

            plot <- plot + ggtheme + scale_color_brewer(palette = self$options$colorPalette)

            if (timeVarIsDate) {
                Sys.setlocale("LC_TIME", .("en_US.utf-8"))
                plot <- plot + scale_x_date(date_labels = self$options$displayFormat, date_breaks = self$options$dateBreak)
            }

            if (self$options$rotateLabels)
                plot <- plot + theme(axis.text.x=element_text(angle=60, hjust=0.5))

            if (length(depVars) > 1 )
                plot <- plot + ylab("Values") + labs(color = "Variables") + guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))
            if (length(depVars) > 1 && is.null(groupVar))
                plot <- plot + labs(color = '')

            if (length(depVars) == 1 && is.null(groupVar))
                plot <- plot + theme(legend.position='none')

            return(plot)
        },
        .convertToDate = function(dAsString, fmt) {
            n <- length(na.omit(dAsString))

            if (fmt == "iso")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d"))
            else if (fmt == "us")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%m-%d-%Y", "%m/%d/%Y", "%m.%d.%Y", "%m%d%Y"))
            else if (fmt== "eu")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y", "%d%m%Y"))
            else
                dAsDate <- NULL

            if (length(na.omit(dAsDate)) != n || min(as.numeric(format(dAsDate, "%Y")), na.rm=T) < 100)
                dAsDate <- NULL

            return(dAsDate)
        })
)

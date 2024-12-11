
# This file is a generated template, your changes will not be overwritten

boxplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "boxplotClass",
    inherit = boxplotBase,
    private = list(
        .init = function() {
            if( is.null(self$options$group) ) {
                plotWidth <- 100 + 150*length(self$options$vars)
            } else {
                plotWidth <- 100 + 100*length(self$options$vars) * nlevels(self$data[[self$options$group]])
            }
            plotWidth <- min(max(plotWidth, 400),800)
            image <- self$results$plot
            image$setSize(plotWidth, 400)
        },
        .run = function() {
            groupVarName <- self$options$group
            depVarNames <- self$options$vars
            varNames <- c(groupVarName, depVarNames)
            if( length(depVarNames) == 0 )
              return()
            data <- jmvcore::select(self$data, varNames)
            # Remove case with missing group
            if( !is.null(groupVarName) ) {
              data <- subset(data, !is.na(data[groupVarName]))
            }
            # Be sure dep var are numeric
            for (varName in depVarNames)
              data[[varName]] <- jmvcore::toNumeric(data[[varName]])
            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if( length(self$options$vars) == 0 )
                return(FALSE)

            plotData <- image$state
            groupVarName <- self$options$group
            depVarNames <- self$options$vars

            if( is.null(groupVarName) ) {
                groupVar = NULL
            } else {
                groupVar <- ensym(groupVarName)
            }
            i <- 0
            plot <- ggplot(plotData)
            for (varName in depVarNames) {
                if (self$options$singleColor) {
                    i <- 1
                } else {
                    i <- i+1
                }
                aVar <- ensym(varName)
                if (is.null(groupVar)) {
                    plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!varName, fill = as.character(!!i)), outliers = self$options$showOutliers)
                    plot <- plot + guides(fill = FALSE)
                    if( self$options$showMean ) {
                        plot <- plot + stat_summary(aes(y = !!aVar, x = !!varName), fun = mean, geom = "point",
                                                    color='red', size=2)
                    }
                } else {
                    if (length(depVarNames) > 1) {
                        plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!varName, fill = !!groupVar), outliers = self$options$showOutliers)
                       #plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!varName, fill = reorder(!!groupVar, !!aVar, na.rm=TRUE, FUN = median)), outliers = self$options$showOutliers)
                        if (self$options$showMean) {
                          plot <- plot + stat_summary(aes(y = !!aVar, x = !!varName, group = !!groupVar), fun = mean, geom = "point",
                                                      position = position_dodge(.75), color='red', size=2)
                         }
                    } else {
                        if (self$options$order == "increasing")
                            plot <- plot + geom_boxplot(aes(y = !!aVar, x = reorder(!!groupVar, !!aVar, na.rm=TRUE, FUN = median), fill = !!groupVar), outliers = self$options$showOutliers)
                        else if (self$options$order == "decreasing")
                            plot <- plot + geom_boxplot(aes(y = !!aVar, x = reorder(!!groupVar, -!!aVar, na.rm=TRUE, FUN = median), fill = !!groupVar), outliers = self$options$showOutliers)
                        else
                            plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!groupVar, fill = !!groupVar), outliers = self$options$showOutliers)
                        if (self$options$showMean) {
                          plot <- plot + stat_summary(aes(y = !!aVar, x = !!groupVar), fun = mean, geom = "point",
                                                      color='red', size=2)
                        }
                    }
                }
            }
            if (length(depVarNames) > 1) {
                if (self$options$order == "none") {
                    plot <- plot + scale_x_discrete(limits=depVarNames)
                } else {
                    medians<-array()
                    for (i in 1:length(depVarNames)){
                        medians[i] <- median(plotData[[depVarNames[i]]], na.rm=TRUE)
                    }
                    plot <- plot + scale_x_discrete(limits=depVarNames[order(medians, decreasing = (self$options$order == "decreasing"))])
                }
            }
            plot <- plot + ggtheme
            if (self$options$colorPalette != 'jmv') {
                plot <- plot + scale_fill_brewer(palette = self$options$colorPalette)
            }
            if (!is.null(groupVar) && length(depVarNames) == 1)
                plot <- plot + theme(legend.position='none') + labs(x = groupVarName)
            else
                plot <- plot + labs(x = "")
            if (length(depVarNames) > 1)
              plot <- plot + labs(x = "", y = "")

            return(plot)
        }

        )
)


# This file is a generated template, your changes will not be overwritten

likertplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "likertplotClass",
    inherit = likertplotBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            image <- self$results$plot
            image$setSize(self$options$plotWidth, self$options$plotHeight)
        },
        .run = function() {
            if( length( self$options$liks) > 0  ) {
                plotData <- self$data[c(self$options$liks, self$options$group)]
                # Remove case with missing group
                if (!is.null(self$options$group) & self$options$ignoreNA) {
                    plotData <- subset(plotData, !is.na(plotData[self$options$group]))
                }
                # Change NA to "NA" (workaround to gglikert limitation with NA)
                if (!is.null(self$options$group) & !self$options$ignoreNA) {
                    plotData[[self$options$group]] <- factor(plotData[[self$options$group]], levels =  c(levels(plotData[[self$options$group]]),"NA"))
                    plotData[ is.na(plotData[,self$options$group]),self$options$group] <- "NA"
                }

                #plotData <- jmvcore::naOmit(plotData)
                if (self$options$toInteger) {
                    for (var in self$options$liks)
                        plotData[,var] <- as.integer(plotData[,var])
                }
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (length( self$options$liks) == 0)
                return(FALSE)
            plotData <- image$state
            textSize = self$options$textSize
            # Cleaning the group variable name (it would crash gglikert)
            if( ! is.null(self$options$group) ) {
                groupingVar <- jmvcore::toB64(self$options$group)
                names(plotData)[length(names(plotData))] <- groupingVar
            } else {
                groupingVar <- NULL
            }
            accuracy <- as.numeric(self$options$accuracy)
            # Doing the plot
            if( self$options$type == 'centered' ) {
                # Group setup
                if( ! is.null(groupingVar) ) {
                    if( self$options$groupBy == "variable" ) {
                        yOption <- groupingVar
                        facetRows <- vars(.question)
                    } else {
                        yOption <- ".question"
                        facetRows <- vars(!!ensym(groupingVar))
                    }
                } else {
                    yOption <- ".question"
                    facetRows <- NULL
                }
                # Do Likert Plot (centered)
                plot <- ggstats::gglikert(as_tibble(plotData), include = self$options$liks,
                                          sort = self$options$sorting,
                                          add_labels = self$options$addLabels,
                                          labels_size = (textSize/12)*3.5,
                                          labels_accuracy = accuracy,
                                          add_totals = self$options$addTotals,
                                          reverse_likert = self$options$reverseLikert,
                                          y = yOption, facet_rows = facetRows)
            } else {
                # Group setup
                if( ! is.null(groupingVar) ) {
                    if( self$options$groupBy == "variable" ) {
                        yOption <- groupingVar
                        facetRows <- vars(.question)
                    } else {
                        yOption <- ".question"
                        facetRows <- vars(!!ensym(groupingVar))
                    }
                } else {
                    yOption <- ".question"
                    facetRows <- NULL
                }
                # Do Likert Plot (stacked)
                plot <- ggstats::gglikert_stacked(as_tibble(plotData), include = self$options$liks,
                                                  sort = self$options$sorting,
                                                  add_labels = self$options$addLabels,
                                                  labels_size = (textSize/12)*3.5,
                                                  labels_accuracy = accuracy,
                                                  add_median_line = self$options$addMedianLine,
                                                  reverse_fill = ! self$options$reverseLikert,
                                                  y = yOption) + facet_grid(rows = facetRows)
            }
            plot <- plot + theme(text = element_text(size=textSize)) + scale_fill_brewer(palette = self$options$plotColor)
            return(plot)
        })
)

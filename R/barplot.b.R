
# This file is a generated template, your changes will not be overwritten

barplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "barplotClass",
    inherit = barplotBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)

            if( userWidth * userHeight == 0 ) {
                if( !is.null(self$options$columns)) {
                    width <- 600 #+ 50 * nlevels(self$data[[self$options$columns]]))
                } else {
                    width <- 500
                }
                if( !is.null(self$options$panel)) {
                    nbOfFacet <- nlevels(self$data[[self$options$panel]])
                    nbOfColumn <-self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn )

                    if( self$options$facetBy == "column" ) {
                        height <- max(400,300*nbOfRow)
                        width <- max(500, 200*nbOfColumn)
                    } else {
                        height <- max(400,300*nbOfColumn)
                        width <- max(500, 200*nbOfRow)
                    }
                } else {
                    height <- 400
                }
                if( self$options$legendAtBottom ) {
                    width <- width - 50
                    height <- height + 50
                }
            }
            if( userWidth >0 )
                width = userWidth
            if( userHeight >0 )
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },
        .run = function() {

            if( ! is.null(self$options$rows) ) {
                plotData <- self$data[c(self$options$rows, self$options$columns, self$options$panel)]
                if( self$options$ignoreNA )
                    plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            if( is.null(self$options$rows) )
                return(FALSE)
            plotData <- image$state
            rows <- self$options$rows
            rows <- ensym(rows)

            columns <- self$options$columns
            if( !is.null(columns))
                columns <- ensym(columns)

            if( self$options$borderColor == "none" )
                borderColor = NA
            else
                borderColor = self$options$borderColor

            # Percent format (scales)
            doPercent <- label_percent(accuracy = as.numeric(self$options$accuracy))

            # ggplot with base AES and sorting
            if( self$options$order == "decreasing")
                plot <- ggplot(plotData, aes(x = reorder(!!rows, !!rows, length, decreasing = TRUE)))
            else if( self$options$order == "increasing")
                plot <- ggplot(plotData, aes(x = reorder(!!rows, !!rows, length, decreasing = FALSE)))
            else
                plot <- ggplot(plotData, aes(x = !!rows))
            # Geom bar + labels
            plot <- plot + labs(x = rows)
            ## One variable
            if(is.null(columns)) {
                firstColorOfPalette <- jmvcore::colorPalette(n = 5, pal = self$options$colorPalette, type = "color")[1]
                # One variable with Percentage
                if( self$options$yaxis1var == "percent" ) {
                    if( self$options$singleColor ) {
                        plot <- plot + geom_bar(aes(y = after_stat(prop), group=1),
                                                fill = firstColorOfPalette, color = borderColor)
                    } else {
                        plot <- plot + geom_bar(aes(y = after_stat(prop), by = 1, fill = !!rows), stat = StatProp, color = borderColor)
                        plot <- plot + guides(fill = FALSE)
                    }
                    plot <- plot + scale_y_continuous(labels=percent_format())
                    plot <- plot + labs(y = "Percent")
                    if( self$options$showLabels ) {
                        plot <- plot + geom_text(aes(y = after_stat(prop), group=1, label = doPercent(after_stat(prop))), stat = StatProp, position = position_stack(vjust = 0.5),
                                             color = self$options$textColor, fontface = "bold")
                    }
                # One variable with Count
                } else {
                    if( self$options$singleColor ) {
                        plot <- plot + geom_bar(fill = firstColorOfPalette, color = borderColor)
                    } else {
                        plot <- plot + geom_bar(aes(fill = !!rows), color = borderColor)
                        plot <- plot + guides(fill = FALSE)
                    }
                    if( self$options$showLabels ) {
                        plot <- plot + geom_text(aes(label = after_stat(count), y = after_stat(count)),
                                                stat = "count", position = position_stack(vjust = 0.5),
                                             color = self$options$textColor, fontface = "bold")
                    }
                    plot <- plot + labs(y = "Count")
                }
            ## Two variables
            } else {
                plot <- plot + geom_bar(aes(fill = !!columns, by = !!rows), position = self$options$position, color = borderColor)
                # Two variables with Percentage (position = fill)
                if( self$options$position == "fill") {
                    plot <- plot + scale_y_continuous(labels=percent_format())
                    if( self$options$showLabels ) {
                        plot <- plot + geom_text(aes(fill = !!columns, by = !!rows, label=doPercent(after_stat(prop))), stat = StatProp, position = position_fill(.5),
                                             color = self$options$textColor, fontface = "bold")
                    }
                    plot <- plot + labs(y = "Percent")
                # Two variables with count (dodge)
                } else if( self$options$position == "dodge" || self$options$position == "dodge2") {
                    if( self$options$showLabels ) {
                        plot <- plot + geom_text(aes(fill = !!columns, label = after_stat(count), y = after_stat(count/2)),
                                         position = position_dodge(width = 0.9),
                                         stat = "count",
                                         color = self$options$textColor, fontface = "bold")
                    }
                    plot <- plot + labs(y = "Count")
                # Two variables with count (staked)
                } else { # Stacked
                    plot <- plot + labs(y = "Count")
                    if( self$options$showLabels ) {
                        plot <- plot + geom_text(aes(fill = !!columns, label = after_stat(count), y = after_stat(count)),
                                             position = position_stack(vjust = 0.5),
                                             stat = "count",
                                             color = self$options$textColor, fontface = "bold")
                    }
                    plot <- plot + labs(y = "Count")
                }
            }

            # Horizontal Plot
            if( self$options$horizontal )
                plot <- plot + coord_flip()

            # Panel
            if( !is.null(self$options$panel) ) {
                panelVar <- self$options$panel
                panelVar <- ensym(panelVar)
                if( self$options$facetBy == "column")
                    plot <- plot + facet_wrap(vars(!!panelVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + facet_wrap(vars(!!panelVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme
            if( self$options$colorPalette != 'jmv' ) {
                plot <- plot + scale_fill_brewer(palette = self$options$colorPalette, na.value="grey")
            }

            # Legend position
            if( self$options$legendAtBottom )
                plot <-plot + theme(legend.position="bottom")

            return(plot)
        })
)

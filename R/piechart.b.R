
# This file is a generated template, your changes will not be overwritten

piechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "piechartClass",
    inherit = piechartBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            width <- 600
            if( !is.null(self$options$panel)) {
                height <- max(400,350*nlevels(self$data[[self$options$panel]]))
            } else {
                height <- 400
            }
            if(self$options$legendBottom) {
                height <- height + 50
                width <- width - 50
            }
            image <- self$results$plot
            image$setSize(width, height)
        },

        .run = function() {
            if( ! is.null(self$options$aVar) ) {
                plotData <- self$data[c(self$options$aVar, self$options$panel)]
                plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {
            if( is.null(self$options$aVar) )
                return(FALSE)
            plotData <- image$state
            aVar <- self$options$aVar
            aVar <- ensym(aVar)

            if( !is.null(self$options$panel) ) {
              panelVar <- self$options$panel
              panelVar <- ensym(panelVar)
            } else {
              panelVar <- NULL
            }

            # set the border color
            if( self$options$borderColor == "none") {
              borderColor <- NA
            } else {
              borderColor = self$options$borderColor
            }

            if(self$options$donut) {
                plot <- ggplot(plotData, aes(x=10, fill= !!aVar)) + xlim(c(8.5,10.5))
            } else {
                plot <- ggplot(plotData, aes(x="", fill= !!aVar))
            }

            plot <- plot + geom_bar(color=borderColor) + coord_polar("y")

            # Labels
            if( self$options$showLabels && self$options$labels == "count" ) {
                plot <- plot + geom_text(aes(label = after_stat(count)), stat = "count",
                                    position = position_stack(vjust = 0.5),
                                    color=self$options$textColor, fontface = "bold", size = 5)
            } else if( self$options$showLabels && self$options$labels == "percent" ) {
                plot <- plot + geom_text(aes(label = paste(100*round(after_stat(count/sum(count)),2),"%", sep="")),
                                        stat = "count", position = position_stack(vjust = 0.5),
                                        color=self$options$textColor, fontface = "bold", size = 5)
            }


            if( !is.null(panelVar) ) {
              plot <- plot + facet_wrap(vars(!!panelVar), ncol=1, scales = "free")

            }

            # Theme and colors
            plot <- plot + ggtheme + labs(x = "", y = "")
            plot <- plot + theme(axis.ticks = element_blank(),
                                 axis.line.x = element_blank(), axis.line.y = element_blank(),
                                 axis.text.x=element_blank(),axis.text.y=element_blank(),
                                 panel.grid.major = element_blank(),panel.grid.minor = element_blank())
            if( self$options$colorPalette != 'jmv' ) {
              plot <- plot + scale_fill_brewer(palette = self$options$colorPalette)
            }

            if(self$options$legendBottom) {
              plot <- plot + theme(legend.position="bottom")
            }

            return(plot)
        }




        )
)

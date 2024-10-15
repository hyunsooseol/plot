
# This file is a generated template, your changes will not be overwritten

histogramClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "histogramClass",
    inherit = histogramBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            image <- self$results$plot
            if( !is.null(self$options$panel)) {
                image$setSize(600, max(400, 250*nlevels(self$data[[self$options$panel]])))
            }
        },
        .run = function() {
            if( ! is.null(self$options$aVar) ) {
                plotData <- self$data[c(self$options$aVar, self$options$group, self$options$panel)]
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
          if( is.null(self$options$aVar) )
            return(FALSE)
          plotData <- image$state

          xVar <- self$options$aVar
          xVar <- ensym(xVar)

          if( !is.null(self$options$group) ) {
              groupVar <- self$options$group
              groupVar <- ensym(groupVar)
          } else {
              groupVar <- NULL
          }

          if( !is.null(self$options$panel) ) {
            panelVar <- self$options$panel
            panelVar <- ensym(panelVar)
          } else {
            panelVar <- NULL
          }

          # Set bin width and boundary
          if( self$options$binWidth == 0 )
              binWidth <- NULL
          else
              binWidth <- self$options$binWidth
          if( self$options$binBoundary == 0 )
              binBoundary <- NULL
          else
              binBoundary <- self$options$binBoundary

          # set the border color
          if( self$options$borderColor == "none") {
              borderColor <- NA
          } else {
              borderColor = self$options$borderColor
          }
          # set the fill color
          if( self$options$fillColor == "none") {
            fillColor <- NA
          } else {
            fillColor = self$options$fillColor
          }

          # Define geom_historgram argument list
          if( self$options$histtype == "density" ) {
              hist_arg = list( aes(y = after_stat(density)) )
          } else {
              hist_arg = list()
          }
          if( self$options$grouping != "none" ) {
              hist_arg[["position"]] <- self$options$grouping
              if( self$options$grouping == "identity" && ( fillColor != "white" || self$options$usePalette == "forFilling" ) ) {
                  hist_arg[["alpha"]] <- 0.5
              }
              if( self$options$usePalette == "forFilling" ) {
                hist_arg[["color"]] <- borderColor
              } else {
                hist_arg[["fill"]] <- fillColor
              }
          } else {
            hist_arg[["fill"]] <- fillColor
            hist_arg[["color"]] <- borderColor
          }
          hist_arg[["binwidth"]] <- binWidth
          hist_arg[["boundary"]] <- binBoundary

          plot <- ggplot(plotData, aes(x = !!xVar, fill= !!groupVar, color = !!groupVar))
          plot <- plot + do.call(geom_histogram, hist_arg)

          plot <- plot + labs(x=self$options$aVar)

          if (self$options$normalCurve && self$options$grouping == "none" && self$options$histtype == "density")
              plot <- plot + stat_function(fun = dnorm, #dlnorm
                        args = list(mean = mean(plotData[[1]], na.rm=TRUE),
                                    sd = sd(plotData[[1]], na.rm=TRUE)),
                        color = "red", linewidth = 1, na.rm=TRUE)

          if( self$options$histtype == "density" )
              plot <- plot + labs(y="Density") + scale_y_continuous(labels = scales::comma)
          else
              plot <- plot + labs(y="Count")

          if( !is.null(panelVar) ) {
              plot <- plot + facet_wrap(vars(!!panelVar), ncol=1)

          }

          plot <- plot + ggtheme + scale_fill_brewer(palette = self$options$colorPalette) + scale_colour_brewer(palette = self$options$colorPalette)

          return(plot)
        })
)

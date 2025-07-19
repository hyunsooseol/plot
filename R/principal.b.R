
# This file is a generated template, your changes will not be overwritten

principalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "principalClass",
    inherit = principalBase,
    private = list(
        .init = function() {

            if (!is.null(self$options$groupVar)) {
                image <- self$results$obsPlot
                n <- max(nchar(levels(self$data[[self$options$groupVar]])))
                width <- image$width + 50 + n*15
                image$setSize(width, image$height)
             }
        },
        .run = function() {
            if (is.null(self$options$vars) || length(self$options$vars) < 2)
                return()

            nDim <- self$options$dimNum


            if (self$options$missingValues == "mean")
                missing <- TRUE
            else
                missing <- FALSE

            if (self$options$missingValues == "listwise")
                use <- "complete"
            else
                use <- "pairwise"

            if (nDim > length(self$options$vars))
                reject("The number of dimensions cannot be greater than the number of variables")
            if (self$options$xaxis > nDim || self$options$yaxis > nDim)
                reject("X-Axis and Y-Axis cannot be greater than the number of dimensions")
            if (self$options$xaxis == self$options$yaxis)
                reject("X-Axis and Y-Axis cannot be equal")

            tmpData <- self$data[,self$options$vars]
            if (self$options$missingValues == "mean")
                tmpData[] <- lapply(tmpData, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

            corrMat <- cor(tmpData, use = use)
            if (abs(det(corrMat)) < 1e-10)
                reject("The correlation matrix is not positive-definite. You may have more variables than observations!")

            # KMO & Bartlett's test
            if (self$options$showKMO) {
                kmo <- psych::KMO(corrMat)
                bartlett <- psych::cortest.bartlett(corrMat)

                self$results$kmoTable$setRow(rowNo = 1,
                                             values = list(test = "Bartlett's Test of Sphericity",
                                                           statistic = bartlett$chisq,
                                                           df = bartlett$df, p = bartlett$p.value))
                self$results$kmoTable$setRow(rowNo = 2,
                                             values = list(test = "Kaiser-Meyer-Olkin Measure of Sampling Adequacy (MSA)",
                                                            statistic = kmo$MSA,
                                                            df = NULL, p = NULL))
            }


            # Compute observation QLT
            if (self$options$showObservations) {
                # compute original norm^2
                zscores <- sapply(self$data[,self$options$vars], function(df) (df-mean(df, na.rm = TRUE))/sd(df, na.rm = TRUE))
                norm2 <- rowSums(zscores**2)
                # pca without rotation
                res <- psych::principal(self$data[,self$options$vars], nfactors = nDim, rotate = "none",
                                        impute = "mean", missing = missing, use = use)
                eigenvalues <- res$Vaccounted["SS loadings",]
                # Compute norm^2 of projections
                norm2pca <- res$scores**2 %*% eigenvalues
                # then QLT
                qlt <- norm2pca / norm2
            }

            # PCA computation
            if (self$options$rotation == "varimax")
                res <- psych::principal(self$data[,self$options$vars], nfactors = nDim,
                                        rotate = self$options$rotation, impute = "mean",
                                        missing = missing, use = use, eps = 1e-14)
            else
                res <- psych::principal(self$data[,self$options$vars], nfactors = nDim,
                                        rotate = self$options$rotation, impute = "mean",
                                        missing = missing, use = use)



            if (!self$options$stdScores)
                res$scores <- t(t(res$scores) * sqrt(res$Vaccounted["SS loadings",]))
            res$scores <- as.data.frame(res$scores)



            if (!is.null(self$options$labelVar))
                rownames(res$scores) <- self$data[[self$options$labelVar]]
            if (!is.null(self$options$groupVar))
                res$scores <- cbind(res$scores, group = self$data[[self$options$groupVar]])

            self$results$text$setContent(res$scores)

            # Rotation string
            rotation = paste0(toupper(substring(self$options$rotation, 1, 1)), substring(self$options$rotation, 2))

            # Summary Table
            if (self$options$showSummary) {
                eigen <- res$values
                eigenSum <- sum(eigen)
                eigenCum <- cumsum(eigen)
                for (i in 1:nDim) { # first dimensions
                    self$results$summaryTable$addRow(rowKey = i,
                                                     list(comp = i,
                                                          eigenvalue = eigen[i],
                                                          initVarProp = eigen[i]/eigenSum,
                                                          initVarCum = eigenCum[i]/eigenSum,
                                                          loadings = res$Vaccounted["SS loadings",i],
                                                          varProp = res$Vaccounted["Proportion Var",i],
                                                          varCum = res$Vaccounted["Cumulative Var",i]
                                                     ))
                }
                if (length(eigen) > nDim) { # is there more dimensions ?
                    for (i in (nDim+1):length(eigen)) {
                        self$results$summaryTable$addRow(rowKey = i,
                                                         list(comp = i,
                                                              eigenvalue = eigen[i],
                                                              initVarProp = eigen[i]/eigenSum,
                                                              initVarCum = eigenCum[i]/eigenSum,
                                                              loadings = NULL,
                                                              varProp = NULL,
                                                              varCum = NULL
                                                         ))
                    }
                }
                if (rotation != "None")
                    self$results$summaryTable$setNote('rot',
                                                      jmvcore::format(.("'{rotation}' rotation was used."), rotation = rotation))
            }

            # Loading Table
            if (self$options$showLoadings) {
                for(i in 1:nDim) {
                    self$results$loadingTable$addColumn(name = paste0("loading:",i), title = as.character(i), superTitle = "Component", type = "number", format = "zto")
                }
                self$results$loadingTable$addColumn(name = "QLT", title = "Communalities", type = "number")
                loadings <- as.data.frame.array(res$loadings)
                for(aVar in rownames(loadings)) {
                    values = list()
                    for(i in 1:nDim) {
                        values[[paste0("loading:",i)]] <- loadings[aVar, i]
                    }
                    values[["QLT"]] <- res$communality[aVar]
                    self$results$loadingTable$setRow(rowKey = aVar, values = values)
                }
                if (rotation != "None")
                    self$results$loadingTable$setNote('rot',
                                                  jmvcore::format(.("'{rotation}' rotation was used."), rotation = rotation))
            }

            # Observation Table
            if (self$options$showObservations) {
                if (is.null(self$options$labelVar))
                    self$results$obsTable$addColumn("obs", title = "Observation", type = "integer")
                else
                    self$results$obsTable$addColumn("obs", title = self$options$labelVar, type = "text")
                for(i in 1:nDim) {
                    self$results$obsTable$addColumn(as.character(i), title = as.character(i), , superTitle = "Component", type = "number", format = "zto")
                }
                self$results$obsTable$addColumn("qlt", title = "QLT", type = "number", format = "zto")
                for (i in 1:nrow(res$scores)) {
                    values = list()
                    if (is.null(self$options$labelVar))
                        values["obs"] <- i
                    else
                        values["obs"] <- rownames(res$scores)[i]
                    values["qlt"] <- qlt[i]
                    for(j in 1:nDim)
                        values[as.character(j)] <- res$scores[i,j]
                    self$results$obsTable$addRow(rowKey = i, values = values)
                }
                if (rotation != "None")
                    self$results$obsTable$setNote('rot',
                                                      jmvcore::format(.("'{rotation}' rotation was used."), rotation = rotation))
            }

            # Plots
            if (self$options$showScreePlot) {
                screeplot <- self$results$screePlot
                screeplot$setState(res$values)
            }
            if (self$options$showVarPlot) {
                varplot <- self$results$varPlot
                varplot$setState(res)
            }
            if (self$options$showObsPlot) {
                obsplot <- self$results$obsPlot
                obsplot$setState(res)
            }
            if (self$options$showBiplot) {
                biplot <- self$results$biPlot
                biplot$setState(res)
            }
        },
        .screeplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            nd <- length(res)
            plot <- ggplot(NULL,aes(x=1:nd, y=res))
            plot <- plot + geom_line(size=0.8) + geom_point(size=3, color="darkgrey")

            plot <- plot + labs(x = "Component", y = "Eigenvalues")
            plot <- plot + scale_x_continuous(breaks = 1:nd)
            #plot <- plot + theme(legend.position = c(1, 1), legend.justification = c(1, 1))
            plot <- plot + ggtheme

            return(plot)
        },
        .varplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            propIn <- round(res$Vaccounted["Proportion Var",]*100,1)

            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis

            c1string <- paste0("Component ", dim1, " (", propIn[dim1],"%)")
            c2string <- paste0("Component ", dim2, " (", propIn[dim2],"%)")

            imgData <- as.data.frame.array(res$loadings)

            c1 <- names(imgData)[self$options$xaxis]
            c1 <- ensym(c1)
            c2 <- names(imgData)[self$options$yaxis]
            c2 <- ensym(c2)

            plot <-  ggplot(imgData)
            plot <- plot + geom_segment(aes(x = 0, y = 0, xend = !!c1, yend = !!c2),
                                        arrow = arrow(length = unit(0.05, "inches"), type = "closed"),
                                        color="blue")
            plot <- plot + ggrepel::geom_text_repel(aes(x = !!c1, y = !!c2, label = rownames(imgData)), check_overlap = TRUE)

            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)
            plot <- plot + ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), linewidth = 0.2, n=720)
            plot <- plot + coord_fixed(xlim = c(-1,1), ylim = c(-1,1))
            plot <- plot + labs(x = c1string, y = c2string)
            plot <- plot + ggtheme

            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))

            return(plot)
        },
        .obsplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            propIn <- round(res$Vaccounted["Proportion Var",]*100,1)

            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis

            c1string <- paste0("Component ", dim1, " (", propIn[dim1],"%)")
            c2string <- paste0("Component ", dim2, " (", propIn[dim2],"%)")

            imgData <- as.data.frame(res$scores)

            c1 <- names(imgData)[dim1]
            c1 <- ensym(c1)
            c2 <- names(imgData)[dim2]
            c2 <- ensym(c2)

            #self$results$text$setContent(imgData)

            xmin = 1.01*min(imgData[[dim1]])
            xmax = 1.01*max(imgData[[dim1]])
            ymin = 1.01*min(imgData[[dim2]])
            ymax = 1.01*max(imgData[[dim2]])

            if (!is.null(self$options$groupVar)) {
                plot <-  ggplot(imgData, aes(x = !!c1, y = !!c2, label = rownames(imgData), color = group))
                plot <- plot + geom_point(size = 3) #color="red")
            } else {
                plot <-  ggplot(imgData, aes(x = !!c1, y = !!c2, label = rownames(imgData)))
                plot <- plot + geom_point(color="red", size = 2)
            }

            if (!is.null(self$options$labelVar))
                plot <- plot + ggrepel::geom_text_repel(check_overlap = TRUE, color="black", box.padding = 0.4, min.segment.length = 0.6)
            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)
            #plot <- plot + coord_cartesian(xlim = c(-xmax,xmax), ylim = c(-ymax,ymax))
            plot <- plot + coord_fixed(xlim = c(xmin,xmax), ylim = c(ymin,ymax))

            if (!is.null(self$options$groupVar))
                plot <- plot + labs(x = c1string, y = c2string, color = self$options$groupVar)
            else
                plot <- plot + labs(x = c1string, y = c2string)

            plot <- plot + ggtheme

            #self$results$text$setContent( nlevels(imgData[["group"]]) )

            if (!is.null(self$options$groupVar)) {
                n <- nlevels(imgData[["group"]])
                plot <- plot + scale_color_manual(values = jmvcore::colorPalette(n=n, theme$palette, type="color"), na.value="grey55")
            }

            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))

            return(plot)
        },
        .biplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            propIn <- round(res$Vaccounted["Proportion Var",]*100,1)

            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis

            c1string <- paste0("Component ", dim1, " (", propIn[dim1],"%)")
            c2string <- paste0("Component ", dim2, " (", propIn[dim2],"%)")

            imgData <- as.data.frame(res$scores)

            c1 <- names(imgData)[dim1]
            c1 <- ensym(c1)
            c2 <- names(imgData)[dim2]
            c2 <- ensym(c2)

            #self$results$text$setContent(imgData)

            xmin = 1.01*min(imgData[[dim1]])
            xmax = 1.01*max(imgData[[dim1]])
            ymin = 1.01*min(imgData[[dim2]])
            ymax = 1.01*max(imgData[[dim2]])

            if (!is.null(self$options$groupVar)) {
                plot <-  ggplot(imgData, aes(x = !!c1, y = !!c2, label = rownames(imgData), color = group))
                plot <- plot + geom_point(size = 3) #color="red")
            } else {
                plot <-  ggplot(imgData, aes(x = !!c1, y = !!c2, label = rownames(imgData)))
                plot <- plot + geom_point(color="red", size = 2)
            }

            if (!is.null(self$options$labelVar))
                plot <- plot + ggrepel::geom_text_repel(check_overlap = TRUE, color="black", box.padding = 0.4, min.segment.length = 0.6)
            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)
            #plot <- plot + coord_cartesian(xlim = c(-xmax,xmax), ylim = c(-ymax,ymax))
            plot <- plot + coord_fixed(xlim = c(xmin,xmax), ylim = c(ymin,ymax))

            if (!is.null(self$options$groupVar))
                plot <- plot + labs(x = c1string, y = c2string, color = self$options$groupVar)
            else
                plot <- plot + labs(x = c1string, y = c2string)

            plot <- plot + ggtheme

            #self$results$text$setContent( nlevels(imgData[["group"]]) )

            if (!is.null(self$options$groupVar)) {
                n <- nlevels(imgData[["group"]])
                plot <- plot + scale_color_manual(values = jmvcore::colorPalette(n=n, theme$palette, type="color"), na.value="grey55")
            }

            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))

            return(plot)
        }

        )
)


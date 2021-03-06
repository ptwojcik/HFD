
#---------------------------------------------------------------------
#### plotHeatmap ####

# function plotting strategy summary
# in a form of a heatmap

plotHeatmap <- function(data_plot, # dataset (data.frame) with calculations
                        col_vlabels, # column name with the labels for a vertical axis (string)
                        col_hlabels, # column name with the labels for a horizontal axis (string)
                        col_variable, # column name with the variable to show (string)
                        main,      # title
                        label_size = 6, # size of labels
                        save_graph = FALSE, # whether to save the graph
                        width = 12,
                        height = 8,
                        file_name = NULL) { # filename for saving

  require(ggplot2)
  require(dplyr)
  
  # sprawdz czy nie za duzo wierszy i zwroc komunikat
  
  
  data_plot$labels_ <- round(data_plot[, col_variable], 2)
  data_plot[, col_hlabels] <- as.factor(data_plot[, col_hlabels])
  data_plot[, col_vlabels] <- as.factor(data_plot[, col_vlabels])
  

  p1 <- ggplot(data_plot, 
               aes_string(x = col_hlabels, 
                          y = col_vlabels)) +
    geom_raster(aes_string(fill = col_variable)) +
    theme_bw() +
    xlab(col_hlabels) +
    ylab(col_vlabels) +
    ggtitle(main) +
    scale_fill_gradient2(low = "red",
                         high = "darkgreen",
                         mid = "white",
                         midpoint = 0) +
    geom_label(aes_string(label = "labels_"),
               size = label_size) +
    theme(legend.position = "bottom", 
          legend.key.width = unit(2, "cm"))
  
  if(save_graph) { 
    if(is.null(file_name)) stop("Please provide the file_name= argument") else
      ggsave(filename = file_name, 
             plot = p1, 
             units = "in",
             width = width, 
             height = height)
  }
  
  return(p1)
}


#---------------------------------------------------------------------
#### plotPositionsMA ####

# function plotting strategy framework wor a single 
# moving average entry technique for a selected day

plotPositionsMA <- function(data_plot, # dataset (xts) with calculations
                            date_plot, # date for which plotting is done: YYYY-MM-DD
                            col_price, # column name with the price (string)
                            col_ma,    # column name with the moving average/median (string)
                            col_pos,   # column name with the position (string)
                            main,      # title
                            save_graph = FALSE, # whether to save the graph
                            width = 12,
                            height = 8,
                            file_name = NULL) { # filename for saving

  require(ggplot2)
  require(dplyr)
  require(tibble)
  require(lubridate)
  require(scales)
  
  data_plot_ <- data_plot %>% 
    .[date_plot] %>% 
    data.frame() %>% 
    rownames_to_column("Time") %>% 
    select(Time, col_price, col_pos,
           col_ma) %>% 
    mutate(Time = ymd_hms(Time),
	       next_Time = dplyr::lead(Time)) %>% 
    rename("position" = col_pos)
  
  pos_cols <- c("-1" = "red", 
                "0" = "gray",
                "1" = "green")
  
  p1 <- ggplot(data_plot_) + 
    geom_rect(aes(xmin = Time,
                  xmax = next_Time, 
                  ymin = -Inf, 
                  ymax = Inf, 
                  fill = factor(position)),
              alpha = 0.2, 
              na.rm = FALSE) +
    scale_fill_manual(values = pos_cols) +
    geom_line(aes_string(x = "Time",
                         y = col_price),
              col = "black", 
              na.rm = FALSE) +
    geom_line(aes_string(x = "Time",
                         y = col_ma),
              col = "blue",
              size = 1.5,
              na.rm = FALSE) + 
    theme_bw() +
    labs(x = "time", 
         y = "position entry signals", 
         fill = "position", 
         title = main) +
    theme(legend.position = "bottom") +
    scale_x_datetime( breaks = date_breaks("15 mins"), 
                      labels = date_format("%H:%M"), 
                      expand = c(0,0))
  
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

#---------------------------------------------------------------------
#### plotPositions2MAs ####


# function plotting strategy framework wor a single 
# moving average entry technique for a selected day

plotPositions2MAs <- function(data_plot, # dataset (xts) with calculations
                              date_plot, # date for which plotting is done: YYYY-MM-DD
                              col_price, # column name with the price (string)
                              col_fma,   # column name with the fast moving average/median (string)
                              col_sma,   # column name with the slow moving average/median (string)
                              col_pos,   # column name with the position (string)
                              main,
                              save_graph = FALSE, # whether to save the graph
                              width = 12,
                              height = 8,
                              file_name = NULL) {
  
  require(ggplot2)
  require(dplyr)
  require(tibble)
  require(lubridate)
  require(scales)
  
  data_plot_ <- data_plot %>% 
    .[date_plot] %>% 
    data.frame() %>% 
    rownames_to_column("Time") %>% 
    select(Time, col_price, col_pos,
           col_fma, col_sma) %>% 
    mutate(Time = ymd_hms(Time)) %>% 
    rename("position" = col_pos)
  
  pos_cols <- c("-1" = "red", 
                "0" = "gray",
                "1" = "green")
  
  p1 <- ggplot(data_plot_) + 
    geom_rect(aes(xmin = Time,
                  xmax = dplyr::lead(Time), 
                  ymin = -Inf, 
                  ymax = Inf, 
                  fill = factor(position)),
              alpha = 0.2) +
    scale_fill_manual(values = pos_cols) +
    geom_line(aes_string(x = "Time",
                         y = col_price),
              col = "black") +
    geom_line(aes_string(x = "Time",
                         y = col_fma),
              col = "blue",
              size = 1.5) +
    geom_line(aes_string(x = "Time",
                         y = col_sma),
              col = "green",
              size = 1.5) + 
    theme_bw() +
    labs(x = "time",
         y = "position entry signals", 
         fill = "position", 
         title = main) +
    theme(legend.position = "bottom") +
    scale_x_datetime( breaks = date_breaks("15 mins"), 
                      labels = date_format("%H:%M"), 
                      expand = c(0,0))
  
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


#---------------------------------------------------------------------
#### plotPositions2MAs ####


# function plotting strategy framework wor a single 
# moving average entry technique for a selected day

plotPositionsVB <- function(data_plot,  # dataset (xts) with calculations
                            date_plot,  # date for which plotting is done: YYYY-MM-DD
                            col_signal, # column name with the signal (string)
                            col_upper,  # column name with the upper bound (string)
                            col_lower,  # column name with the lower bound (string)
                            col_pos,   # column name with the position (string)
                            main,
                            save_graph = FALSE, # whether to save the graph
                            width = 12,
                            height = 8,
                            file_name = NULL) {
  
  require(ggplot2)
  require(dplyr)
  require(tibble)
  require(lubridate)
  require(scales)
  
  data_plot_ <- data_plot %>% 
    .[date_plot] %>% 
    data.frame() %>% 
    rownames_to_column("Time") %>% 
    select(Time, col_pos,
           col_signal, col_lower, col_upper) %>% 
    mutate(Time = ymd_hms(Time)) %>% 
    rename("position" = col_pos)
  
  pos_cols <- c("-1" = "red", 
                "0" = "gray",
                "1" = "green")
  
  p1 <- ggplot(data_plot_) + 
    geom_rect(aes(xmin = Time,
                  xmax = dplyr::lead(Time), 
                  ymin = -Inf, 
                  ymax = Inf, 
                  fill = factor(position)),
              alpha = 0.2) +
    scale_fill_manual(values = pos_cols) +
    geom_line(aes_string(x = "Time",
                         y = col_signal),
              col = "black") +
    geom_line(aes_string(x = "Time",
                         y = col_upper),
              col = "blue",
              size = 1.5) +
    geom_line(aes_string(x = "Time",
                         y = col_lower),
              col = "blue",
              size = 1.5) + 
    theme_bw() +
    labs(x = "time",
         y = "position entry signals", 
         fill = "position", 
         title = main) +
    theme(legend.position = "bottom") +
    scale_x_datetime( breaks = date_breaks("15 mins"), 
                      labels = date_format("%H:%M"), 
                      expand = c(0,0))
  
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

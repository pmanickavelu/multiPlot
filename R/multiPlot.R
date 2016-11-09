#' multi line plotly
#'
#' Reading a DF of config with path of the csv file with data, the x column, y column n, legend and color for the line.
#'
#' @param config a data.frame of length n
#' the config data.frame must contain a files column which holds the location of the file, a x column which tells the column name to be used for the x axis, and y column for y axis. the data.frame may also contain a trace_names column for the legend.
#' @return pp a plot form plotly
#' @author Prabhu Manickavelu
#' @export
#' @import plotly
#' @importFrom utils read.csv

multiPlot <- function(config) {
  if(!is.data.frame(config)){
      print("Config is not a data frame")
      return()
  }
    pp <- NULL
    for(i in seq(nrow(config))){
        ds<-read.csv(config[i,"files"], header = T,stringsAsFactors = F)
        if(is.null(pp)){
            pp <- plot_ly(x = ds[[config[i,"x"]]], y= ds[[config[i,"y"]]], type = 'scatter', mode = 'lines+markers', name = config[i,"trace_names"])
        }
        else{
            pp <- add_trace(pp,y= ds[[config[i,"y"]]], mode = 'lines+markers', name = config[i,"trace_names"])
        }
    }
    pp
}

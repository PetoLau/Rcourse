# remove all variables from memory and garbage collection
# rm(list=ls())
# gc()

# load visualization libraries
library(ggplot2)
library(dygraphs)
library(plotly)
library(animation)

# difference between simple and advanced one
# time series example
# basic one
data_ts <- sin(c(1:100)/5)
plot(1:100, data_ts, type = "l")
plot(ts(data_ts))

# ggplot usage
# data.frame (table) way

df_ts <- data.frame(value = data_ts,
                    time = 1:length(data_ts))

ggplot(df_ts, aes(time, value)) + 
  geom_line() +
  theme_bw()

# dygraphs usage
# must use pipes %>%
dygraph(df_ts[, c("time", "value")]) %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.5,
            strokeWidth = 2) %>%
  dyRangeSelector()

# plotly usage
ggplotly() # calls last plot

# animation in R
saveGIF({
  oopt = ani.options(interval = 0.2, nmax = 50)
  for(i in 1:ani.options("nmax")){
    print(ggplot(df_ts[i:(i+50),], aes(time, value)) + 
            geom_line() +
            theme_bw())
    ani.pause()
  }
}, movie.name = "example.gif", ani.height = 450, ani.width = 650)

# shiny!
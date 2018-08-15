library("ggplot2")
library("readr")
library("dplyr")
library("tibble")
library("tidyr")
library("lubridate")
library("plotly")
library("RColorBrewer")
library("extrafont")
setwd("/Users/andrew/Documents/GitHub/andrewvanleuven.github.io/files/timeline")
df <- read_csv("timeline.csv")

df$start <- mdy(df$start)
df$collect <- mdy(df$collect)
df$draft <- mdy(df$draft)
df$pres <- mdy(df$pres)
df$randr <- mdy(df$randr)
df$accept <- mdy(df$accept)

df <- df[order(-df$number),]

df <- df %>% 
  mutate(earliest_date = pmin(start,collect,draft,pres,randr,accept, na.rm = T),
         latest_date = pmax(start,collect,draft,pres,randr,accept, na.rm = T))

df$paper <- factor(df$paper, levels = df$paper[order(df$latest_date)])

font <- list(size = 14,color = "black")
tfont <- list(size = 30,color = "black")
xax <- list(title = "",showticklabels = TRUE,tickangle = 45,tickfont = font,zeroline = FALSE,showline = FALSE,showgrid = T)
yax <- list(title = "",showticklabels = TRUE,tickfont = font,zeroline = FALSE,showline = FALSE,showgrid = T)
m <- list(l = 50,r = 50,b = 100,t = 100,pad = 4)
bord <- list(color = "black",width = 1.1)
seg <- list(color = "black",width = 3)

timeline <- plot_ly(df, color = I("gray40")) %>%
  add_segments(x = ~earliest_date, xend = ~latest_date, y = ~paper, yend = ~paper, showlegend = FALSE,line =seg) %>%
  add_markers(x = ~start, y = ~paper, name = "Started",marker = list(color = "yellow",size=10,line =bord)) %>%
  add_markers(x = ~collect, y = ~paper, name = "Data Collected",marker = list(color = "blue",size=10,line =bord)) %>%
  add_markers(x = ~draft, y = ~paper, name = "First Draft",marker = list(color = "green",size=10,line =bord)) %>%
  add_markers(x = ~pres, y = ~paper, name = "First Presentation",marker = list(color = "red",size=10,line =bord)) %>%
  add_markers(x = ~randr, y = ~paper, name = "First R&R",marker = list(color = "orange",size=10,line =bord)) %>%
  add_markers(x = ~accept, y = ~paper, name = "Accepted",marker = list(color = "white",size=10,line =bord)) %>%
  layout(title = "<b>Timeline of Research Streams</b>",titlefont = tfont,
         xaxis = xax,yaxis = yax,autosize = F, width = 1600, height = 650,
         margin = m,font = list(family = "Ubuntu",color = "black"))

timeline


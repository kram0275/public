rm(list = ls())

# USER INPUT
start.date <- as.Date("2010-1-08")
end.date <- as.Date("2020-10-11")
stocklist <- c("JPEM", "BBIN", "BBCA", "BBUS", "JAGG", "JCPB", "BBSA")

# Prepare for run
library(quantmod)
library(ggplot2)
library(plotly)
library(data.table)

# Prep list for data frames
list.of.dfs <- list()

# Start loops
for(i in 1:length(stocklist)) {
  temp.df <- as.data.frame(loadSymbols(stocklist[i], auto.assign = FALSE))
  temp.df$date <- row.names(temp.df)
  temp.df <- temp.df[ , c(7, 1:6)]
  temp.df$Stock <- rep(stocklist[i], length(temp.df[ , 1]))
  colnames(temp.df) <- c("Date", "Open", "High", "Low", "Close",
                         "Volume", "Adjusted", "Stock")
  list.of.dfs[[i]] <- temp.df
  rm(temp.df)
}

# Aggregate stocklist data
df <- data.frame(rbindlist(list.of.dfs))
df$Date <- as.Date(df$Date)
df$Stock <- as.factor(df$Stock)
rownames(df) <- seq(1:length(df[ , 1]))

# Start graphing
## Run following as an entire chunk
df.sub <- as.data.frame(df[df$Date >= start.date & df$Date <= end.date, ])
row.names(df.sub) <- NULL

the.plot <- ggplot(aes(x = Date, y = Close, color = Stock), data = df.sub) +
  geom_line(aes(group = Stock), data = df.sub) +
  labs() +
  ggtitle("Closing Prices for Your Selected Portfolio") +
  scale_x_date(date_breaks = "12 month") +
  xlab("Date") +
  ylab("Closing Price [$]") +
  theme_bw()
ggplotly(the.plot, tooltip = (c("y", "x", "group")))
## End Chunk
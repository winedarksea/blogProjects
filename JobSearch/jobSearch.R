setwd("C:/Users/Owner/Documents/R")
library(dplyr)
library(ggplot2)
library(ggthemes)
jobs <- read.csv("JobSearchOutcomes.csv")

## Temporal Distribution

byDate <- jobs %>% group_by(Date) %>% summarise(app_count = sum(App),
                                      reply_count = sum(Reply),
                                      offer_count = sum(Offer))
byDate$replyPercentApp <- byDate$reply_count / byDate$app_count
byDate <- byDate %>% filter(Date != "", app_count != 0)
byDate$Date  <- as.character(byDate$Date)
byDate$Date <- as.Date((byDate$Date), format = "%m/%d/%Y")

byDate$fail <- byDate$app_count - byDate$reply_count
byDate$replySuccess <- byDate$reply_count
fail <- byDate[,c("Date", "fail")] 
fail$group <- "fail"
names(fail) <- c("Date", "number", "group")
replySuccess <- byDate[,c("Date", "replySuccess")]
replySuccess$group <- "replySuccess"
names(replySuccess) <- c("Date", "number", "group")

byDate.graph <- rbind(fail, replySuccess)

ggplot(byDate, aes(x= Date, y = app_count, fill = reply_count)) + geom_bar(stat = "identity")
g1 <- ggplot(byDate.graph, aes(x= Date, y = number, fill = group)) + geom_bar(stat = "identity") 
g1 <- g1 + ylab("Number of Apps") + ggtitle("Apps Across 2018 Job Search") + theme_minimal()
g1 <- g1 + annotate("text", x = as.Date("2018-05-15"), y = 3, label = "Europe Trip")
g1 <- g1 + annotate("text", x = as.Date("2018-02-15"), y = 12, label = "First Search")
g1 <- g1 + annotate("text", x = as.Date("2018-06-15"), y = 12, label = "Second Search")
png(filename = "JobSearchTime.png", width = 750, height = 475, units = "px")
g1
dev.off()

## Geography Distribution

byState <- jobs %>% group_by(State) %>% summarise(app_count = sum(App),
                                                  reply_count = sum(Reply),
                                                  offer_count = sum(Offer))

byState$replyPercentApp <- byState$reply_count / byState$app_count
byState %>% filter(app_count >= 5) %>% arrange(desc(replyPercentApp))



byState$state = byState$State
byState <- byState %>% filter(state != "", !(is.na(state)))

usmap::plot_usmap(data = byState, values = "app_count", lines = "gray") + 
  scale_fill_continuous(
    low = "white", high = "darkblue", name = "Job Apps", label = scales::comma
  ) + theme(legend.position = "right")

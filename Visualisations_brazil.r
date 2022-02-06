#------------------------- Load the (Cleaned) Olist Data -------------------------# 
tmp <- read_csv("Cleaned2.csv")
#------ See customer cities
tmp %>%
  group_by(customer_city) %>%
  count(sort = T) %>%
  head(10)
#------------------------ See customer states------------------------
# Interactive graph
gg <-
  tmp %>% group_by(State) %>% count() %>% arrange(desc(n)) %>%
  ggplot(aes(
    x = fct_reorder(State, n),
    y = n,
    label = n,
    fill = State
  )) + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  coord_flip() + labs(title = "Number of customers in each state") + xlab("Brazilian State") +
  ylab("Number of customers ") + scale_y_continuous(labels = comma)
ggplotly(gg)

# Graph for thesis
# using ggsave
gg <-
  tmp %>% group_by(State) %>% count() %>% arrange(desc(n)) %>%
  ggplot(aes(
    x = fct_reorder(State, n),
    y = n,
    label = n,
    fill = State
  )) + geom_col() + theme(legend.position = "none") +
  coord_flip() + labs(title = "Number of customers in each state") + xlab("Brazilian State") +
  ylab("Number of customers ") + scale_y_continuous(labels = comma)
ggplotly(gg)
ggsave("test.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')

#----------- See product count
tmp$n <- 1
fig <- plot_ly(tmp
               , x = ~product_category, y = ~n,
               color = ~product_category, size = ~reviews_avg_score
) 
fig
#----------- See product categories
# Summarise per product
tmp <- tmp %>% 
  group_by(tmp$State,product_category) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(product_catergory_by_date_per_state = cumsum(n)) 

cat1 <- tmp %>%
  group_by(product_category) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            sd_price = sd(price),
            max_reviews = max(reviews_made_by_date),
            mean_review_score = mean(reviews_avg_score),
            count = n())

# See top product categories           
top1 <- tmp %>% 
  select(State, product_category, product_catergory_by_date_per_state)%>%
  top_n(1, product_catergory_by_date_per_state)


# See product category scores
fig <- plot_ly(
  cat1, x = ~product_category, y = ~mean_review_score,
  color = ~product_category
)

fig
#----------- See states summary
States <- tmp %>%
  group_by(State) %>%
  summarise(max_reviews = max(reviews_made_by_date),
            mean_reviews = mean(rev_score),
            mean_review_made = mean(review_made),
            mean_price = mean(price),
            median_price = median(price),
            sd_price = sd(price),
            mean_gdp = mean(gdp_city),
            median_gdp = median(gdp_city),
            sd_gdp = sd(gdp_city),
            count = n())
regions <- distinct(tmp[,c(26,27)])
States <- merge(States, regions)

States$countperc <- 0
summing <- sum(States$count)
States$countperc <- States$count/summing
#-------- See states GDP
fig <- plot_ly(
  States, x = ~State, y = ~mean_gdp,
  color = ~State
)

fig
fig <- fig %>% layout(title = 'Relationship between within state city mean GDP and review making',
                      yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))
fig
# ---------- See regional data
States$Regions <-States$Region
States$Regions[States$Regions == "Center-west"] <- "CW"
States$Regions[States$Regions == "North"] <- "N"
States$Regions[States$Regions == "Northeast"] <- "NE"
States$Regions[States$Regions == "South"] <- "S"
States$Regions[States$Regions == "Southeast"] <- "SE"

# Boxplots GDP all
A.plot <- plot_ly(data = dplyr::filter(tmp, Region == "Center-west"),x =~ State, y =~ gdp_city, type = "box", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "Center-west", zeroline = F), yaxis = list(title = "GDP", zeroline = F))

B.plot <- plot_ly(data = dplyr::filter(tmp, Region == "North"),x =~ State, y =~ gdp_city, type = "box", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "North", zeroline = F), yaxis = list(title = "GDP", zeroline = F))

C.plot <- plot_ly(data = dplyr::filter(tmp, Region == "Northeast"),x =~ State, y =~ gdp_city, type = "box", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "Northeast", zeroline = F), yaxis = list(title = "GDP", zeroline = F))

D.plot <- plot_ly(data = dplyr::filter(tmp, Region == "South"),x =~ State, y =~ gdp_city, type = "box", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "South", zeroline = F), yaxis = list(title = "GDP", zeroline = F))

E.plot <- plot_ly(data = dplyr::filter(tmp, Region == "Southeast"),x =~ State, y =~ gdp_city, type = "box", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "Southeast", zeroline = F), yaxis = list(title = "GDP", zeroline = F))

subplot(list(A.plot, B.plot, C.plot, D.plot, E.plot), shareY = T, shareX = F, titleX = T, titleY = T)%>% layout(title = 'State wide GDP distribution',
                                                                                                                yaxis = list(zeroline = FALSE),
                                                                                                                xaxis = list(zeroline = FALSE))
# Bar plot GDP mean
A.plot <- plot_ly(data = dplyr::filter(States, Region == "Center-west"),x =~ State, y =~ mean_gdp, type = "bar", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "Center-west", zeroline = F), yaxis = list(title = "Mean GDP", zeroline = F))

B.plot <- plot_ly(data = dplyr::filter(States, Region == "North"),x =~ State, y =~ mean_gdp, type = "bar", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "North", zeroline = F), yaxis = list(title = "Mean GDP", zeroline = F))

C.plot <- plot_ly(data = dplyr::filter(States, Region == "Northeast"),x =~ State, y =~ mean_gdp, type = "bar", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "Northeast", zeroline = F), yaxis = list(title = "Mean GDP", zeroline = F))

D.plot <- plot_ly(data = dplyr::filter(States, Region == "South"),x =~ State, y =~ mean_gdp, type = "bar", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "South", zeroline = F), yaxis = list(title = "Mean GDP", zeroline = F))

E.plot <- plot_ly(data = dplyr::filter(States, Region == "Southeast"),x =~ State, y =~ mean_gdp, type = "bar", showlegend = F, color =~ State) %>%
  layout(xaxis = list(title = "Southeast", zeroline = F), yaxis = list(title = "Mean GDP", zeroline = F))

subplot(list(A.plot, B.plot, C.plot, D.plot, E.plot), shareY = T, shareX = F, titleX = T, titleY = T)%>% layout(title = 'State mean GDP distribution')


Paretto <- States %>% 
  select(State, count, countperc)

Paretto <-Paretto%>%
  dplyr::select(State, count, countperc)%>%
  arrange(desc(countperc, .by_group = FALSE)) %>%
  mutate(cumperc = cumsum(countperc)) 
Paretto90 <-Paretto[Paretto$cumperc < 0.9,]
Paretto80 <-Paretto[Paretto$cumperc < 0.8,]

x <- Paretto90$State
y <- Paretto90$count
p <- ggplot(Paretto90, aes(x=State, y=count)) +
  stat_pareto(point.color = "lightblue3",
              point.size = 1,
              line.color = "grey23",
              size.line = 0.5,
              bars.fill = c("plum3", "lightseagreen", "green", "dodger blue"),
              line
  )

ggplotly(p)

mean(tmp$gdp_city)
median(tmp$gdp_city)
sd(tmp$gdp_city)
mean(tmp$price)
median(tmp$price)
sd(tmp$price)
tmp$hours <- tmp$date
tmp$hours <- format(as.POSIXct(tmp$hours), format = "%H")
tmp$hours <- as.numeric(tmp$hours)
tmp$Time <- tmp$hours
tmp$Time[tmp$hours > 21] <- "night"
tmp$Time[tmp$hours < 6] <- "night"
tmp$Time[tmp$hours >= 6 & tmp$hours < 12] <- "morning"
tmp$Time[tmp$hours >= 12 & tmp$hours < 18] <- "afternoon"
tmp$Time[tmp$hours >= 18 & tmp$hours < 22] <- "evening"
((nrow(tmp[which(tmp$Time=="night"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$Time=="morning"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$Time=="afternoon"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$Time=="evening"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$review_made=="1"),]))/(nrow(tmp))) *100

#---------- Reviewing states
tmp$rev_score <- as.numeric(tmp$rev_score )
fig <- plot_ly(States
               , x = ~States$State, y = States$count, type = 'bar',
               color = ~States$mean_reviews)
fig
tmp$rev_score <- as.numeric(tmp$rev_score)
mean(tmp$rev_score)
hist(tmp$rev_score)
fig <- plot_ly(States
               , x = ~States$State, y = States$mean_review_made, type = 'bar',
               color = ~States$mean_gdp)
fig


Cities <- tmp %>%
  group_by(customer_city) %>%
  summarise(max_reviews = max(reviews_made_by_date),
            mean_reviews = mean(reviews_avg_score),
            mean_price = mean(price),
            median_price = median(price),
            sd_price = sd(price),
            gdp = mean(gdp_city),
            gdp_price = (mean(gdp_city)/mean(price)),
            count = n())
mean(tmp$review_made)

#---------- Review score

((nrow(tmp[which(tmp$rev_score=="1"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$rev_score=="2"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$rev_score=="3"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$rev_score=="4"),]))/(nrow(tmp))) *100
((nrow(tmp[which(tmp$rev_score=="5"),]))/(nrow(tmp))) *100

#---------- Product category

gg <-
  tmp %>% group_by(product_category) %>% count() %>% arrange(desc(n)) %>%
  ggplot(aes(
    x = fct_reorder(product_category, n),
    y = n,
    label = n,
    fill = product_category
  )) + geom_col() + theme(legend.position = "none") +
  coord_flip() + labs(title = "Product category purchase sahare") + xlab("Product category") +
  ylab("Number of orders ") + scale_y_continuous(labels = comma)
ggplotly(gg)


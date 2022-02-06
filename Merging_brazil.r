setwd("~/Documents/UNI/MADS/Thesis/Data/Olist datasets")

#------------------------- Load the Olist Data -------------------------# 
prod_translat <- read_csv("product_category_name_translation.csv")
products <- read_csv("olist_products_dataset.csv")
reviews <- read_csv("olist_order_reviews_dataset.csv")
reviews_freq <- reviews %>%
  group_by(order_id) %>% 
  filter(n()>1)
reviews$review_made <- 0
reviews$review_made[reviews$review_comment_title >0 | reviews$review_comment_message > 0] <- 1
reviews <- reviews %>%
  group_by(order_id) %>%
  mutate(reviews_in_order = cumsum(review_made),
            rev_mean_score = mean(review_score))%>%
  ungroup()
orders <- read_csv("olist_orders_dataset.csv")
DT <- data.table(orders)
 DT<- as.data.frame(DT[, .N, by=.(customer_id)]) # we see no duplicated customer ids
order_items <- read_csv("olist_order_items_dataset.csv")
DT <- data.table(order_items)
DT<- as.data.frame(DT[, .N, by=.(order_id)]) # we see duplicates
order_items$n <- 1
temp <- order_items %>%
     group_by(order_id, product_id) %>%
     mutate(n_prod_cat_bought = sum(n),
            price_subtotal = sum(price), .groups = 'drop')%>%
     ungroup()
temp <- temp %>%
  group_by(order_id) %>%
  mutate(items_in_order = sum(n), .groups = 'drop')%>%
  ungroup()
temp <- temp %>%
  group_by(product_id, order_id) %>%
  mutate(prodcat_in_order = sum(n), .groups = 'drop')%>%
  ungroup()
order_items  <- unique(temp[ , c(1,3:5,6,9:10,12,13)])
order_paym <- read_csv("olist_order_payments_dataset.csv")
order_paym <-order_paym %>%
  group_by(order_id) %>%
  arrange(payment_type, .by_group = TRUE) %>%
  mutate(paym_sequential = max(payment_sequential),
         paym_installments =max(payment_installments),
         paym_value =sum(payment_value),
         payment_type = paste0(payment_type, collapse = " & "))%>%
  ungroup()  
order_paym <-order_paym[,c(1,3,6:8)]
order_paym <-unique(order_paym)
DT <- data.table(order_paym)
DT<- as.data.frame(DT[, .N, by=.(payment_type)]) # no more duplicates

a <- order_paym %>% 
  filter(across(payment_type, ~ grepl('debit_card & debit_card', .))) %>%
  mutate(payment_type = 'debit_card')
order_paym <- order_paym[!(order_paym$order_id %in% a$order_id),] 
order_paym <- rbind(order_paym,a)
a <- order_paym %>% 
  filter(across(payment_type, ~ grepl('credit_card & credit_card & voucher', .))) %>%
  mutate(payment_type = 'credit_card & voucher')
order_paym <- order_paym[!(order_paym$order_id %in% a$order_id),] 
order_paym <- rbind(order_paym,a)
a <- order_paym %>% 
  filter(across(payment_type, ~ grepl('credit_card & voucher', .))) %>%
  mutate(payment_type = 'credit_card & voucher')
order_paym <- order_paym[!(order_paym$order_id %in% a$order_id),] 
order_paym <- rbind(order_paym,a)
a <- order_paym %>% 
  filter(across(payment_type, ~ grepl('voucher & voucher', .))) %>%
  mutate(payment_type = 'voucher')
order_paym <- order_paym[!(order_paym$order_id %in% a$order_id),] 
order_paym <- rbind(order_paym,a)
a <- order_paym %>% 
  filter(across(payment_type, ~ grepl('debit_card & credit_card', .))) %>%
  mutate(payment_type = 'credit_card & debit_card')
order_paym <- order_paym[!(order_paym$order_id %in% a$order_id),] 
order_paym <- rbind(order_paym,a)
a <- order_paym %>% 
  filter(across(payment_type, ~ grepl('credit_card & credit_card', .))) %>%
  mutate(payment_type = 'credit_card')
order_paym <- order_paym[!(order_paym$order_id %in% a$order_id),] 
order_paym <- rbind(order_paym,a)
DT <- data.table(order_paym)
DT<- as.data.frame(DT[, .N, by=.(payment_type)]) # no more duplicates

geoloc <- read_csv("olist_geolocation_dataset.csv")
cust <- read_csv("olist_customers_dataset.csv")
DT <- data.table(cust)
DT<- as.data.frame(DT[, .N, by=.(customer_id)]) # we see no duplicated customer ids
seller <- read_csv("olist_sellers_dataset.csv")
#------------------------- Import gdp per year -------------------------# 
# merge geolocations
#City level GDPs
gdp_city <- read_excel("~/Documents/Uni/MADS/Thesis/Data/Olist datasets/data_city.xlsx")
names(gdp_city)[1]<-paste("customer_city")
names(gdp_city)[2]<-paste("customer_state")
names(gdp_city)[3]<-paste("gdp_city")

gdp_city$customer_city <- gsub('é','e', gdp_city$customer_city)
gdp_city$customer_city <- gsub('á','a', gdp_city$customer_city)
gdp_city$customer_city <- gsub('í','i', gdp_city$customer_city)
gdp_city$customer_city <- gsub('â','a', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ç','c', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ú','u', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ó','o', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ú','u', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ã','a', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ô','o', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ã','a', gdp_city$customer_city)
gdp_city$customer_city <- gsub('ê','e', gdp_city$customer_city)

geoloc$geolocation_city <- gsub('é','e', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('á','a', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('í','i', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('â','a', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ç','c', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ú','u', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ó','o', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ú','u', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ã','a', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ô','o', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ã','a', geoloc$geolocation_city)
geoloc$geolocation_city <- gsub('ê','e', geoloc$geolocation_city)

# Make zip code dataset
agg = aggregate(geoloc,
                by = list(geoloc$geolocation_zip_code_prefix),
                FUN = mean)
Geo <- agg[,c(1,3,4)]

#seller
geosell <- merge(Geo, seller,
                 by.x = "Group.1", 
                 by.y = "seller_zip_code_prefix", all = TRUE)
geosell <- geosell[,-c(5:6)]
names(geosell)[1]<-paste("seller_zip_code")
#customer
geocust <- merge(Geo, cust,
                 by.x = "Group.1", 
                 by.y = "customer_zip_code_prefix", all = TRUE)
geocust <- geocust[,-c(5:6)]
names(geocust)[1]<-paste("customer_zip_code_prefix")

geosell %>% # replace to your needs
  summarise_all(funs(sum(is.na(.))))
geocust %>% # replace to your needs
  summarise_all(funs(sum(is.na(.))))

geocust %>% # replace to your needs
  summarise_all(funs(sum(is.na(.))))
geocust[is.na(geocust$customer_id),]
geocust <- geocust[!is.na(geocust$customer_id),]

# In thesis a better name was given "customer_orders" 
cust_orders <- merge(cust, orders,by.x = "customer_id", 
                     by.y = "customer_id", all = TRUE)

cust_orders_items <- inner_join(cust_orders, order_items,
  by = c( "order_id"), copy = FALSE,suffix = c(".x", ".y"),
  keep = FALSE, na_matches = c("na", "never")
)

Geoloc <- inner_join(cust_orders_items,
  geosell,by = c( "seller_id"),
  copy = FALSE, suffix = c(".x", ".y"),
  keep = FALSE,na_matches = c("na", "never"))

Geoloc <- Geoloc %>% rename(seller_lat=geolocation_lat, seller_lng=geolocation_lng)

Geoloc_all <- inner_join(Geoloc,geocust,
  by = c("customer_id", "customer_zip_code_prefix", "customer_state"),
  copy = FALSE,keep = FALSE, na_matches = c("na", "never"))

Cust_seller_orders <- inner_join(
  Geoloc_all,gdp_city,  by = c("customer_city", "customer_state"),
  copy = FALSE, keep = FALSE,  na_matches = c("na", "never"))

Cust_seller_orders %>% summarise_all(funs(sum(is.na(.))))
Cust_seller_orders <- Cust_seller_orders[!is.na(Cust_seller_orders$order_delivered_customer_date),]

trans_prod <- merge(products,prod_translat, by.x = "product_category_name", 
                    by.y = "product_category_name", all.x = TRUE)

Cust_seller_orders_prod <- inner_join(
  Cust_seller_orders, trans_prod,by = c("product_id"), copy = FALSE,
  keep = FALSE,na_matches = c("na", "never"))

Cust_seller_orders_prod_rev <- left_join(
  Cust_seller_orders_prod, reviews,by = c("order_id"), copy = FALSE,
  keep = FALSE,na_matches = c("na", "never"))

Cust_seller_orders_prod_rev <- Cust_seller_orders_prod_rev[Cust_seller_orders_prod_rev$order_status == "delivered",]
Cust_seller_orders_prod_rev %>% summarise_all(funs(sum(is.na(.))))

Cust_seller_orders_prod_paym <- inner_join(
  Cust_seller_orders_prod_rev, order_paym,
  by = c( "order_id"),
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = FALSE,
  na_matches = c("na", "never")
)

Cust_seller_orders_prod_paym %>% summarise_all(funs(sum(is.na(.))))
1398/nrow(Cust_seller_orders_prod_paym) # 1.41% missing. Let's delete all the missing prod categories
Cust_seller_orders_prod_paym <- Cust_seller_orders_prod_paym[!is.na(Cust_seller_orders_prod_paym$product_category_name),]
Cust_seller_orders_prod_paym %>% summarise_all(funs(sum(is.na(.))))
Cust_seller_orders_prod_paym <- Cust_seller_orders_prod_paym[,-c(38,39)]

Cust_seller_orders_prod_paym <- Cust_seller_orders_prod_paym %>%
  na.omit()

Cust_seller_orders_prod_paym$dist_diff <- distHaversine(cbind(Cust_seller_orders_prod_paym$seller_lat, Cust_seller_orders_prod_paym$seller_lng), cbind(Cust_seller_orders_prod_paym$geolocation_lat, Cust_seller_orders_prod_paym$geolocation_lng))
#----------------------- Create year variable

Cust_seller_orders_prod_paym$Year <- Cust_seller_orders_prod_paym$order_purchase_timestamp
Cust_seller_orders_prod_paym$Year <- format(as.Date(Cust_seller_orders_prod_paym$Year, format="%d/%m/%Y"),"%Y")

#----------------------- Create order on-time variable variable

Cust_seller_orders_prod_paym$time_dev <- 0
Cust_seller_orders_prod_paym$order_delivered_customer_date <- format(as.Date(Cust_seller_orders_prod_paym$order_delivered_customer_date,format="%d/%m/%Y"))
Cust_seller_orders_prod_paym$order_delivered_customer_date <- as.Date(Cust_seller_orders_prod_paym$order_delivered_customer_date)
Cust_seller_orders_prod_paym$order_estimated_delivery_date <- as.Date(Cust_seller_orders_prod_paym$order_estimated_delivery_date)
Cust_seller_orders_prod_paym$time_dev <- Cust_seller_orders_prod_paym$order_delivered_customer_date - Cust_seller_orders_prod_paym$order_estimated_delivery_date 

#----------------------- Add State
# Merge State
states_names <- read_csv("states.csv") # This is used for geographical information, as this is data from 2019. 
#Thus state abbrviations, maybe density and number of cities are used
states_names <- states_names[,c(1:4)] 
df <- merge(Cust_seller_orders_prod_paym, states_names, 
                                      by.x='customer_state', by.y='UF', 
                                      Header = TRUE, label = TRUE)
#------------------------- Aggregate  -------------------------#
# H1 is at the product level so no need have it aggregated
# H2, H3, H4 is at order level, so it must be aggregated
df$unique <- 1
df$unique[duplicated(df$order_id)] <- 0
df <- df[df$order_status == "delivered",]
df <- df[df$paym_value > 0,]
df <- df[df$product_weight_g > 0,]
df <- df[df$product_height_cm > 0,]
df <- df[df$product_width_cm > 0,]
df <- df[df$product_name_lenght > 0,]
df <- df[df$product_name_lenght > 0,]
df <-  df[-which(df$review_made == 1 & df$order_purchase_timestamp > df$review_creation_date),]
df <- df[-which(df$order_purchase_timestamp > df$order_delivered_customer_date),]

# Let's get make a review count up to date variable
df %>% 
  dplyr::select(product_id, order_purchase_timestamp, review_made)%>% 
  group_by(product_id) %>%
  arrange(order_purchase_timestamp, .by_group = TRUE) %>%
  mutate(reviews_made_by_date = cumsum(review_made)) 

orders_to_date <- df %>% 
  dplyr::select(product_id, review_creation_date, review_made) %>% 
  group_by(product_id) %>%
  arrange(review_creation_date, .by_group = TRUE) %>%
  mutate(reviews_made_by_date = cumsum(review_made)) %>%
  rename(order_purchase_timestamp = review_creation_date)

tmp<-df %>%
  mutate(reviews_made_by_date = NA_real_) %>%
  union_all(ungroup(orders_to_date)) %>%
  group_by(product_id) %>%
  arrange(order_purchase_timestamp, .by_group = TRUE) %>%
  fill(reviews_made_by_date, .direction="up") %>%
  drop_na(customer_id)

# create rating average
tmp <- tmp %>% 
  group_by(product_id) %>%
  arrange(review_creation_date, .by_group = TRUE) %>%
  mutate(reviews_avg_score = cummean(review_score))

# create price index
tmp <- tmp %>% 
  group_by(product_id) %>%
  arrange(order_purchase_timestamp, .by_group = TRUE) %>%
  mutate(price_mean_to_date = cummean(price)) %>%
  mutate(price_index = price/price_mean_to_date)

tmp <- distinct(tmp, customer_id, order_id, seller_id, review_id, .keep_all = TRUE)
#--------------Find out how many rows kept after merging
# Geoloc
str(agg)
str(df)
which( colnames(df)=="geolocation_lat" )
names(agg)[1]<-paste("customer_zip_code_prefix")
common <- generics::intersect(df[,c(4,24,25)], agg[,c(1,3,4)]) 
nrow(common)/nrow(geoloc) #1.4%
names(agg)[1]<-paste("seller_zip_code")
names(agg)[3]<-paste("seller_lat")
names(agg)[4]<-paste("seller_lng")
which( colnames(df)=="seller_zip_code" )
common1 <- generics::intersect(df[,c(21,22,23)], agg[,c(1,3,4)]) 
names(common1)[1]<-paste("1")
names(common1)[2]<-paste("2")
names(common1)[3]<-paste("3")
names(common)[1]<-paste("1")
names(common)[2]<-paste("2")
names(common)[3]<-paste("3")
common <- rbind(common,common1)
common <- unique(common)
nrow(common)/nrow(geoloc) #1.4%
# Sellers
str(seller)
str(df)
which( colnames(df)=="seller_id" )
seller <- seller %>%
  rename(seller_zip_code=seller_zip_code_prefix)
common <- generics::intersect(df[,c(14,21)], seller[,c(1:2)]) 
nrow(common)/(nrow(seller)) # 93.6% kept
# customers
str(cust)
str(df)
which( colnames(df)=="customer_zip_code_prefix" )
common <- generics::intersect(df[,c(1,2,3,4,5)], cust[,c(1,2,3,4,5)])
(nrow(common))/(nrow(cust)) # 94%
# order items
str(order_items)
str(df)
which( colnames(tmp)=="price" )
common <- generics::intersect(tmp[,c(6,13,14,15,16)], order_items[,c(1:5)])
(nrow(common))/(nrow(order_items)) # 87.7%
# order_reviews
str(reviews)
str(tmp)
which( colnames(tmp)=="review_answer_timestamp" )
common <- generics::intersect(tmp[,c(2,25,26,27,28)], reviews[,c(1,2,3,6,7)])
(nrow(common))/(nrow(reviews)) # 95.6%
# orders
str(orders)
str(tmp) 
which( colnames(tmp)=="order_approved_at" )
common <- generics::intersect(tmp[,c(2,18,19,20,21,22,24)], orders[,c(1:6,8)])
(nrow(common))/(nrow(reviews)) # 95.1%
# products
str(products)
str(tmp) 
which( colnames(tmp)=="product_width_cm")
common <- generics::intersect(tmp[,c(3:11)], products[,c(1:9)])
(nrow(common))/(nrow(products)) # 95.9%
# order payments
str(order_paym)
str(tmp) 
which( colnames(tmp)=="payment_value")
common <- generics::intersect(tmp[,c(2,29:32)], order_paym[,c(1:5)])
(nrow(common))/(nrow(order_paym)) # 91.6%
# ----------------
str(tmp)
tmp$date <- tmp$order_purchase_timestamp
tmp <- tmp %>% 
  rename(
    product_category = product_category_name_english,
    rev_score = review_score)

tmp$late <- "on time"
tmp$late[tmp$time_dev > 0] <- "late"
tmp$wealth <- "poor"
quantile(tmp$gdp_city, probs = seq(0, 1, 1/5))  
median(tmp$gdp_city) # 26.8 50%
#    0%    20%    40%    60%    80%   100% 
#  3.04  15.00  23.32  30.83  37.89 208.90 
tmp$wealth[tmp$gdp_city <= 15 ] <- "poor"
tmp$wealth[tmp$gdp_city %between% c(15,26.8) ] <- "lower middle class"
tmp$wealth[tmp$gdp_city %between% c(26.81,37.89) ] <- "upper middle class"
tmp$wealth[tmp$gdp_city >= 37.89 ] <- "wealthy"

write.csv(tmp,"Full_dataset.csv", row.names = FALSE)
tmp <- tmp[,-c(1,3,7,8,9,10,11,12)] # removing shipping date type of variable
tmp <- tmp[,-c(13:17)]#removing review type variables
tmp <- tmp[,-c(7,14,15,18:21)]#removing seller/order type variables
tmp <- tmp[,-c(31,33)]#removing id type variables
tmp <- tmp[,-c(2,16)]#removing id type variables
tmp <- tmp[,-c(16,17)]#removing id type variables

write.csv(tmp,"Cleaned2.csv", row.names = FALSE)

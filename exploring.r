#####
#### Data Prep
library(tidyr)
library(dplyr)
library(ggmap)
super_chipotle = read.csv("/Users/jakebialer/chipotle_project/allchipotledatawithdisclaimer.csv", stringsAsFactors = FALSE)
ncol(super_chipotle)
# 19 
super_chipotle = super_chipotle[,-1]
ncol(super_chipotle)
# 18
# Filter out duplicate rows
nrow(super_chipotle)
# 190483
sort(table(super_chipotle$menu_items))

super_chipotle=unique(super_chipotle)
nrow(super_chipotle)
#186131

free_super_chipotle = super_chipotle[super_chipotle$item_price==0,]
sort(table(free_super_chipotle$menu_items))
super_chipotle = super_chipotle[super_chipotle$item_price!=0,]

# Fix Extra Items In Tacos 
is_extra=function(x){
  return(grepl("Tacos",x$menu_items)>0&x$max_count==1)
}

super_chipotle$menu_items[is_extra(super_chipotle)] = paste(super_chipotle$menu_items[is_extra(super_chipotle)], "Extra")

# Fix Remove Guac Min

# Filter max guac 

top_guac=super_chipotle %>% group_by(num,menu_items) %>% filter(menu_items=="Guacamole" & item_price== max(item_price))
no_guac=super_chipotle  %>% filter(menu_items!="Guacamole")
super_chipotle = rbind(as.data.frame(top_guac),as.data.frame(no_guac))


table(super_chipotle$menu_items)
### Canada Items 


table(super_chipotle$menu_items)[table(super_chipotle$menu_items)==17]


super_chipotle %>% filter(country=="CA")
super_chipotle %>% filter(menu_items %in% c("Aranciata Rossa (Blood Orange)","Aranciata (Orange)","Lemonade","Limonata","Orange Mango","Pompelmo (Grapefruit)","Strawberry")) %>% group_by(country) %>% summarise(n())

### Soda in France and One US # Cambride 
super_chipotle %>% filter(menu_items %in% c("Soda")) %>% group_by(country) %>% summarise(n())

### Orange Juice  for $4 at Dulles  Most at any chipotle
filter(super_chipotle,menu_items=="Orange Juice")

# Tonic Water in the UK 
super_chipotle %>% filter(menu_items %in% c("Juice/Tonic Water")) %>% group_by(country) %>% summarise(n())

# Nantucket in US 
super_chipotle %>% filter(grepl("Nantucket",menu_items)) %>% group_by(country) %>% summarise(n())

super_chipotle %>% filter(country=="US") %>% group_by(num) %>% summarise(n()) %>% arrange(-`n()`)

# Cal Colorado, No Nantucket
super_chipotle %>% filter(country=="US") %>%
  group_by(num) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>%
  filter(count==54) %>% 
  inner_join(super_chipotle,by="num") %>% 
  select(num,city,state,country) %>% unique() %>% 
  group_by(state) %>% summarise(n())

# Colorado, MA 
super_chipotle %>% filter(country=="US") %>%
  group_by(num) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>%
  filter(count<54) %>% 
  inner_join(super_chipotle,by="num") %>% 
  select(num,city,state,country) %>% unique() %>% 
  group_by(state) %>% summarise(n())

super_chipotle %>% filter(country=="US") %>%
  group_by(num) %>% 
  summarise(count=n()) %>% 
  arrange(-count) %>%
  filter(count<54) %>% 
  inner_join(super_chipotle,by="num") %>% 
  select(num,city,state,country) %>% unique()


super_chipotle[super_chipotle$num==54,]
# No Nantucket
super_chipotle[super_chipotle$num==15,]$menu_items[!super_chipotle[super_chipotle$num==15,]$menu_items %in% super_chipotle[super_chipotle$num==2600,]$menu_items]

# No Nantucket and No Izze
super_chipotle[super_chipotle$num==15,]$menu_items[!super_chipotle[super_chipotle$num==15,]$menu_items %in% super_chipotle[super_chipotle$num==54,]$menu_items]

# No Nantucket, No Izze, or Bottled Water 
super_chipotle[super_chipotle$num==15,]$menu_items[!super_chipotle[super_chipotle$num==15,]$menu_items %in% super_chipotle[super_chipotle$num==1970,]$menu_items]


filter(super_chipotle, num==1271)
# Dulles
       
filter(super_chipotle, num==15)

## 
price_table=as.data.frame.matrix(table(super_chipotle$menu_items, super_chipotle$item_price))

apply(price_table,1,function(x) sum(x!=0))

super_chipotle %>% filter(menu_items=="Small Soda") %>% group_by(state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()
# San Francisco Soda Tax -> Leads to most expensive 
super_chipotle %>% filter(menu_items=="Small Soda" & item_price=="2.15") %>% group_by(state,item_price,city) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()

# NY most expensive guac 
super_chipotle %>% filter(menu_items=="Guacamole") %>% group_by(country,state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()
# California most expensivve Barbacoa 
super_chipotle %>% filter(menu_items=="Extra Barbacoa") %>% group_by(country,state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()
# California most expensive Steak 
super_chipotle %>% filter(menu_items=="Extra Steak") %>% group_by(country,state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()
# NY most expensive chicken
super_chipotle %>% filter(menu_items=="Extra Chicken") %>% group_by(country,state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()
# Ny most expensive sofritas
super_chipotle %>% filter(menu_items=="Extra Sofritas") %>% group_by(country,state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()
# Ny most expensive carnitas
super_chipotle %>% filter(menu_items=="Extra Carnitas") %>% group_by(country,state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()
# NY most expensive chorizo 
super_chipotle %>% filter(menu_items=="Extra Chorizo") %>% group_by(country,state,item_price) %>% summarise(count=n()) %>% arrange(-item_price) %>% as.data.frame()



# 
super_chipotle %>% filter(city=="Billings") 
# Guac 1.95, Small Soda 1.80

unique(super_chipotle$hours)
# "Mon-Sun: 5:00 AM - 10:00 PM"  
# Baltimore Airport 

# "Mon-Thu, Sun: 10:00 AM - 11:59 PM\nFri-Sat: 10:00 AM - 1:00 AM"     
super_chipotle[super_chipotle$hours=="Mon-Thu, Sun: 10:00 AM - 11:59 PM\nFri-Sat: 10:00 AM - 1:00 AM" ,]
# Las Vegas 

super_burritos  =  super_chipotle[grepl("Burrito",super_chipotle$menu_items) & grepl("Bowl",super_chipotle$menu_items)==FALSE,]
super_burritos = filter(super_burritos, menu_items %in% c("Steak Burrito","Carnitas Burrito","Chicken Burrito"))
us = get_map(location='united states', zoom=4)
ggmap(us, base_layer = ggplot(super_burritos %>% filter(country=="US"), aes(x=long,y=lat))) + geom_point(alpha=.3,aes(color=item_price)) + facet_grid(menu_items~.)

super_burritos$menu_items=as.factor(super_burritos$menu_items)
levels(super_burritos$menu_items) = c("Carnitas/Chorizo","Chicken/Veggie/Sofritas","Steak/Barbacoa")
super_burritos %>% 
  filter(country=="US") %>% 
  ggplot(aes(x=item_price)) + geom_bar() + facet_grid(menu_items~.)
us_super_burritos = super_burritos %>% 
  filter(country=="US")
burrito_price_prop= as.matrix.data.frame(prop.table(table(us_super_burritos$item_price,us_super_burritos$menu_items),margin=2))
burrito_price_prop = as.data.frame(burrito_price_prop)
burrito_price_prop$prices = row.names(prop.table(table(us_super_burritos$item_price,us_super_burritos$menu_items),margin=2))
names(burrito_price_prop)[1:3] = c("Carnitas/Chorizo","Chicken/Veggie/Sofritas","Steak/Barbacoa")
burrito_price_prop[,1:3] = lapply(burrito_price_prop[,1:3],function(x) round(x*100))
# Analyze hours 
super_chipotle %>% 
  select(num,hours, country, state) %>% 
  unique() %>% 
  group_by(hours,state) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

super_chipotle %>% 
  select(num,hours, country, state) %>% 
  unique() %>% 
  filter(grepl("Closed",hours)) %>% 
  group_by(hours,state) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

super_chipotle %>% 
  select(num,hours, country, state) %>% 
  unique() %>% 
  filter(grepl("Closed",hours)) %>% 
  group_by(state) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
# disclaimers = read.csv("/Users/jakebialer/chipotle_project/allchipotledatawithdisclaimer.csv", stringsAsFactors = FALSE)
# burrito = disclaimers[grepl("Burrito",disclaimers$menu_items) & grepl("Bowl",disclaimers$menu_items)==FALSE,]
# burrito =select(burrito,num, address,city,state,country,zipcode,lat,long,hours,item_price,menu_items,message,title)

super_burrito=spread(burrito,key=menu_items,value=item_price)
sales_tax = read.csv("/Users/jakebialer/sales_tax_data1.csv")

names(sales_tax)[2] = "zipcode"
yelp_data = read.csv("/Users/jakebialer/yelpwithmatches.csv")
super_burrito=left_join(super_burrito,yelp_data, by="num")
super_burrito = left_join(super_burrito,sales_tax)

round_down = function(x){
  floor(x * 100) / 100
  
}

super_burrito$`Steak Burrito_aftertax` = round_down(super_burrito$`Steak Burrito`* (super_burrito$rate/100+1))
super_burrito$`Chicken Burrito_aftertax` = round_down(super_burrito$`Chicken Burrito`* (super_burrito$rate/100+1))
super_burrito$`Carnitas Burrito_aftertax` = round_down(super_burrito$`Carnitas Burrito`* (super_burrito$rate/100+1))
super_burrito$`Barbacoa Burrito_aftertax` = round_down(super_burrito$`Barbacoa Burrito`* (super_burrito$rate/100+1))

super_burrito$`Chorizo Burrito_aftertax` = round_down(super_burrito$`Chorizo Burrito`* (super_burrito$rate/100+1))
super_burrito$`Sofritas Burrito_aftertax` = round_down(super_burrito$`Steak Burrito`* (super_burrito$rate/100+1))
super_burrito$`Veggie Burrito_aftertax` = round_down(super_burrito$`Veggie Burrito`* (super_burrito$rate/100+1))







### Steak To Chicken Burrito Ratio 
# Three prices Steak|Barabaco -> |Carnitas|Chorizo| -> Sofritas|Veggie|Chicken


### Veggie Burrito the same as chicken
table(super_burrito$`Chicken Burrito`==super_burrito$`Veggie Burrito`)
### Sofritas Burrito the same as chicken
table(super_burrito$`Chicken Burrito`==super_burrito$`Sofritas Burrito`)
### Steak Burrito == Barbacoa Burrito 
table(super_burrito$`Steak Burrito`==super_burrito$`Barbacoa Burrito`)
### Carnitas == Chorizo Burrito 
table(super_burrito$`Carnitas Burrito`==super_burrito$`Chorizo Burrito`)

ggplot(data=super_burrito, aes(x=`Chicken Burrito`,y=`Steak Burrito`)) + geom_point(aes(color=country)) 

super_burrito$ratio =super_burrito$`Steak Burrito`/super_burrito$`Chicken Burrito`

arrange(super_burrito, -ratio)

arrange(super_burrito, ratio)

table(super_burrito$country,super_burrito$ratio)
# canada france -> 1-1 ratio 
# UK almost 1-1 

# Chicken More Expensive. This follows CPI price. 
super_burrito %>% filter(ratio=="1.10294117647059") %>% group_by(state,city.x) %>% summarise(n())
#  Chicken More Expensive Relative to Chicken in Upstate New York 

super_burrito %>% filter(ratio=="1.125") %>% group_by(state,city.x) %>% summarise(n())

# DC 
super_burrito %>% filter(ratio=="1.13409415121255") %>% group_by(state,city.x) %>% summarise(n())
#MA, NJ

super_burrito %>% filter(ratio=="1.1374269005848") %>% group_by(state,city.x) %>% summarise(n())
# MD 

super_burrito %>% filter(ratio=="1.13793103448276") %>% group_by(state,city.x) %>% summarise(n())
# CT

super_burrito %>% filter(ratio=="1.1437908496732") %>% group_by(state,city.x) %>% summarise(n())
# San Fran


super_burrito %>% filter(ratio=="1.14388489208633") %>% group_by(state,city.x) %>% summarise(n())
# Col, IL, Chicago

super_burrito %>% filter(ratio=="1.14705882352941") %>% group_by(state,city.x) %>% summarise(n())
# CT, Boston

super_burrito %>% filter(ratio=="1.14925373134328") %>% group_by(state,city.x) %>% summarise(n())
# Ca

super_burrito %>% filter(ratio=="1.15384615384615") %>% group_by(state,city.x) %>% summarise(n())
# Everywhere else





library(ggmap)
super_burrito %>% filter(ratio=="1.13409415121255") %>% group_by(state,city.x) %>% summarise(n())
us_map = get_map(location = "United States", zoom = 4)
ggmap(us_map, base_layer=ggplot(data=super_burrito %>% filter(country=="US"),aes(x=long,y=lat)))+ geom_point(alpha=.2,aes(color=ratio)) 


world_map = get_map(location = "World", zoom = 1)

ggmap(world_map, base_layer=ggplot(data=super_burrito,aes(x=long,y=lat)))+ geom_point(alpha=.2,aes(color=ratio)) 

### Steak To Pork Burrito Ratio 

super_burrito$ratio2 =super_burrito$`Steak Burrito`/super_burrito$`Carnitas Burrito`
# Canada, France and UK are all 1-1 prices 

table(super_burrito$country,super_burrito$ratio2)
ggmap(us_map, base_layer=ggplot(data=super_burrito %>% filter(country=="US"),aes(x=long,y=lat)))+ geom_point(alpha=.2,aes(color=ratio2)) 
## Upstate New York, New YOrk


super_burrito %>% filter(ratio2=="1.03448275862069") %>% group_by(state,city.x) %>% summarise(n())

super_burrito %>% filter(ratio2=="1.05269186712486") %>% group_by(state,city.x) %>% summarise(n())

###
lapply(super_burrito, function(x) sum(is.na(x)))

# No Chorizo Overseas
table(super_burrito[is.na(super_burrito$`Chorizo Burrito`),]$country)

# No Sofritas in France or UK, but in Canada
table(super_burrito[is.na(super_burrito$`Sofritas Burrito`),]$country)

#### Geocounts
table(super_burrito$country)
sort(table(super_burrito$state))
# Compare vs state population
sort(table(super_burrito$city.x))
# Compare vs city population


#### Yelp Analyiss 
table(super_burrito$rating)
ggplot(super_burrito, aes(x=rating)) + geom_bar()
ggplot(super_burrito, aes(x=review_count)) + geom_bar()

quantile(super_burrito$review_count, na.rm=T)
quantile(super_burrito$review_count,  probs = c(seq(0,1,by=.1)), na.rm=T)

### Where were there reveiws?

# Ca, NY, TX
super_burrito %>% 
  filter(country=="US" & !is.na(review_count)) %>% 
  group_by(state) %>% 
  summarise(count=sum(review_count)) %>% 
  arrange(-count)

# New York, LA, San Fran
super_burrito %>% 
  filter(country=="US" & !is.na(review_count)) %>% 
  group_by(city.x) %>% 
  summarise(count=sum(review_count)) %>% 
  arrange(-count)


# No Reviews outside US 
super_burrito %>% 
  filter(country!="US" & !is.na(review_count)) %>% 
  group_by(country) %>% 
  summarise(count=sum(review_count)) %>% 
  arrange(-count)


super_burrito %>% 
  filter(country=="US" & !is.na(review_count)) %>% 
  group_by(state) %>% 
  summarise(count=sum(review_count), med=median(review_count), avg=mean(review_count), ma=max(review_count), mi=min(review_count)) %>% 
  arrange(-count)

super_burrito %>% 
  filter(country=="US" & !is.na(review_count)) %>% 
  group_by(state) %>% 
  summarise(count=sum(rating), med=median(rating), avg=mean(rating), ma=max(rating), mi=min(rating)) %>% 
  arrange(-count)

### Best Chipotles By State 
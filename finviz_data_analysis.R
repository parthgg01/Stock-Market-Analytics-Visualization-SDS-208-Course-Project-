rm(list=ls())

library(rvest)
library(httr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)

ticker<-c()
name<-c()
sector<-c()
industry<-c()
country<-c()
mcap<-c()
PE<-c()
price<-c()
change<-c()
volume<-c()

for(r in seq(1,11641,20)){
  
  # all of the url differ only by r value
  
  Sys.sleep(0.5)
  
  #implements a lag of 0.5 seconds so that finviz doesn't flag us
  
  url<-paste0("https://finviz.com/screener?v=111&r=",r)
  html<-read_html(httr::content(httr::GET(url),"text"))
  
  t<-html%>%
    html_elements("tr.styled-row td:nth-child(2)")%>%
    html_text()%>%
    sub("^.","",.)
  ticker<-c(ticker,t)
  
  n<-html%>%
    html_elements("tr.styled-row td:nth-child(3) a")%>%
    html_text(trim=TRUE)
  name<-c(name,n)
  
  s<-html%>%
    html_elements("tr.styled-row td:nth-child(4) a")%>%
    html_text(trim=TRUE)
  sector<-c(sector,s)
  
  i<-html%>%
    html_elements("tr.styled-row td:nth-child(5) a")%>%
    html_text(trim=TRUE)
  industry<-c(industry,i)
  
  ctry<-html%>%
    html_elements("tr.styled-row td:nth-child(6) a")%>%
    html_text(trim=TRUE)
  country<-c(country,ctry)
  
  m<-html%>%
    html_elements("tr.styled-row td:nth-child(7) a")%>%
    html_text(trim=TRUE)
  mcap<-c(mcap,m)
  
  pe<-html%>%
    html_elements("tr.styled-row td:nth-child(8) a")%>%
    html_text(trim=TRUE)
  PE<-c(PE,pe)
  
  p<-html%>%
    html_elements("tr.styled-row td:nth-child(9) a")%>%
    html_text(trim=TRUE)
  price<-c(price,p)
  
  ch<-html%>%
    html_elements("tr.styled-row td:nth-child(10) a")%>%
    html_text(trim=TRUE)
  change<-c(change,ch)
  
  v<-html%>%
    html_elements("tr.styled-row td:nth-child(11) a")%>%
    html_text(trim=TRUE)
  volume<-c(volume,v)
}

mcap<-gsub("B","e9",mcap)
mcap<-gsub("M","e6",mcap)
mcap<-gsub("K","e3",mcap)
mcap[mcap=="-"]<-NA
mcap<-as.numeric(mcap)

PE[PE=="-"]<-NA
PE<-as.numeric(PE)

price[price=="-"]<-NA
price<-as.numeric(price)

change<-gsub("%","",change)
change[change=="-"]<-NA
change<-as.numeric(change)

volume<-gsub(",","",volume)
volume[volume=="-"]<-NA
volume<-as.numeric(volume)

data<-data.frame(
  Ticker=ticker,
  Company=name,
  Sector=sector,
  Industry=industry,
  Country=country,
  'Market Cap'=mcap,
  'P/E'=PE,
  Price=price,
  'Change in %'=change,
  Volume=volume
)

View(data)

# approximately 11500 companies number changes as finviz updates regularly 

# ANALYSIS

website_data<-data

website_data<-website_data%>%
  filter(!is.na(Market.Cap)&!is.na(P.E))

View(website_data)

# MARKET CAP vs VOLUME

ggplot(data=website_data,aes(x=Market.Cap,y=Volume))+
  geom_point(alpha=0.5,color="steelblue")+
  theme_minimal()+
  labs(
    title="Market Cap vs Volume",
    x="Market Cap",
    y="Volume"
  )
cor(website_data$Market.Cap,website_data$Volume)

# cor of 0.3 was observed which is moderate correlation 

ggplot(data=website_data,aes(x=Market.Cap,y=Volume))+
  geom_point(alpha=0.5,color="steelblue")+
  coord_cartesian(
    xlim=c(0,5e10),
    ylim=c(0,5e6)
  )+
  theme_minimal()+
  labs(
    title="Market Cap vs Volume Zoomed",
    x="Market Cap",
    y="Volume"
  )

# zoomed in graph helps visualize correlation better as ignores edge cases

# INDUSTRY vs VOLUME TRADED

top_industries<-website_data%>%group_by(Industry)%>%
  summarize(Total_Volume=sum(Volume,na.rm=TRUE))%>%
  arrange(desc(Total_Volume))%>%head(10)

top_industries

ggplot(data=top_industries,aes(x=reorder(Industry,Total_Volume),y=Total_Volume))+
  geom_col(fill="steelblue")+
  coord_flip()+
  theme_minimal()+
  labs(title="Industry vs Volume Traded",
       y="Total Volume",
       x="Industry")

# this tells us that what industries are traded the most in a particular day
# tech at top is expected as often tech companies have the highest liquidity 

# COUNTRIES RANKED BY MARKET CAP

top_countries<-website_data%>%group_by(Country)%>%
  summarize(Market_Cap=sum(Market.Cap,na.rm=TRUE))%>%
  arrange(desc(Market_Cap))%>%head(10)

top_countries

ggplot(data=top_countries,aes(x=reorder(Country,Market_Cap),y=Market_Cap))+
  geom_col(fill="steelblue")+
  coord_flip()+
  theme_minimal()+
  labs(title="Country vs Market Cap",
       y="Market Cap",
       x="Country")

# US dominates as it is ultimately the most valuable country which can also be seen here

# COUNTRIES RANKED BY AVERAGE MARKET CAP

top_countries<-website_data%>%
  group_by(Country)%>%
  summarize(Average_Market_Cap=mean(Market.Cap,na.rm=TRUE))%>%
  arrange(desc(Average_Market_Cap))%>%
  head(10)

top_countries

ggplot(data=top_countries,aes(x=reorder(Country,Average_Market_Cap),y=Average_Market_Cap))+
  geom_col(fill="steelblue")+
  coord_flip()+
  theme_minimal()+
  labs(title="Country vs Average Market Cap per Company",
       y="Average Market Cap",
       x="Country")

# Taiwan South Korea dominate here it is expected as they are onle of the global leaders in the semiconductor market which is one of the most valuable industry
# US is not even in top 10
# Here countries who mostly dominate in only top industries dominate
# Countries with diverse markets are not present

# SECTORS RANKED BY MARKET CAP

top_sectors<-website_data%>%
  group_by(Sector)%>%
  summarize(Market_Cap2=sum(Market.Cap,na.rm=TRUE))%>%
  arrange(desc(Market_Cap2))%>%
  head(10)

top_sectors

ggplot(data=top_sectors,aes(x=reorder(Sector,Market_Cap2),y=Market_Cap2))+
  geom_col(fill="orange")+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels=label_comma())+
  labs(
    title="Sector vs Market Cap",
    y="Market Cap",
    x="Sector"
  )

# VOLATILITY OF DIFFERENT SECTORS


ggplot(data=website_data,aes(x=Sector,y=Change.in..))+
  geom_boxplot(fill="lightblue",outlier.color="red",outlier.alpha=0.5)+
  coord_flip()+
  theme_minimal()+
  labs(
    title="Sector Volatility: Distribution of % Change",
    x="Sector",
    y="Change (%)"
  )

# Width of the box plot tells how volatile a sector
# This essentially shows what sectors are ideal for low risk investments
# Change as compared to 0 also tells the trend of the market
# we can see that change in energy is positive hence it is a growing market
# we can see that change in real estate is negative hence it is a declining market 

ggplot(data=website_data,aes(x=Sector,y=Change.in..))+
  geom_boxplot(fill="lightblue",outlier.color="red",outlier.alpha=0.5)+
  scale_y_continuous(limits=c(-7.5,7.5))+
  coord_flip()+
  theme_minimal()+
  labs(
    title="Sector Volatility: Distribution of % Change Zoomed",
    x="Sector",
    y="Change (%)"
  )

# zoomed in graph removes edge cases hence is better for visualization 



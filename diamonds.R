library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
data("diamonds")

?diamonds

summary(diamonds)
head(diamonds)
ls.str(diamonds)

qplot(price,data = diamonds)

summary(diamonds$price)

diamonds %>% 
  filter(price < 500) %>% 
  summarise(price_less500 = n())

diamonds %>% 
  filter(price < 250) %>% 
  summarise(price_less250 = n())

diamonds %>% 
  filter(price >= 15000) %>% 
  summarise(price_emore15000 = n())

#ggsave("filename.png/jpeg/whatevs)

#the below indicates a natural break in price at 1500.
#This value will be used as an upper limit on cheap diamonds
qplot(x = price, data = diamonds, binwidth = 25)+
  coord_cartesian(xlim = c(0,2500))

#refined zoom
qplot(x = price, data = diamonds, binwidth = 25)+
  scale_x_continuous(breaks = seq(0,1500,100))+
  coord_cartesian(xlim = c(0,1500))
ggsave("Cheap Diamonds Zoom.png")

qplot(x = price, data = diamonds, binwidth = 25)+
  facet_wrap(~cut,ncol=2)
ggsave("Diamond Prices by Cut.png")

diamonds %>% 
  group_by(cut) %>% 
  summarise(min_price = min(price), max_price = max(price), med_price = median(price))

qplot(x = price, data = diamonds, binwidth = 25)+
  facet_wrap(~cut,ncol=2,scales = "free_y")
ggsave("Diamond Prices by Cut_free y.png")

qplot(x = price/carat, data = diamonds, xlab = 'price per carat', binwidth = 0.05)+
  scale_x_log10()+
  facet_wrap(~cut)
ggsave("Diamonds by Price per Carat.png")

ggplot(aes(x = clarity, y = price), data = diamonds) + geom_boxplot()
ggsave("Clarity and Price Box.png")

by(diamonds$price,diamonds$clarity,summary)

by(diamonds$price, diamonds$color, summary)

IQR(subset(diamonds, color = 'D')$price)

IQR(subset(diamonds, color = 'J')$price)

qplot(x = color, y = price/carat, data = diamonds, geom = 'boxplot', 
      color = I('red'), fill = I('green'),
      xlab = 'color', ylab= 'price per carat') +
  scale_y_continuous()
ggsave("Color and Price per carat box.png")

ggplot(aes(x = carat), data = diamonds) + geom_freqpoly(binwidth = 0.01)+
  scale_x_continuous(breaks = seq(0,5,0.1))



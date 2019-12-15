library(ggplot2)
library(tidyverse)
library(cowplot)

marketShare <- read.csv("MarketAnalysis.csv")
#normPriceWO <- marketShare$Estimated.Wholesale.Price.of.Non.Lockout.Shocks/max(marketShare$Estimated.Wholesale.Price.of.Non.Lockout.Shocks)
marketShare$total <- marketShare$Quantity.of.Non.Lockout.Shocks.Produced + marketShare$Quantity.of.Variable.Lockout.Shocks.Produced
summary(marketShare)

marketShare <- marketShare %>%
  mutate("p"=runif(18,min=0, max=1)) %>%
  mutate("TestTrain"=ifelse(p<=0.9,"Train","Test"))

#pulls separates training and testing data sets
TrainingSet <- marketShare %>% 
  filter(TestTrain =="Train")
TestingSet <- marketShare %>% 
  filter(TestTrain == "Test")

TrainingSet <- marketShare

woLM <- lm(Estimated.Wholesale.Price.of.Non.Lockout.Shocks ~ Quantity.of.Non.Lockout.Shocks.Produced + Quantity.of.Variable.Lockout.Shocks.Produced,
         data = TrainingSet)
TrainingSet$woEstimate <- woLM$fitted.values
# (Intercept)      Quantity.of.Non.Lockout.Shocks.Produced    Quantity.of.Variable.Lockout.Shocks.Produced 
#355.355095904                                 -0.008925757                                 -0.002430861 
sum(woLM$residuals) #9.992007e-16
(MSE_wo <- (sum(TrainingSet$Estimated.Wholesale.Price.of.Non.Lockout.Shocks - TrainingSet$woEstimate))^2/nrow(TrainingSet)) #8.077936e-28


p1 <- ggplot(TrainingSet) + 
  geom_point(aes(x=TrainingSet$Quantity.of.Non.Lockout.Shocks.Produced, 
                 y=TrainingSet$Estimated.Wholesale.Price.of.Non.Lockout.Shocks,
                 col=TrainingSet$Estimated.Wholesale.Price.of.Non.Lockout.Shocks),
                 size=3) +
  labs(title="Market Price of Air-Oil Bikes", 
         x = "Quantity of Air-Oil Bikes Sold", 
         y = "Quantity of Lockout Shock Bikes Sold",
         col = "Price of Air-Oil Bikes ($)") +
  theme(legend.position = c(0.6,0.75)) 


#geom_smooth(aes(x = TrainingSet$Quantity.of.Non.Lockout.Shocks.Produced,
 #               y= TrainingSet$woEstimate),
    #        se = FALSE)  +

ggplot(TrainingSet)+
  geom_tile(aes(x=TrainingSet$Quantity.of.Non.Lockout.Shocks.Produced, 
                   y=TrainingSet$Estimated.Wholesale.Price.of.Non.Lockout.Shocks,
                   fill=TrainingSet$Estimated.Wholesale.Price.of.Non.Lockout.Shocks))

wLM  <- lm(Estimated.Wholesale.Price.of.Variable.Lockout.Shocks ~ Quantity.of.Non.Lockout.Shocks.Produced + Quantity.of.Variable.Lockout.Shocks.Produced,
           data = TrainingSet)
TrainingSet$wEstimate <- wLM$fitted.values
#(Intercept)      Quantity.of.Non.Lockout.Shocks.Produced Quantity.of.Variable.Lockout.Shocks.Produced 
#624.409388112                                  0.002311864                                 -0.008406672 
sum(woLM$residuals) #-6.661338e-16
(MSEw <- (sum(TrainingSet$Estimated.Wholesale.Price.of.Variable.Lockout.Shocks - TrainingSet$wEstimate))^2/nrow(TrainingSet)) #0

p2 <- ggplot(TrainingSet) + 
  geom_point(aes(x=TrainingSet$Quantity.of.Variable.Lockout.Shocks.Produced, 
                 y=TrainingSet$Quantity.of.Non.Lockout.Shocks.Produced,
                 col = TrainingSet$Estimated.Wholesale.Price.of.Variable.Lockout.Shocks),
                 size =3) +
  labs(title="Market Price of Lockout Shock Bikes", 
       x = "Quantity of Lockout Shock Bikes Sold", 
       y = "Quantity of Air-Oil Bikes Sold",
       col = "Price of Lockout Shock Bikes ($)") +
  theme(legend.position = c(0.5,0.1)) 


plot_grid(p1,p2)

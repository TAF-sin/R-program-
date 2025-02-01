install.packages('tidyverse')
library('tidyverse')

data()
BOD


ggplot(data = BOD,
       mapping = aes(x = Time, y= demand)) +
 
  #increase the point size
   geom_point(size = 5)+
  geom_line(color = "red")



#make line for co2 


CO2

co2_plot<- ggplot(data = CO2, mapping = aes(x = conc, y = uptake ))+
  geom_point(size = 4)+
  geom_line( color = "blue")


#pipe operator %>%
 
CO2 %>%
  ggplot(aes(conc,uptake, color= Treatment))+
  geom_point(size = 3, alpha =0.5)+
  geom_smooth(method = lm, se = FALSE)+
  facet_wrap(~Type)+
  labs(title = "concentration of CO2")+
  theme_light()




CO2 %>%
  ggplot(aes(Treatment,uptake))+
  geom_boxplot()+
  geom_point(aes(size = conc , colour = Plant))+
  coord_flip()+
  theme_bw()+
  labs(title = "concentration of CO2")+
  facet_wrap(~Type)


#mpg

mpg
view(mpg)


mpg %>%
  ggplot(aes(displ,cty))+
  geom_point(aes(size = trans ,colour = drv),
             alpha = .5)+
  geom_smooth(method=lm)+
  facet_wrap(~year,nrow = 1)+
  labs(x = "Engine size",
       y = "mpg in the city",
       tittle = "fuel efficiency " )+
  theme_classic()


#scatterplot

data()

?mpg

mpg %>%
  filter(hwy < 35) %>%
  ggplot(aes(x = displ,
             y = hwy,
             colour = drv ))+
  geom_point()+
  geom_smooth(method = lm , se = F)+
  labs( x = "Engine size",
        y = "MPG ON highway",
        title = " fuel efficiency" )+
  theme_minimal()+
  ggsave("MPH_hwy.jpg")


#bar charts

library(tidyverse)

data()  
?msleep
view(msleep)

#single categorical

names(msleep)
msleep %>%
  drop_na(vore) %>%
  ggplot(aes(x = vore))+
  ##80FF33=  colorcode
  geom_bar(fill = "#80FF33")+
  # coord_flip() flip the graph
  theme_classic()+
  labs(x = "Vore",
       y = NULL,
       title = " number of observation")

#histogram

msleep %>%
  ggplot(aes(awake))+
  geom_histogram(binwidth = 4 , fill = "#80FF33" )+
  theme_bw()+
  labs(x = " total sleep",
       y = NULL,
       title = "total sleep time of animal")

#scatterplot
msleep %>%
  filter(bodywt < 2) %>%
  ggplot(aes(bodywt , brainwt))+
  geom_point(aes(color = sleep_total, size = awake))+
  geom_smooth()+
  theme_classic()+
  labs(x = "bodyweight",
       y = "brainweight",
       title = "brain and body weight")

#single categorical
View(Orange)
Orange %>%
  filter( Tree != "2") %>%
  ggplot(aes(age , circumference))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Tree)+
  theme_bw()+
  labs(title = " circumference of orange tree")

Orange %>%
  filter( Tree != "1" &
            Tree != "2") %>%
  ggplot(aes(x = age , y = circumference, color = Tree))+
  geom_point(size = 4 , alpha = .6)+
  geom_line(size = 1 , alpha =.7 )+
  theme_minimal()+
  labs(title = "Tree age & circumference")

msleep %>%
  drop_na(vore) %>%
  filter(vore == "herbi" | vore == "omni") %>%
  ggplot(aes(sleep_total, fill = vore))+
  geom_density(alpha = .5)
  theme_bw()
  
  
#starwars data set
  
view(starwars)

starwars %>%
  filter (hair_color == "black" |
          hair_color == "brown") %>%
  drop_na(sex) %>%
  ggplot(aes(hair_color , fill = sex))+
  geom_bar(position = "dodge", alpha = .5)+
  theme_minimal()
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
    labs( title = "Gender & Hair color",
         x = "hair_color",
         y = "number")

  


  

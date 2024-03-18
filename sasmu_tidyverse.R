################################################################################
################## SASMU TRAINING 15 MARCH 2024 ################################
##################### tidyverse package ########################################

#The tidyverse is a collection of R packages designed specifically for data 
#science. It was created with the aim of making data cleaning, manipulation,
#visualization, and modeling easier and more efficient.

## install package using install.package()
install.packages('tidyverse')

## call the package tidyverse using function library()
library(tidyverse)
## dataset to use
#data() to check dataset present
?msleep
## check data
head(msleep) # to view the top 6 rows
tail(msleep) # to view the bottom 6 rows 
glimpse(msleep) ## looking at structure of the dataset
view(msleep) ## to view full data 
names(msleep) ## to extract the variable names

#how deal with categorical variable-------------
#unique
msleep %>% 
  pull(vore) %>% 
  unique() 
#count
msleep %>% 
  count(vore) %>% 
  view()
## slice
msleep %>% 
  count(vore) %>% 
  slice(1) # specific row 

# to obtain top 3
msleep %>% 
  count(vore) %>% 
  slice_head(n=3)
# or 
msleep %>% 
  count(vore) %>% 
  head(n=3)

# to obtain bottom 3
msleep %>% 
  count(vore) %>% 
  slice_tail(n=3)
#or
msleep %>% 
  count(vore) %>% 
  tail(n=3)

#how to select variable-----
msleep%>%
  select(name,sleep_total)%>% ## to select variable in a dataset
  View()
msleep%>%
  select(-name,-sleep_total)%>% ## to remove  variable in a dataset use -
  View()
msleep%>%
  select(ends_with('wt'))%>% # to select the variable names that end with a particular character
  View()
msleep%>%
  select(starts_with('sleep'))%>%# to select the variable names that start with a particular character
  View()
msleep %>% 
  select(2:4,awake,starts_with('sleep'),
         contains('wt')) %>% 
  view()
#how to rename a variable name -----
df<-msleep %>% 
  rename('conserv'='conservation') %>% #start with new name
  select(name, conserv) %>% 
  view()
#sorting-------
#descending order
msleep %>% 
  select(vore) %>% 
  count(vore) %>% 
  arrange(desc(n)) %>% 
  view()
## ascending order
msleep %>% 
  select(vore) %>% 
  count(vore) %>% 
  arrange(n) %>% 
  view()

#how to filter observations-----
msleep %>% 
  filter(vore =="carni" )%>% ## exact  vore
  view()

msleep %>% 
  select(name,sleep_total)%>%
  filter(sleep_total<10 )%>%  ## sleep_total which is less than 10 
  view()

msleep %>% 
  select(name,vore,sleep_total)%>%
  filter(vore %in% c("carni","herbi" ))%>% ## 
  view()

msleep %>% 
  select(name,sleep_total) %>% 
  filter(between(sleep_total,8,10)) %>%  
  view()

msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order=="Carnivora",bodywt>20) %>% 
  view()
msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order=="Carnivora"| bodywt>20) %>%  # return is either is true
  view()
msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order=="Carnivora"& bodywt>20) %>%  # return if all are true
  view()
msleep %>% 
  select(name,sleep_total) %>% 
  filter(name=="Cow"|
           name=="Dog"|
           name=="Goat") %>%  
  view()
my_data2<-msleep %>% 
  select(name,sleep_total) %>% 
  filter(near(sleep_total,17,tol=0.5)) %>%  
  view()
#how to create column variable using mutate------
msleep %>%
  select(brainwt) %>% 
  mutate(brainwt_in_grams=brainwt*1000) %>% 
  view()
#lets us add a column called body height in (cm)
df<-msleep %>% 
  mutate(bodyht=rnorm(83,50,10)) %>% 
  select(name,bodyht) %>% 
  view()
#else_if
size_of_brain<-msleep %>% 
  select(name,brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size=if_else(brainwt>0.01,
                            "bright","foolish")) %>% 
  view()
#recode 
size_of_brain<-msleep %>% 
  select(name,brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size=if_else(brainwt>0.01,
                            "bright","foolish")) %>% 
  mutate(brain_code=recode(brain_size,
                           "bright"=1,
                           "foolish"=2 )) %>%
  view()

#dealing with stings---------
# upper  case
uppe<-msleep %>% 
  select(name) %>% 
  mutate(names=str_to_upper(name)) %>% 
  view()
#lower case
uppe<-msleep %>% 
  select(name) %>% 
  mutate(names=str_to_lower(name)) %>% 
  view()
# count
uppe<-msleep %>% 
  select(name) %>% 
  mutate(names=str_count(name)) %>% 
  view()

#summary and creating summary table
# summary
msleep %>% 
  select(sleep_total,brainwt) %>% 
  summary()
#create a summary table
msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(lower=min(sleep_total),
            average=mean(sleep_total),
            upper=max(sleep_total),
            difference=max(sleep_total)-min(sleep_total)) %>% 
  arrange(average) %>% 
  view()
# table, pivot wider and longer-------
#table
msleep %>% 
  select(vore) %>% 
  table() %>% 
  view()

wide_data<-msleep %>%
  select(name,order, vore) %>% 
  drop_na() %>% 
  pivot_wider(names_from = vore,values_from = order )%>% 
  view()
long_data<-wide_data %>% 
  pivot_longer(2:5,
               names_to = "vore",values_to = "order") %>% 
  view()

#how to separate, fill, unite and replace -------
# create a data frame 
name<-data.frame(Name=c("maurice odhiambo",
                     "moses Macdonald",
                     "Catherine wavering "))
# separate 
d1<-name%>% 
  separate(col = Name,
           into = c("first name","last name"),
           sep = " ") %>% 
  view()
#unite
d1 %>% 
  unite(col = "full names",
        "first name","last name",
        sep = " ") %>% 
  view()
# replace
name <- name %>%
  mutate(Name = replace(Name, 
                        Name == "maurice odhiambo",
                        "Maurice willington")) %>% 
  view()
#how to fill
# create a data frame 
data<-data.frame(type=c('domestic',NA,NA,NA,"wild",NA,NA,NA),
                 animals=c('dog','cow','donkey','cat', 'elephant',
                           'monkey','lion','tiger'))
view(data)
# fill
data %>% 
  fill(type) %>% 
  view()

#ggplot----------
# histogram
msleep %>% 
  select(sleep_total,order) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(sleep_total))+
  geom_histogram(bins = 8,color='white',fill='grey')+
  theme_classic()

#barplot
view(msleep)
msleep %>% 
  select(vore) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(vore))+ 
  geom_bar()+ # barplot
  labs(title= "counts of vore", # labeling
       xaxis='vore',
       yaxis='counts')+
  theme_bw()+ # theme
  theme(plot.title = element_text(color = "red",hjust = 0.5,)) # customize theme

#scatter plot
msleep %>% 
  select(sleep_total,brainwt,bodywt) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(sleep_total,bodywt))+
  geom_point()+
  labs(title= "scatter plot of sleep total and bodyweightbodyweight",
       xaxis='sleep total',
       yaxis='body weight')+
  theme_bw()+
  theme(plot.title = element_text(color = "green",hjust = 0.5,))

#line graph
msleep %>% 
  select(sleep_total,brainwt,bodywt) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(sleep_total,bodywt))+
  geom_line()+
  labs(title= "line graph sleep total and bodyweightbodyweight",
       xaxis='sleep total',
       yaxis='body weight')+
  theme_bw()+
  theme(plot.title = element_text(color = "green",hjust = 0.5,)) 

#boxplot
msleep %>% 
  select(sleep_total,order,vore) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(sleep_total))+
  geom_boxplot(fill='brown')+
  labs(caption = "boxplot created by tidyverse")
  #coord_flip()+
  #facet_wrap(~vore)+
  theme_classic()

# density plot
msleep %>% 
  select(sleep_total,order) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(sleep_total))+
  geom_density ()+
  theme_classic()
# how to save 
ggsave("msleep.png",height = 7,width = 10,units = "cm",dpi = 300)


#---------------
print(matrix(rep('thank you',15),ncol=1))
#--------------

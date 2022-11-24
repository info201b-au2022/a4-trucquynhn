library(tidyverse)
# The functions might be useful for A4
source("../source/a4-helpers.R")
library(scales)
library("dplyr")
library("ggplot2")
library("plotly")
library("leaflet")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
library("dplyr")
library("tidyverse")
library("ggplot2")

jail_data <-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# The highest number of black people in jail 
highest_black <-jail_data %>% 
  select(black_jail_pop) %>% 
  drop_na() %>% 
  filter(black_jail_pop==max(black_jail_pop)) %>% 
  pull(black_jail_pop)

# The highest number of white people in jail
highest_white <-jail_data %>% 
  select(white_jail_pop) %>% 
  drop_na() %>% 
  filter(white_jail_pop==max(white_jail_pop)) %>% 
  pull(white_jail_pop)


# Which state had the highest number
highest_state <-jail_data %>% 
  select(state,total_pop) %>% 
  drop_na %>% 
  filter(total_pop==max(total_pop)) %>% 
  pull(state)


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function will get the range of total jail populations between 1970 and 2018

get_year_jail_pop <- function() {
  Year <- c(1970:2018)
  incarceration <- jail_data %>% 
    select(year,total_jail_pop) %>% 
    drop_na() %>% 
    filter(year%in%Year) %>% 
    group_by(year) %>% 
    summarise(total_jail_pop=sum(total_jail_pop))
  return(incarceration)

}
# This function plots the data
plot_jail_pop_for_us <- function()  {
  bar <-get_year_jail_pop()
  plot <-ggplot(data =bar) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous("Total Jail Population", labels = comma)+
      labs(x = "Year", y= "Total Jail Population",title= "Increase of Jail Population in U.S (1970-2018") 
  return(plot)
}
bar <-get_year_jail_pop()
Plot <-ggplot(data =bar) +
  geom_col(mapping = aes(x = year, y = total_jail_pop)) +
  scale_y_continuous("Total Jail Population", labels = comma)+
  labs(x = "Year", y= "Total Jail Population",title= "Increase of Jail Population in U.S (1970-2018")

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
test <- c("WA", "OR", "CA")
get_jail_pop_by_states <- function(states) {
  Year <- c(1970:2018)
  incarceration <- jail_data %>% 
    select(year,total_jail_pop, state) %>% 
    drop_na() %>% 
    filter(state%in%states) %>% 
    filter(year %in% Year) %>% 
    group_by(year) %>% 
    summarise(total_jail_pop=sum(total_jail_pop), state = state, .groups = "drop")
  return(incarceration)

}


plot_jail_pop_by_states <- function(states) {
  Data_States_Line <- get_jail_pop_by_states(states)
  Updated <- Data_States_Line %>%
    gather(key = Jail_Pop, value = States, state) %>%
    group_by(year, States) %>%
    summarise(total_jail_pop = sum(total_jail_pop), .groups = "drop")
  Create_Plot <- ggplot(data = Updated) +
    geom_line(mapping = aes(x = year, y= total_jail_pop, color = States), size = 4) +
    scale_y_continuous("Total Jail Population", labels = comma)+
    labs(x = "Year", y = "Total Jail Population", )
  return(Create_Plot)
}

#In this line chart, we can see that the state California has the highest peak in jail populations.Caliorinias jail population went up around 1990 and has been up there until this year. As also can be seen, Washington and Orgeons are low comapared to Califorina. 

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#Creating a bar chart in order to be inequailties between male and between throughout states 

get_year_jail_pop <- function() {
  Year <- c(1970:2018)
  confinement <- jail_data %>% 
    select(female_jail_pop,male_jail_pop, year) %>% 
    drop_na() %>% 
    filter(year%in%Year) %>% 
    group_by(year) %>% 
    summarise(male_jail_pop=sum(male_jail_pop))
  return(confinement)
  
}
# This function plots the data
sex_plot <- function()  {
  bar <-get_year_jail_pop()
  imprisonment <-ggplot(data =bar) +
    geom_col(mapping = aes(x = year, y = male_jail_pop)) +
    scale_y_continuous("Male Jail Population", labels = comma)+
    labs(x = "Year", y= "Total Jail Population",title= " Jail Population Male (1970-2018") 
  return(imprisonment)
}

#Female Bar Chart

girl_plot <- function() {
  Year <- c(1970:2018)
  custody <- jail_data %>% 
    select(female_jail_pop,male_jail_pop, year) %>% 
    drop_na() %>% 
    filter(year%in%Year) %>% 
    group_by(year) %>% 
    summarise(female_jail_pop=sum(female_jail_pop))
  return(custody)
  
}
# This function plots the data
female_sex_plot <- function()  {
  bar <-girl_plot()
  time <-ggplot(data =bar) +
    geom_col(mapping = aes(x = year, y = female_jail_pop)) +
    scale_y_continuous("Female Jail Population", labels = comma)+
    labs(x = "Year", y= "Total Jail Population",title= " Jail Population Female (1970-2018") 
  return(time)
}


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 



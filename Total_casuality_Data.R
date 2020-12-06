# you can create a branch from the parent branch and continue to write code and publish it to your branch
install.packages("pxR")
install.packages("ggplot2")
library(pxR)
library("ggplot2")
d<-as.data.frame(read.px("ROA16.px"))

str(d)
head(d)
colnames(d)
dim(d) 
check_factors<-function(d){
  list1<-d[]
  sapply(list1, factor(list1))
}

library("dplyr")


# There are 5 categorical data in the data-set i.e. 
# Statistics has 3 levels
levels(d$Statistic)

# years has 14 levels as:
levels(d$Year)

# Road user type has 8 levels
levels(d$Road.User.Type)

# Sex has 2 levels
levels(d$Sex)

# age group has 13 levels
levels(d$Age.Group)

# if we see the total no. of casualities over the years we can sum it as the below data set
d1<-subset(d,d$Statistic=="All Killed and Injured Casualties (Number)"
           &
             d$Road.User.Type=="All road users" 
           & 
             d$Age.Group=="All ages"
           & 
             d$Sex=="Male")%>%
  group_by(Year)

d2<-subset(d,d$Statistic=="All Killed and Injured Casualties (Number)"
           &
             d$Road.User.Type=="All road users" 
           & 
             d$Age.Group=="All ages"
           & 
             d$Sex=="Female")%>%
  group_by(Year)
total_casuality<-as_tibble(full_join(d1,d2,by=c("Year","Road.User.Type","Age.Group")))
total_casuality<-mutate(total_casuality,Total_vaue=total_casuality$value.x+total_casuality$value.y)


# if we see the total no. of injuries over the years we can sum it as the below data set
d_only_injured_male<-subset(d,d$Statistic=="Injured Cassualties (Number)"
           &
             d$Road.User.Type=="All road users" 
           & 
             d$Age.Group=="All ages"
           & 
             d$Sex=="Male")%>%
  group_by(Year)

d_only_injured_female<-subset(d,d$Statistic=="Injured Cassualties (Number)"
           &
             d$Road.User.Type=="All road users" 
           & 
             d$Age.Group=="All ages"
           & 
             d$Sex=="Female")%>%
  group_by(Year)
total_casuality_injured<-as_tibble(full_join(d_only_injured_male,d_only_injured_female,by=c("Year","Road.User.Type","Age.Group")))
total_casuality_injured<-mutate(total_casuality_injured,Total_vaue_injured=total_casuality_injured$value.x+total_casuality_injured$value.y)

  
normalized_data <-data.frame("year"=c(2005:2018),
                             "Road.User.Type"=rep("All road users",14),
                            "Injured"=total_casuality_injured$Total_vaue_injured,
                             "Killed_and_Injured"=total_casuality$Total_vaue,
                            "killed"=total_casuality$Total_vaue-total_casuality_injured$Total_vaue_injured)

# summary for casualities
summary(normalized_data)
# ggplot(normalized_data,mapping=aes(x=year,y=Injured))+
#   geom_line(col="blue")+
#   line(normalized_data$year,normalized_data$Killed_and_Injured,col="green",ylim=c(0,10000)

# plot for the total casuaities
plot(normalized_data$year,normalized_data$Killed_and_Injured,col="green",type="l",xlab="Year",ylab = "Casualities",ylim=c(0,10000))
lines(normalized_data$year,normalized_data$Injured,col="blue",type="l")
lines(normalized_data$year,normalized_data$killed,col="red",type="l")

lm()
       
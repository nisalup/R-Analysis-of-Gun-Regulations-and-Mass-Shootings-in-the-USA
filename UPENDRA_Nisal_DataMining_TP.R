#PART 1 LOADING AND ORGANIZING DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#I need the following packages
#install.packages("dplyr")
#install.packages("maps")
#install.packages("choroplethr")
#install.packages("choroplethrMaps")
library(ggplot2)
library(plotly)
library(dplyr)
library(choroplethr)
library(data.table)
library(ggthemes)
#install.packages("sqldf")
library(ggpubr)
library(sqldf)
gridExtra

#first, I load the data regarding gun laws in the US by State
setwd("F:\\M1 MLDM\\Semester 2\\Data Mining\\Practical\\Project")
law_data<-read.csv('raw_data.csv')

#I only take the column called lawtotal, which takes the cumalative number of laws 
#applicable to every state on a 1,0 basis;
#i.e. if a restriction is imposed, the value 1 is assigned for a partcular law, and, 
##in general, more laws correspond to a higher the value of the lawtotal column and 
#more restrictions to gun use in that respective state.
#So, I create a filter so that the dataset will have only lawtotal 
#data related to 2017

law_data = subset(law_data, year=='2017')
law_data = law_data[,c("state", "lawtotal")]

#next, I load data from the mass_shootings_dataset, whiwh has information on all 
#gun related mass shootings carried out in the US since 1966.

mass_shootings<-read.csv('Mass Shootings Dataset.csv')

#assign to data table
mass_shootings_data <- data.table(mass_shootings)

head(mass_shootings_data)
dim(mass_shootings_data)

#I start the analysis with a US map to show the distribution of gun laws
#by state. There are a total of 133 laws in the dataset. I will use the 
#lowtotal column to find out how many of them have been implemented in
#each state

tt<-law_data %>%
  dplyr::select(state,lawtotal) %>% 
  dplyr::rename(region=state,value=lawtotal) %>% 
  dplyr::mutate(region=tolower(region),
                value = round(value,0))
choro = StateChoropleth$new(tt)
choro$title  = "Firearms Regulations Implementation Across States"
choro$ggplot_scale = scale_fill_manual(name="No. of Laws", values = rev(viridis::viridis(10)),drop=FALSE)
choro$render()

#To have a more precise Idea, we will select the top 10 states, with 
#the most firearm regulations and the state with the least firearms regulations.

#tbd

#then let us have a look at the geographical spread of the mass shootings from 1966
#for this we need to split the location feature as it includes both state and town,
#and then add a new column called state. This is needed because the data in the location
# column is not consistent, especially with state names. I tied splitting the data by a comma
# only, but it is not effective enough because I will miss a lot of data.
#therefor I split the data, and replaced the abbrevations with the state names.

mass_shootings_data$State <- sapply(mass_shootings_data$Location, function(x){
  temp <- strsplit(as.character(x), split = ",")
  sapply(temp, function(y){y[2]
    
  })
})
# inspired by https://www.kaggle.com/antonaks/mass-shooting-in-us-using-plotly/notebook
#for purposes of data cleaning
mass_shootings_data[is.na(`State`), `State`:="Unknown"]
mass_shootings_data[`State`==' CA'| `State`== ' San Diego'| `State`==" LA" |`State`== " California", `State`:="California"]
mass_shootings_data[`State`==' NV'| `State`==" Nevada",`State`:="Nevada"]
mass_shootings_data[`State`==' CO'| `State` == " Colorado",`State`:="Colorado"] 
mass_shootings_data[`State`=='  Virginia'|`State`==" Virginia",`State`:="Virginia"]
mass_shootings_data[`State`==" TX"| `State` == " Texas",`State`:="Texas"]
mass_shootings_data[`State`==" MD",`State`:="Maryland"]
mass_shootings_data[`State`==" PA"|`State`==" Lancaster"|`State`==" Souderton",`State`:="Pennsylvania"]
mass_shootings_data[`State`==" WA"|`State`==" Washington",`State`:="Washington"]

# set colors for pie chart
colors_pie1 <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

#pie chart representation
plot_ly(data = mass_shootings_data[!is.na(State),.('Number of incidents'= uniqueN(`S.`)),by=State]
        ,type = 'pie'
        ,labels = ~State
        ,values = ~`Number of incidents`
        ,textposition = 'inside'
        ,insidetextfont = list(color = '#FFFFFF')
        ,marker = list(colors = colors_pie1,
                       line = list(color = '#FFFFFF', width = 1)))%>%
  layout(title = "Number of incidents by States",
         showlegend = T)


#The next step is to show relation between gun laws and number of incidents. But for this, I
# need to add a field called number_of_incidents to the gun_laws table, which takes data from the mass_shootings_dataset 
# and counts the number of incidents for each state.

#remove leading and trailing whitespaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#newcolumn to add incident count with total number of laws in the state
law_data$count <-NA

for(i in 1:nrow(law_data)) {
  row <- law_data[i,]
  state_temp <- trim(row$state)
  print(state_temp)
  incident_count = 0
  
  for(j in 1:nrow(mass_shootings_data)) {
    row1 <- mass_shootings_data[j,]
    state_incident <- trim(row1$State)
    if(state_temp == state_incident){
      incident_count <- incident_count + 1
    }
    
  }
  law_data$count[i] <- incident_count
  print(incident_count)
}


#number of incedents by state shown in a US map using dplyr
ff<-law_data%>%
  dplyr::select(state,count) %>% 
  dplyr::rename(region=state,value=count) %>% 
  dplyr::mutate(region=tolower(region),
                value = round(value,0))
choro1 = StateChoropleth$new(ff)
choro1$title  = "NUmber of Incidents by State"
choro1$ggplot_scale = scale_fill_manual(name="No. of Incidents", values = rev(viridis::viridis(10)),drop=FALSE)
choro1$render()
# now we have the required law_data dataset
#I will do a correlation analysis first
#Pearson correlation test
scatter1 <- ggscatter(law_data, x = "lawtotal", y = "count", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Number of Laws a Select State", ylab = "Total Number of Incidents")

scatter1


#see if my data has a normal distribution
# Shapiro-Wilk normality test for lawtotal
shapiro.test(law_data$lawtotal) 
# Shapiro-Wilk normality test for count
shapiro.test(law_data$count) 

#since the results show both datasets are lower than the significance level, we caanot assume a normal distribution


#visual inspection
ggqqplot(law_data$lawtotal, ylab = "Total Number of laws")
ggqqplot(law_data$count, ylab = "Incident Count")

#we will use non parametric methods, for spearman correlation:
dd<- cor.test(law_data$lawtotal, law_data$count, method="spearman")
dd
#this shows a noticeable positive correlation, the normal dstribution cannot be assumed becaus of the outliers

# we will also do a linear regression to see if there is a relationship between them
m1<-lm(lawtotal~count,data=law_data)
summary(m1)
par(mfrow=c(2,2))
plot(m1)


#Next, I will try to do a classification to see the following relationship.
#If the race of the attacker can be predicted by the gunlaws in the state, and the total number of victims in the attack.

#for this, I will first need to clean the data in the race column in the mass_shootings_dataset

mass_shootings_data[is.na(`Race`)|`Race`==""|`Race`=="Black"|`Race` %like% "Native"|`Race`=="Latino"|`Race`=="Unknown"|`Race` %like% "Asian"|`Race`=="Black American or African American"|`Race`=="black"|`Race`=="Some other race"|`Race`=="Two or more races"|`Race`=="Asian American"|`Race`=="Black American or African American/Unknown", `Race`:="Other"]
mass_shootings_data[`Race` %like% "%White%"|`Race` %like% "European"|`Race` == "White American or European American"|`Race` %like% "white"|`Race`=="White American or European American/Some other Race" ,`Race`=="White"]

for(i in 1:length(mass_shootings_data$Race)){
  if(mass_shootings_data$Race[i] == "White American or European American"){
    mass_shootings_data$Race[i] <- "White"
  }
}

for(i in 1:length(mass_shootings_data$Race)){
  if(mass_shootings_data$Race[i] == "white"){
    mass_shootings_data$Race[i] <- "White"
  }
}

for(i in 1:length(mass_shootings_data$Race)){
  if(mass_shootings_data$Race[i] == "unclear"){
    mass_shootings_data$Race[i] <- "Other"
  }
}

mass_shootings_data$Race[146] <- "White"
print(mass_shootings_data$Race)

#add a column to mass_shootings data set with the number of gun laws in the state where the 
#attack occured
mass_shootings_data$NumGunLawsInState <- NA

for(i in 1:nrow(mass_shootings_data)) {
  temp_state <- mass_shootings_data$State[i]
  temp_law_data <- filter(law_data,trim(law_data$state) == trim(temp_state))
  print(temp_law_data[1,]$count)
  mass_shootings_data$NumGunLawsInState[i] <- temp_law_data[1,]$count
  }

mass_shootings_kmeans <- mass_shootings_data[,c("Total.victims", "NumGunLawsInState", "Race")]
#install.packages("tidyr")
library(tidyr)
mass_shootings_kmeans <- mass_shootings_kmeans %>% drop_na(NumGunLawsInState)
mass_shootings_kmeans <- as.data.table(mass_shootings_kmeans)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mass_shootings_kmeans))

## set the seed to make the  partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mass_shootings_kmeans)), size = smp_size)

train_data <- mass_shootings_kmeans[train_ind, ]
test_data <- mass_shootings_kmeans[-train_ind, ]

x <- mass_shootings_data[,c("Total.victims", "NumGunLawsInState")]
y <- mass_shootings_kmeans$Race

library("e1071")

colnames(law_data)

svm_model <- svm(Race~., data= mass_shootings_kmeans)
summary(svm_model)
plot(svm_model, mass_shootings_kmeans)

#Now we will use a KNN classifier to see if we can predict the race of the attacker based on the states
#gun laws and based on the number of victims

# the assumption here is that most attackers who do mass killings, who are from states with relaxed gun laws are white.



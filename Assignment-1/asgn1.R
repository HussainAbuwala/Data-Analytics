#----------------------Calling required libraries-----------------------------
#-----------------------------------------------------------------------------

install.packages("fmsb")
install.packages("dplyr")
install.packages("igraph")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("ggradar")
install.packages("scales")
install.packages("chron")
install.packages("tibble")

library(tibble)
library(fmsb)
library(dplyr)
library(chron)
library(wordcloud)
library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)
#-----------------------------------------------------------------------------


#----------------------Loading the csv file-----------------------------------
#-----------------------------------------------------------------------------

originalData <- read.csv("webforum.csv", header = TRUE)
head(originalData)
#-----------------------------------------------------------------------------


#-------------------------------Data Cleaning---------------------------------
#-----------------------------------------------------------------------------


#removing rows with word count less than 20
cleanedData <- subset(cleanedData,   cleanedData[6] >= 20) 

#removing rows with given attribute values less than 0
cleanedData <- subset(cleanedData,    cleanedData[7] >= 0 |
                                      cleanedData[8] >= 0 |
                                      cleanedData[9] >= 0 |
                                      cleanedData[10] >= 0 |
                                      cleanedData[11] >= 0 |
                                      cleanedData[12] >= 0 |
                                      cleanedData[13] >= 0 |
                                      cleanedData[14] >= 0 |
                                      cleanedData[15] >= 0 |
                                      cleanedData[16] >= 0 |
                                      cleanedData[17] >= 0 |
                                      cleanedData[18] >= 0 |
                                      cleanedData[19] >= 0 |
                                      cleanedData[20] >= 0 |
                                      cleanedData[21] >= 0 |
                                      cleanedData[22] >= 0 |
                                      cleanedData[23] >= 0 |
                                      cleanedData[24] >= 0 |
                                      cleanedData[25] >= 0 |
                                      cleanedData[26] >= 0 |
                                      cleanedData[27] >= 0 |
                                      cleanedData[28] >= 0 |
                                      cleanedData[29] >= 0 |
                                      cleanedData[30] >= 0 |
                                      cleanedData[31] >= 0 |
                                      cleanedData[32] >= 0 )
#----------------------------------------------------------------------------------------------------------


#-----------------Plot graph showing comparison of similarity of language used in thread-------------------
#---------------------- and language used in forum using standard deviation -------------------------------

                                          #TYPE OF GRAPH - BAR CHART

#find standard deviation for each attribute by factor = thread
sd_of_threads = as.data.frame(aggregate(cleanedData[7:31], cleanedData[2], sd))
thread1 = sd_of_threads[sample(1:500, 1),2:26]
thread2 = sd_of_threads[sample(1:500, 1),2:26]


#find standard deviation for eac attribute by no factor. Full forum
sd_of_cleanedData = data.frame(lapply(cleanedData[7:31], sd))
sd_of_cleanedData


#merges both the rows of thread and forum
t1 = rbind(thread1,sd_of_cleanedData)
t1 = c(as.numeric(t1[1,]),as.numeric(t1[2,]))
#converts to matrix for being able to bar plot
t1 = matrix(t1,nrow=2,ncol=25,byrow=TRUE)
#labels the coloumns
colnames(t1)=c("Anlt","Clout", "Auth", "Tone", "ppron", "i", "we", "you", "shehe", "they", "num", "aff", "pos", "neg", "anx","ang","soc","fam","frn","work","lsr","home","mny", "rel", "swear")
#labels the rows
rownames(t1)=c("Thread1","Forum")
#bar plots the data
barplot(t1, col=colors()[c(45,89)] , main = "Thread vs Forum similarity comparison", border="white", beside=T, legend=rownames(t1),ylim=c(0,35), xlab="Language Attributes", ylab = "Standard Deviation Value")

#merges both the rows of thread and forum
t2 = rbind(thread2,sd_of_cleanedData)
t2 = c(as.numeric(t2[1,]),as.numeric(t2[2,]))
#converts to matrix for being able to bar plot
t2 = matrix(t2,nrow=2,ncol=25,byrow=TRUE)
#labels the coloumns
colnames(t2)=c("Anltc","Clout", "Auth", "Tone", "ppron", "i", "we", "you", "shehe", "they", "num", "aff", "pos", "neg", "anx","ang","soc","fam","frn","work","lsr","home","mny", "rel", "swear")
#labels the rows
rownames(t2)=c("Thread2","Forum")
#bar plots the data
barplot(t2, col=c("#79c36a","#f1595f"),main = "Thread vs Forum similarity comparison", border="white", beside=T, legend=rownames(t2),ylim=c(0,35), xlab="Language Attributes", ylab = "Standard Deviation Value")

#----------------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------------
#----------------------Barchart showing difference of optimism proportion between groups-------------------

                                #TYPE OF GRAPH - grouped bar chart

#calculates mean of posemo and negemo for every thread
mean_p_n_t = as.data.frame(aggregate(cleanedData[19:20], cleanedData[2], mean))
#takes specific rows (i.e,threads) from the dataset above
thread1_p_n = mean_p_n_t[sample(1:500, 1),2:3]
thread2_p_n = mean_p_n_t[sample(1:500, 1),2:3]
thread3_p_n = mean_p_n_t[sample(1:500, 1),2:3]
thread4_p_n = mean_p_n_t[sample(1:500, 1),2:3]
thread5_p_n = mean_p_n_t[sample(1:500, 1),2:3]

#merges all the threads together
group = rbind(thread1_p_n,thread2_p_n,thread3_p_n,thread4_p_n,thread5_p_n)

group = c(as.numeric(group[1,]),as.numeric(group[2,]),as.numeric(group[3,]),
       as.numeric(group[4,]),
       as.numeric(group[5,]))

#converts datatype to matrix to be able to plot in bar chart 
group = matrix(group,nrow=5,ncol=2,byrow=TRUE)

#labels the coloumns and rows
colnames(group)=c("Postive Emotions","Negative Emotions")
rownames(group)=c("Thread1","Thread2","Thread3","Thread4","Thread5")

#plots the bar chart
barplot(group, col=colors()[c(45,85,20,90,470)], main="Optimism proportion comparison between threads", border="white", beside=T, legend=rownames(group), ylab="Mean value")

#------------------------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------------------------
#----------------------Plot graph showing how language changes with time for each thread---------------------------

                                      #TYPE OF GRAPH - line graph


#convert date column to date class in R
cleanedData[4] = data.frame(as.Date(cleanedData[,4]));

#ADD new column of year
cleanedData[33] = format(cleanedData[4],format="%Y")


#group by year and thread and find mean of attributes
summary_attr = cleanedData[19:20]
summary_attr[3] = cleanedData[33]

#caluclates the mean value of posemo and negemo by each year
mean_year = as.data.frame(aggregate(summary_attr[1:2], summary_attr[3], mean))

#renames the coloumn heading 
colnames(mean_year)[1] = "Year";

#plotting line chart for positive emotions vs year
ggplot(data = mean_year, aes(x=as.numeric(Year), y=posemo)) +
  geom_point(color = 'cyan')+
  geom_line(color = 'cyan', linetype = "dashed") +
  labs(x = 'Year'
       , y = 'Positive emotion mean value'
       , title = "Change in positive emotions over time") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444")
        ,panel.background = element_rect(fill = '#444B5A')
        ,panel.grid.minor = element_line(color = '#4d5566')
        ,panel.grid.major = element_line(color = '#586174')
        ,plot.title = element_text(size = 20)
        ,axis.title = element_text(size = 15, color = '#555555')
  ) 


#plotting line chart for negative emotions vs year
ggplot(data = mean_year, aes(x=as.numeric(Year), y=negemo)) +
  geom_point(color = 'cyan')+
  geom_line(color = 'cyan', linetype = "dashed") +
  labs(x = 'Year'
       , y = 'Negative emotion mean value'
       , title = "Change in negative emotions over time") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444")
        ,panel.background = element_rect(fill = '#444B5A')
        ,panel.grid.minor = element_line(color = '#4d5566')
        ,panel.grid.major = element_line(color = '#586174')
        ,plot.title = element_text(size = 20)
        ,axis.title = element_text(size = 15, color = '#555555')
  ) 
#----------------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------------
#-------------------------------WHAT TIME OF DAY HAS MOST ACTIVITY-----------------------------------------
                                
                                #TYPE OF GRAPH - BAR CHART / PIE CHART

#data in reproducible form
df2 <- data.frame(Times = cleanedData[,5])

#append a chron "times" class column
df2$Times <- times(paste0(df2$Times, ":00")) 

#times are internally broken down into fractions of a day
breaks <- c(0,6, 12, 18, 24) / 24 
#labels each time period respectively
labels <- c("Late-night","Morning", "Afternoon", "Night")
df2$ind <- cut(df2$Times, breaks, labels, include.lowest = TRUE)


#adds a coloumn which corresponds to which time of the day it is
cleanedData[34] = df2[2]

#calculates frequency of posts during each time period
day_freq = data.frame(table(cleanedData$ind))
day_freq[1,1]

#Creates data for the graph.
x <- c(day_freq[1,2],day_freq[2,2],day_freq[3,2],day_freq[4,2])
labels <- c("Late-night", "Morning", "Afternoon", "Night")

#calulcates percentage for pie-chart values
piepercent<- round(100*x/sum(x), 1)

#plotting the pie chart
pie(x, labels = piepercent, main = "Posts Activity at different times of the day",col = rainbow(length(x)))
legend("bottomright", c("Late-night (00:00 - 06:00) ", "Morning (06:01 - 12:00)", "Afternoon (12:01 - 18:00)", "Night (18:01 - 23:59)"), cex = 0.8,
       fill = rainbow(length(x)),title = "Timings")

#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------FIND TYPE OF LANGUAGE USED BY AUTHOR------------------------------------------

                                          #TYPE OF GRAPH - RADAR CHART

#mean of language attributes grouped by AuthorID
mean_of_author = as.data.frame(aggregate(cleanedData[7:31], cleanedData[3], mean))
mean_of_author


#creates a list of the values that need to be plotted
mean_of_author %>%
  rownames_to_column( var = "group" ) %>%
  mutate_at(vars(-group),funs(rescale)) %>%
  head(4) %>% select(2:6) -> mean_of_author_radar

#sets the respective AuthorID
mean_of_author_radar[1] = head(mean_of_author,4)[1]

#plots the radar chart
ggradar(mean_of_author_radar, plot.legend  = TRUE,legend.title = "AuthorID") 


#-----------------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------Word Cloud Of most common word types in a thread----------------------------
                                        
                                            #TYPE OF GRAPH - WORD CLOUD

#mean of language categories grouped by thread
mean_of_thread = as.data.frame(aggregate(cleanedData[21:31], cleanedData[2], mean))
#removing social
mean_of_thread[4] = NULL

#mean of language categories of the forum
mean_of_cleanedData = data.frame(lapply(cleanedData[21:31], mean))
#removing social
mean_of_cleanedData[3] = NULL

#plotting the word cloud
wordcloud(c("anxiety","anger","family","friend","work","leisure", "home", "money", "religion", "swear"), mean_of_cleanedData[,1:10],
          ordered.colors=TRUE
)

#plotting the word cloud
wordcloud(c("anxiety","anger","family","friend","work","leisure", "home", "money", "religion", "swear"), mean_of_thread[100,2:11],
          ordered.colors=TRUE
)

#--------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------
#-------------------------------------Scatter plot to find correlation of variables----------------------------
                                              
                                                #TYPE OF GRAPH - Scatter Plot

#anger vs swear = correlation exists but other factors present
#--------------

#gathering data values for anger and swear words
anger = cleanedData[,22]
swear = cleanedData[,31]

#plotting a scatterplot of swear vs anger 
plot(anger,swear, main="Correlation between anger and swear words", xlab = "Words related to anger", ylab="Swear words")

#creating a linear regression line
fit = lm(swear ~ anger)
#drawing the line on exisiting plot
abline(fit, col = "red")
#gives summary of the regression
summary(fit)


#work vs money
#-------------

#gathering data values for words related to work and money
work = cleanedData[,26]
money = cleanedData[,29]

#plotting a scatterplot of money vs work 
plot(work,money,main="Correlation between work and money related words", xlab = "Words related to work", ylab="Words related to money")

#creating a linear regression line
fit = lm(money ~ work)
#drawing the line on exisiting plot
abline(fit, col = "red")
#gives summary of the regression
summary(fit)


#-------------------------------------------------------------------------------------------------


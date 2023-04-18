
# === description ==============================================================

# === to do ====================================================================

# 1) Clean up code as its confusing
# 2) Birth Rate vs precipitation?
# 3) Mass Vs Temp - redo 
# 4) Figure out what to do about mass/body condition 
# 5) Create final figures 
# 6) create final analysis 
# 7) put precip and temp into the same plot 



# === read in cleaned snake data  ===================

## READ IN THE CLEANED DATA FROM CLEAN DATA FOLDER


d.c <- read.csv(paste(p.data.clean, "snake.all.years.clean.csv", sep = ""))
head(d.c)
d.c$year <- as.character(substr(d.c$catch.date, 1, 4))
unique(d.c$year)
is.na(d.c$year) == T
loc.NA <- which(is.na(d.c$year) == T)
d.c[loc.NA, ]

# remove for now
d.c <- d.c[-loc.NA,]
table(d.c$year)

#the amount of data points per year? Seems good to know 
point.per.year <- aggregate(d.c$year, list(d.c$year), length)


# === Read in cleaned climate data  ===================

# read the cleaned climate data
clim.d <- read.csv(paste(p.data.clean, "climate.years.all.clean.csv", sep = ""))
str(clim.d)
clim.d <- clim.d[,-1]
clim.d$Date.Time <- as.Date(clim.d$Date.Time)

# define the period of interest for each year to get e.g. mean temp
# start month and days in mm-dd format Jan 1st till Feb 14 is 01-01 and 02-14


# === Climate Data plots and analysis   ===================



#clim.period <- c("05-01", "09-30")
#period.name <- c("May-Sept")
clim.period <- c("05-01", "09-30")
period.name <- c("May-Sept")
clim.years <- unique(clim.d$Year)


# changed the period to be between may and September because that is when they are out on
# the range. Other times of year they are hibernating 


# make an empty data frame
clim.period.select <- clim.d[1,]
str(clim.period.select)
clim.period.select <- clim.period.select[-1,]

# vector to store missing values of mean Temp
missing.mean.temp <- cbind(rep(NA, length(clim.years)), 
                           rep(NA, length(clim.years)))

colnames(missing.mean.temp)<- c("Year", "n.NA")

# go through each year and select the data from the period
for(i in 1:length(clim.years)){
  
  # i <- 1
  d.t <- clim.d[clim.d$Year == clim.years[i],]
  date.start <- as.Date(paste(clim.years[i],"-", clim.period[1], sep =""))
  date.end <- as.Date(paste(clim.years[i],"-", clim.period[2], sep =""))
  
  #make selection for specific time period
  d.t.s <- d.t[d.t$Date.Time >= date.start & d.t$Date.Time <= date.end,]
  
  #get number of missing values
  missing.mean.temp[i, 1] <- clim.years[i]
  missing.mean.temp[i, 2] <- length(which(is.na(d.t.s$Mean.Temp.C) == T))
  
  clim.period.select <- rbind.data.frame(clim.period.select, d.t.s)
}

#calculate the mean of the mean temp for the selected period for each years
# some years have NA values, these are omitted.
mean.temp.per.period <- aggregate(clim.period.select$Mean.Temp.C, list(clim.period.select$Year), mean, 
                                  na.rm = T)
colnames(mean.temp.per.period) <- c("Year", "Mean.Period.Temp.C")

# to check if number of NA's is not a problem check missing.mean.temp
missing.mean.temp

# temperature change over time plot 


pdf(paste(p.fig, "Osoyoos.Summer.Temp.", period.name, ".pdf",sep=""), 
    width = 6, height = 5)

ggplot(mean.temp.per.period, aes(x= Year , y= Mean.Period.Temp.C )) +
  geom_point() +
  ylim(18, 22) +
  geom_smooth(method=lm, se=FALSE, col='black', size=.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title = "Osoyoos  Temperatures", y = "Average Summer Temp (C)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

#####  statisitcs - Is this enough or 
summary(lm(mean.temp.per.period$Mean.Period.Temp.C ~ mean.temp.per.period$Year))




# precipitation through time 
## Is this the mean precip per day? 
mean.precip.per.period <- aggregate(clim.period.select$Total.Precip.mm, list(clim.period.select$Year), mean, 
                                    na.rm = T)
colnames(mean.precip.per.period) <- c("Year", "Total.Precip.mm")


pdf(paste(p.fig, "Osoyoos.Summer.precip", ".pdf",sep=""), 
    width = 6, height = 5)

ggplot(mean.precip.per.period, aes(x= Total.Precip.mm , y= Year )) +
  geom_point() +
  ylim(0, 3) +
  geom_smooth(method=lm, se=FALSE, col='black', size=.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title = "Osoyoos Summer Precip", y = "Average Summer Precip", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5))


dev.off()

# warning message here - investigate 
# confused if this is the precip per day averaged out? 


### Statistics 
summary(lm(mean.precip.per.period$Year ~ mean.precip.per.period$Total.Precip.mm))




# === Looking at distribution of  length/mass data   ===================

## Sage - save these for appendix for distribution of the data 
hist.mass <- ggplot(d.c, aes(x=mass)) + geom_histogram(binwidth = 4) +
  xlim(0, 750)

hist.length <-  ggplot(d.c, aes(x=length)) + geom_histogram(binwidth=1) +
  xlim(0, 150)
# Warning messages:
# 1: Removed 620 rows containing non-finite values (stat_bin). 
# 2: Removed 2 rows containing missing values (geom_bar). 



# === Looking at different variables through time   ===================

 mass.length  <- d.c %>% 
  arrange(-year) %>% 
  select(catch.date, length, mass)


mass.length$catch.date <- format(as.Date(d.c$catch.date, format = "%Y- %m - %d"), "%Y")
 mass.length[mass.length==0] <- NA

 mass.length <- na.omit(mass.length)

 mass.length$BMI <- mass.length$length/mass.length$mass

mass.length

# doing it in ggplot2! 


### maybe delete 
# all snakes 
BMI.all.plot <- ggplot(data=mass.length, mapping=aes(x=catch.date, y= BMI)) + 
  coord_cartesian(ylim = c(0, 6)) +
  geom_boxplot()  +
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))




snake.variables  <- d.c %>% 
  #arrange(-year) %>% 
  select(catch.date, year, mass, length, sex, age.class)

str(snake.variables)
#snake.variables$catch.date.year <- format(as.Date(d.c$catch.date, format = "%Y- %m - %d"), "%Y")
# aggregate(snake.variables$year, list(snake.variables$year),leng)


# if there are zeros, make them NA 
snake.variables[snake.variables==0] <- NA

# Omit NAs 
snake.variables <- na.omit(snake.variables)

#creating BMI
snake.variables$BMI <- snake.variables$length/snake.variables$mass

str(snake.variables)

BMI.points <- aggregate(snake.variables$year, list(snake.variables$year), BMI)


# grouped by sex 
### Maybe delete 

BMI.sex.plot <-  ggplot(data=snake.variables, mapping=aes(x=year, y= BMI, fill = sex)) + 
  geom_boxplot()  +
  ylim(0, 6) +
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))


# now we can see more males and females in the data but the resolution is 
# It looks as through the average size and weight of snakes is increasing
# but likely this is due to the fact that the survey effort has increased over the year
# and more and more large snakes are being caught 



# grouped by age 
BMI.age.plot <- ggplot(data=snake.variables, mapping=aes(x=year, y= BMI, fill = age.class)) + 
  geom_boxplot()  +
  ylim(0, 5) +
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))




# === BMI changes through time  ===================
# first: check if BMI changes through the season for each year

# check number of BMI available 
aggregate()

aggregate(snake.variables$BMI, list(snake.variables$year), length)
# aaah very few BMI available....
aggregate(snake.variables$length, list(snake.variables$year), length)
# very few lengths
aggregate(snake.variables$mass, list(snake.variables$year), length)
# very few mass



### creating figures that show BMI change each year 

n.years <- unique(snake.variables$year)
for(i in 1:length(n.years)){
  print(n.years[i])
  #i <- 1
  d.t <- snake.variables[snake.variables$year == n.years[i],]
  d.t$catch.date <- as.Date(d.t$catch.date)
  
  print(head(d.t))
  str(d.t)
  
  pdf(paste(p.fig, "BMI.per.year.", n.years[i],".pdf",sep=""), 
      width = 5, height = 5)
  plot(d.t$catch.date, d.t$BMI, main = n.years[i])
  abline(lm(d.t$BMI ~ d.t$catch.date))
  dev.off()
}



# === Ratio of male and female through time   ===================


### Maybe delete as we only care about neonates basically 
# plot looking at the ratio of male and female snakes per year 

sex.year <- ggplot(snake.variables, aes(fill = snake.variables$sex, x = snake.variables$year)) + 
          geom_bar(position="dodge")

### add trendline for each? 


# need to plot this with just neonates over the years and use a ratio in 
# relation to population of snakes
unique(snake.variables$age.class)
snake.variables.Neo <- snake.variables[snake.variables$age.class == "neonate", ]

# When I look at snake.variables.Neo there is only data from 2007, and 2016-2022 why?


unique(snake.variables.Neo$age.class) # should be only neonates


neonate.year <- ggplot(snake.variables.Neo, aes(fill = snake.variables.Neo$sex, x = snake.variables.Neo$year)) + 
  geom_bar(position="dodge") 

### now plot against temperature 


## need to do an analysis here 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# === climate vs body condition  ===================

# need to find body condition average per year but there isnt many BMI values 
# I am just going to use mass for now 


mean.mass.per.period <- aggregate(snake.variables$mass, list(snake.variables$year), mean, 
                                    na.rm = T)
#renaming column to avoid confusion 
mean.mass.per.period <- rename(mean.mass.per.period  
                   , mass = x
                  , year = Group.1) 
# year 
mean.temp.per.period

# deleteing 2012/2008 because its not in the mass data and its confusing things 
 mean.temp.per.period <- mean.temp.per.period[-8,]
 
# adding temperature to the mass data frame  
mean.mass.per.period$temp <- mean.temp.per.period$Mean.Period.Temp.C


# okay seeing if I can plot these together 


pdf(paste(p.fig, "mass.temp", ".pdf",sep=""), 
    width = 6, height = 5)


ggplot(mean.mass.per.period, aes(x= temp , y= mass )) +
  geom_point() +
  
  geom_smooth(method=lm,  se = FALSE, col='black', size=.5) + 
  ylim(120, 250) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs( y = "Average Mass Per Season", x = "Average Temp per Season") 



dev.off()

### statistics - Probably needs to be better 

summary(lm(mean.mass.per.period$temp ~ mean.mass.per.period$mass))



# now I want to also look at precipitation 


mean.precip.per.period 


# taking out the years that dont match 

mean.precip.per.period  <- mean.precip.per.period [-8,]

# adding precip to the other data frame 
mean.mass.per.period$precip <- mean.precip.per.period$Total.Precip.mm


# now plotting 

pdf(paste(p.fig, "mass.precip", ".pdf",sep=""), 
    width = 6, height = 5)

ggplot(mean.mass.per.period, aes(x= precip , y= mass )) +
  geom_point() +
  
  geom_smooth(method=lm,  col='black', se = FALSE, size=.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title = " Average Summer Mass and Temperature ", y = "Average Mass Per Season", x = "Average Precip per Season") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

## error message coming up here as well 

### Statistics - needs to be better 

summary(lm(mean.mass.per.period$precip ~ mean.mass.per.period$mass))



# === birth rate vs temp/===================

# here I want to show the relationship between temperature and birth rate 
# (number of neonatse per year. Probably neonates need to be as a ratio of all snakes to account for
# sampling errors or years where there were more caught than others)


str(d.c)
age.cl.t <- unique(d.c$age.class)
years.t <- unique(d.c$year)

# 2011 and 2010 are in the wrong order?

#aggregate(d.c$age.class, list(d.c$year, d.c$age.class), length)
# doesn't quite do what I want, as I also like to know # NAs

birth.rate <- as.data.frame(matrix(NA, ncol = 8, nrow = length(years.t)))
colnames(birth.rate) <- c("year","n", "NA","adult","sub.adult","juvenile",
                          "neonate", "ratio")
str(birth.rate)

for(i in 1:length(years.t)){
  # i <- 1
  # make a selection of data set per year
  d.c.t <- d.c[d.c$year ==  years.t[i],]
  nrow(d.c.t)
  birth.rate$year[i] <- years.t[i]  #store the year
  birth.rate$n[i] <- nrow(d.c.t)
  # get the numbers per group incl NAs
  birth.rate[i,3] <- length(which(is.na(d.c.t$age.class)))
  for(j in 1:(length(age.cl.t) - 1)){
    birth.rate[i, j+3] <- length(which(d.c.t$age.class == age.cl.t[j]))
  }
  # calculate the proportion neonates with respect to all classes 
  birth.rate$ratio[i] <- birth.rate$neonate[i]/ sum(birth.rate[i, 4:7])
}

birth.rate$year <- as.numeric(birth.rate$year)
# change of ratio over time




# plot in ggplot 

pdf(paste(p.fig, "Birth.Rate", ".pdf",sep=""), 
    width = 6, height = 5)

ggplot(birth.rate, aes(x= year , y= ratio )) +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title = " Birth Rate Through Time ", y = "Ratio of neonates to all Snakes", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) 

dev.off()

### statistics 

# quick and dirty analysis
m1 <- lm(birth.rate$ratio ~ birth.rate$year)
summary(m1)
str(birth.rate)
abline(m1)




# looking at temperature VS birth rate 

####WEIRD MIX OF VARIABLES!! ##

birth.rate2 <- birth.rate
birth.rate2
colnames(birth.rate2)[1] <- "Year"

temp.ratio <- merge(mean.temp.per.period, birth.rate2, by = "Year")
str(mean.temp.per.period)
str(birth.rate)

# plotting temperature vs birth Rate 

pdf(paste(p.fig, "Birthrate.Temp", ".pdf",sep=""), 
    width = 6, height = 5)

ggplot(temp.ratio, aes(x= Mean.Period.Temp.C , y= ratio )) +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y = "Ratio of neonates to all Snakes", x = "Temperature") 

dev.off()



# statistics summary 
summary(lm(temp.ratio$ratio ~ temp.ratio$Mean.Period.Temp.C))



### do birth rate vs precipitation next 




# === sex ratio vs temp  ===================
# this would be the ratio of male and female 










# === Looking at different variables through time  ===================

mass.length  <- d.c %>% 
  #arrange(-year) %>% 
  select(catch.date, length, mass)


mass.length$catch.date <- format(as.Date(d.c$catch.date, format = "%Y- %m - %d"), "%Y")

mass.length[mass.length==0] <- NA

mass.length <- na.omit(mass.length)

mass.length$BMI <- mass.length$length/mass.length$mass

mass.length

# doing it in ggplot2! 

# all snakes 
BMI.all.plot <- ggplot(data=mass.length, mapping=aes(x=catch.date, y= BMI)) + 
  coord_cartesian(ylim = c(0, 6)) +
  geom_boxplot()  +
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))



snake.variables  <- d.c %>% 
  #arrange(-year) %>% 
  select(catch.date, year, mass, length, sex, age.class)

str(snake.variables)
#snake.variables$catch.date.year <- format(as.Date(d.c$catch.date, format = "%Y- %m - %d"), "%Y")
# aggregate(snake.variables$year, list(snake.variables$year),leng)


snake.variables[snake.variables==0] <- NA

snake.variables <- na.omit(snake.variables)

snake.variables$BMI <- snake.variables$length/snake.variables$mass

snake.variables



# grouped by sex 

BMI.sex.plot <-  ggplot(data=snake.variables, mapping=aes(x=year, y= BMI, fill = sex)) + 
  geom_boxplot()  +
  ylim(0, 6) +
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))


# now we can see more males and females in the data but the resolution is 
# It looks as through the average size and weight of snakes is increasing
# but likely this is due to the fact that the survey effort has increased over the year
# and more and more large snakes are being caught 



# grouped by age 
BMI.age.plot <- ggplot(data=snake.variables, mapping=aes(x=year, y= BMI, fill = age.class)) + 
  geom_boxplot()  +
  ylim(0, 5) +
  geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))




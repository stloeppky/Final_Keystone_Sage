

# === description ==============================================================

# Final Analysis and plots for Keystone 
# === TO DO ==============================================================

# 1) Make sure the body condition is only for female snakes 
# 2) gravid females? 
# 3) make sure variable names are spelled incl capitalized the same 



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

#the amount of  length data points per year
point.per.year <- aggregate(d.c$year, list(d.c$year), length)


# === Create sub-data frame from which to do analysis  ===================

# choosing just these variables as "Repro" contains many NAs and is difficult to handle 

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

#creating BMI which is length devided by mass for now 
snake.variables$BMI <- snake.variables$length/snake.variables$mass

str(snake.variables)

# Checking number of BMI per year 
BMI.points <- aggregate(snake.variables$BMI, list(snake.variables$year), length)


# === Read in cleaned climate data  ===================

# read the cleaned climate data

clim.d <- read.csv(paste(p.data.clean, "climate.years.all.clean.csv", sep = ""))
str(clim.d)
clim.d <- clim.d[,-1]
clim.d$Date.Time <- as.Date(clim.d$Date.Time)


# === Climate Data plots and analysis   ===================

clim.period <- c("05-01", "09-30")
period.name <- c("May-Sept")
clim.years <- unique(clim.d$Year)

# changed the period to be between may and September because that is when they are on
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
  labs( y = "Average Summer Temp (C)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

####  STATISTICS ###-

# intercept only
m0 <- lm(mean.temp.per.period$Mean.Period.Temp.C ~ 1)
# intercept and slope
m1 <- lm(mean.temp.per.period$Mean.Period.Temp.C ~ mean.temp.per.period$Year)
# intercept + slope + 'curvature'(exponential)
# let's look aat the fit of a higher order (non linear) relationship
m2 <- lm(mean.temp.per.period$Mean.Period.Temp.C ~ 
             mean.temp.per.period$Year + I(mean.temp.per.period$Year^2))
# assess the outcomes
summary(m0)
summary(m1)
anova(m0, m1) # compare model m0 with m1 -? does the slope significantly 
# contributes to explaining the variance in the data
summary(m2)
anova(m1, m2)



#### Precipitation through time

# Mean precip 
mean.precip.per.period <- aggregate(clim.period.select$Total.Precip.mm, list(clim.period.select$Year), mean, 
                                    na.rm = T)
colnames(mean.precip.per.period) <- c("Year", "Total.Precip.mm")

# weird result as 2008 has no precip.....
clim.period.select$Total.Precip.mm[clim.period.select$Year == 2008]
# let's remove 2008
mean.precip.per.period <- 
  mean.precip.per.period[-which(mean.precip.per.period$Year==2008),]

pdf(paste(p.fig, "Osoyoos.Summer.precip", ".pdf",sep=""), 
    width = 6, height = 5)

ggplot(mean.precip.per.period, aes(x= Year , y= Total.Precip.mm )) +
  geom_point() +
  ylim(0, 3) +
  geom_smooth(method=lm, se=FALSE, col='black', size=.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs( y = "Average Summer Precip (mm) ", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5))


dev.off()


### Statistics 
## !! SAGE check why we have an NA

mp0 <- lm(mean.precip.per.period$Year ~ 1)
mp1 <- lm(mean.precip.per.period$Year ~ mean.precip.per.period$Total.Precip.mm)
mp2 <- lm(mean.precip.per.period$Year ~ mean.precip.per.period$Total.Precip.mm +
            I(mean.precip.per.period$Total.Precip.mm^2))

is.na(mean.precip.per.period$Year) # no NAs - not sure why there was before - odd 
is.na(mean.precip.per.period$Total.Precip.mm)
anova(mp0, mp1)
anova(mp1, mp2)

summary(mp1)

# === TO DO Relative body condition  ====================================================================
# across all year combined, look at the correlation between
# body length and mass. The idea is that on a poor year, the mass is relative
# lower for a given length than in a good year.
# do regression of length on mass and take residual.
# SAGE: log transformed residuals used in most studies.





# === Temperature Vs Body Condition of FEMALE snakes  ====================================================================

# creating mean masses for each year 
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


# plotting together 

pdf(paste(p.fig, "mass.temp", ".pdf",sep=""), 
    width = 6, height = 5)


ggplot(mean.mass.per.period, aes(x= temp , y= mass )) +
  geom_point() +
  
  geom_smooth(method=lm,  se = FALSE, col='black', size=.5) + 

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs( y = "Average Mass Per Season (g)", x = "Average Summer Temp (C)") 


dev.off()


### statistics - Probably needs to be better 

summary(lm(mean.mass.per.period$temp ~ mean.mass.per.period$mass))




### now just female mass 

mass.female <- snake.variables[snake.variables$sex == "female", ]

mean.mass.female <- aggregate(mass.female$mass, list(mass.female$year), mean, 
                                  na.rm = T)
mean.mass.female <- rename(mean.mass.female 
                               , mass = x
                               , year = Group.1) 


# adding temperature to the mass data frame  
mean.mass.female$temp <- mean.temp.per.period$Mean.Period.Temp.C

ggplot(mean.mass.female, aes(x= temp , y= mass )) +
  geom_point() +
  
  geom_smooth(method=lm,  se = FALSE, col='black', size=.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs( y = "Mean Female Mass Per Season (g)", x = "Average Summer Temp per year  (C)") 



summary(lm(mean.mass.female$temp ~ mean.mass.female$mass))



# === Precipitation Vs Body Condition of FEMALE snake   ====================================================================


mean.precip.per.period 

# taking out the years that dont match 
# mean.precip.per.period  <- mean.precip.per.period [-8,]

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
  labs(title = " Average Summer Mass and Temperature ", y = "Average Mass Per Summer (g)", x = "Average Precip per Summer (mm)") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

## error message coming up here as well 

### Statistics - needs to be better 
summary(lm(mean.mass.per.period$precip ~ mean.mass.per.period$mass))




### now just the females 

mean.mass.female$precip <- mean.precip.per.period$Total.Precip.mm

ggplot(mean.mass.female, aes(x= precip , y= mass )) +
  geom_point() +
  
  geom_smooth(method=lm,  se = FALSE, col='black', size=.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs( y = "Average Female Mass Per Summer (g)", x = "Average Precip per Summer (mm)") 



summary(lm(mean.mass.female$precip ~ mean.mass.female$mass))



# === Temperature Vs. Birth Rate    ====================================================================

str(d.c)
age.cl.t <- unique(d.c$age.class)
years.t <- unique(d.c$year)


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


## plot in ggplot ##

pdf(paste(p.fig, "Birth.Rate", ".pdf",sep=""), 
    width = 6, height = 5)

ggplot(birth.rate, aes(x= year , y= ratio )) +
  geom_point() + 
  
  geom_smooth(method=lm,  se = FALSE, col='black', size=.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title = " Birth Rate Through Time ", y = "Ratio of neonates to all Snakes", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) 

dev.off()

### STATISTICS ###

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
  
  geom_smooth(method=lm,  se = FALSE, col='black', size=.5) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y = "ratio of neonates to all snakes", x = "Mean Temperature per Summer (C)") 

dev.off()


### STATISTICS ###

summary(lm(temp.ratio$ratio ~ temp.ratio$Mean.Period.Temp.C))

# === Temperature Vs. Birth Rate    ====================================================================



### birth rate and precipitation 

temp.ratio1 <- temp.ratio[-4,]

mean.precip <- mean.precip.per.period[-8,]

temp.ratio1$precip <- mean.precip$Total.Precip.mm


ggplot(temp.ratio1, aes(x= precip , y= ratio )) +
  geom_point() + 
  
  geom_smooth(method=lm,  se = FALSE, col='black', size=.5) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y = "ratio of neonates to all snakes", x = "Mean precip per Summer (mm)") 







# === Temperature Vs. Sex Ratio    ====================================================================

### this is just neonates through time... need to put against temperature 

# need to plot this with just neonates over the years and use a ratio in 
# relation to population of snakes
unique(snake.variables$age.class)
which(snake.variables$age.class == "neonate")
snake.variables.Neo <- snake.variables[snake.variables$age.class == "neonate", ]
unique(snake.variables.Neo$age.class) # should be only neonates

nrow(snake.variables.Neo)
# When I look at snake.variables.Neo there is only data from 2007, and 2016-2022 why?

## TV there are only 227 neonate entries it looks like. And these are only in a 
aggregate(snake.variables.Neo$sex, list(snake.variables.Neo$year), length) 
# Group.1  x
# 1    2007  2
# 2    2016  8
# 3    2017  5
# 4    2018 10
# 5    2019 27
# 6    2020 62
# 7    2021 75
# 8    2022 38

# this is not amazing as we only have one average spring temperature per year
# so number of years is our sample unit. 2 indv in 2007 is next to nothing so 
# better discard. 5 is pretty low but let's keep it in for now.

# let's add temperature to the snake.variables.Neo
str(snake.variables.Neo)
temp.t <- mean.temp.per.period
colnames(temp.t)[1] <- "year"
str(temp.t)
sex.ratio <- merge(snake.variables.Neo, temp.t, by = "year", all.x = T)
head(sex.ratio)
# remove 2007
sex.ratio <- sex.ratio[-which(sex.ratio$year == 2007),]
# sex ratio is proportion males
# make new column and turn males to 1 and females to value 0
sex.ratio$sex.num <- sex.ratio$sex
sex.ratio$sex.num[sex.ratio$sex.num == "male"] <- 1
sex.ratio$sex.num[sex.ratio$sex.num == "female"] <- 0
sex.ratio$sex.num <- as.integer(sex.ratio$sex.num)

m.sex <- glm(sex.ratio$sex.num ~ sex.ratio$Mean.Period.Temp.C, 
             family = binomial)
summary(m.sex)

plot(jitter(sex.ratio$Mean.Period.Temp.C, amount = 0.02), 
     jitter(sex.ratio$sex.num,amount = 0.05), pch = 19)


temp.range <- range(sex.ratio$Mean.Period.Temp.C)
x.pr <- seq(temp.range[1], temp.range[2], 0.01)
y.pred <- predict(m.sex,list(Mean.Period.Temp.C = x.pr), type = "response" )
points(sex.ratio$Mean.Period.Temp.C, y.pred, col = "red")

# let's get the proportion males per year
year.sex <- unique(sex.ratio$year)
sex.prob <- as.data.frame(matrix(NA, nrow = length(year.sex), ncol = 3))
colnames(sex.prob) <- c("year", "temp", "prop.males" )
for(i in 1:length(year.sex)){
  #i <- 1
  sex.t <- sex.ratio[which(sex.ratio$year == year.sex[i]),]
  sex.prob$year[i] <- year.sex[i]
  sex.prob$temp[i] <- sex.t$Mean.Period.Temp.C[1]
  n.males <- sum(sex.t$sex.num)
  n.tot <- nrow(sex.t)
  sex.prob$prop.males[i] <- n.males/n.tot
}

plot(jitter(sex.ratio$Mean.Period.Temp.C, amount = 0.02), 
     jitter(sex.ratio$sex.num,amount = 0.08), pch = 19, cex = .7)
points(sex.prob$temp, sex.prob$prop.males, pch = 19,col = "blue")


# make figure
 ggplot(snake.variables.Neo, aes(fill = snake.variables.Neo$sex, x = snake.variables.Neo$year)) + 
  geom_bar(position="dodge") +  scale_fill_grey() + guides(fill=guide_legend(title="Sex")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Count" , x = "Year") 




# ratio of male and female neonates through time 




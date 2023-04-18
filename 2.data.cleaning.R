
# === description ===========================================================================
## brief description what the script file is about
# 1) there many files from different years, all with their own problems
# 2) read each at a time and fix it to match standard names etc

## if you have clear chunks of code, use collapsible dividers (# ==== or # ----)
## and number the sections. Provide brief description for each section here
## # ***** or #_____ does not make sections 


# ===== TO DO ====


# 1) LINE 976 ish: NA s introduced by coercion - look at this and trouble shoot. 
#                  
# 2) LINE 1000 ish: Remove the 0 in length (and also mass which also has negative numbers that need to be removed). 
#                   We probably want to do this all at the end? 

#3) LINE 1170: FALSE needs to be corrected. It is being used for both males and females. 
#              For males needs to be "NA", for females needs to be "non-gravid"

#4) OUTLIERS: Need to finish using the z-score method to remove outliers from this data set 

#5) SPECIES: If there is time correct some of the more minor typos where they say age class instead 
#            of species so I can 



# ================

# Notes about naming: 
# d. = dataframe 
# .c at the end = cleaned 
# .t in the name = temporary? 

# standard names for variables in the cleaned data files
# catch.date    yyyyymmdd 
# person        person who caught 
# species       rattle, gopher, racer, garter etc 
# PIT.tag       ID number for snake 
# length        tip of head till vent while following body in cm
# mass          mass measured in grams
# age.class     Neonate, Juvenile, Sub-adult, Adult (usually determined by length)
# sex           male or female 
# repro         Reproductive status: Either gravid or non-gravid (not relevant for males)
# ova.count     If gravid the number of Ova felt inside the snake 
# rattle        How many segments to the rattle (can be used as a proxy for age with limitations)
# air.temp      Temp of the air as measured by a Kestrel 


# empty data frame
# d.template <- data.frame( catch.date = character(),
#                         person = character(),
#                         species = character(),
#                         PIT.tag = character(),
#                         length = numeric(),
#                         mass = numeric(),
#                         age.class = character(),
#                         repro = character(),
#                         air.temp = numeric())

# ******************************************************************************
## use descriptive names 
# 1) read raw data: check 
# 2) Combine into 1 large data frame
# 3) Fix typos etc 
# 4) outliers: test for outliers and entry errors/typos -> fix or make notes about these 
# 5) Send to clean data folder 

# === 1) read and check =========================================================

list.files(paste(p.data.raw, "capture.2005-2022", sep="" ))

# make path to new data raw location which is in a subfolder
# show all of the files 
p.data.temp <- paste(p.data.raw, "capture.2005-2022", sep="" )
all.years <-list.files(p.data.temp)

# ===  2005 ===== 


file.2005 <- 1 # This is the first file in the 2005 - 2022 so that's how it know... 
d.2005 <- read.csv(paste(p.data.temp, all.years[file.2005], sep="/"))
head(d.2005)
str(d.2005)
#selecting the variables I want 

d.2005.c  <- d.2005 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, Species, PIT.Tag.., SVL, Mass, Age.class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2005.c)
str(d.2005.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


 d.2005.c <- rename(d.2005.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = Date
                   , person = Researcher 
                   , species = Species 
                   , PIT.tag = PIT.Tag.. 
                   , length = SVL 
                   , mass = Mass 
                   , age.class = Age.class 
                   , sex = Sex  
                   , repro = Gravid 
                   , air.temp = Ta )
str(d.2005.c)

# going to try to just fix the date data for 2005 

d.2005.c$catch.date.t <- d.2005.c$catch.date
str(d.2005.c)

# TEMP 
#using as.Date 
d.2005.c$catch.date.t <- as.Date(d.2005.c$catch.date.t, format="%d/%m/%Y")
class(d.2005.c$catch.date.t)

# okay this worked so I'm going to change the actual data column

d.2005.c$catch.date <- as.Date(d.2005.c$catch.date, format="%d/%m/%Y")
class(d.2005.c$catch.date)

# Now to delete the catch.date.t column before 

d.2005.c <- subset (d.2005.c, select = -catch.date.t)
# checking 
str(d.2005.c)

# ===  2006 =====
file.2006 <- 2
d.2006 <- read.csv(paste(p.data.temp, all.years[file.2006], sep="/"))
head(d.2006)
str(d.2006)

#selecting the variables I want 

d.2006.c  <- d.2006 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, Species, PIT.Tag.., SVL, Mass, Age.class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be wierd when I try to make one large dataframe with all the years? 

head(d.2006.c)
str(d.2006.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 

 
 d.2006.c <- rename(d.2006.c  # used the same name for the new data frame so I overwrote the last one
         , catch.date = Date
         , person = Researcher 
         , species = Species
         , PIT.tag = PIT.Tag.. 
         , length = SVL 
         , mass = Mass 
         , age.class = Age.class 
         , sex = Sex  
         , repro = Gravid 
         , air.temp = Ta 
         )
 str(d.2006.c)

 # changing date to be correct format for R 
 
 d.2006.c$catch.date <- as.Date(d.2006.c$catch.date, format="%d/%m/%Y")
 
 class(d.2006.c$catch.date)

# ===  2007 =====
file.2007 <- 3
d.2007 <- read.csv(paste(p.data.temp, all.years[file.2007], sep="/"))
head(d.2007)
str(d.2007)

#selecting the variables I want 

d.2007.c  <- d.2007 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, Spp, PIT.Tag.., SVL, Mass, Age.class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2007.c)
str(d.2007.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2007.c <- rename(d.2007.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = Date
                   , person = Researcher 
                   , species = Spp 
                   , PIT.tag = PIT.Tag.. 
                   , length = SVL 
                   , mass = Mass 
                   , age.class = Age.class 
                   , sex = Sex  
                   , repro = Gravid 
                   , air.temp = Ta )
str(d.2007.c)

#changing date to correct format 

d.2007.c$catch.date <- as.Date(d.2007.c$catch.date, format="%m/%d/%Y")
class(d.2007.c$catch.date)

# ===  2008 =====
file.2008 <- 4
d.2008 <- read.csv(paste(p.data.temp, all.years[file.2008], sep="/"))
head(d.2008)
str(d.2008)


#selecting the variables I want 

d.2008.c  <- d.2008 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, Spp, PIT.Tag.., SVL, Mass, Age.class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2008.c)
str(d.2008.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2008.c <- rename(d.2008.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = Date
                   , person = Researcher 
                   , species = Spp 
                   , PIT.tag = PIT.Tag.. 
                   , length = SVL 
                   , mass = Mass 
                   , age.class = Age.class 
                   , sex = Sex  
                   , repro = Gravid 
                   , air.temp = Ta )
str(d.2008.c)

#changing date to correct format 

d.2008.c$catch.date <- as.Date(d.2008.c$catch.date, format="%m/%d/%Y")
class(d.2008.c$catch.date)


# ===  2009 =====

file.2009 <- 5
d.2009 <- read.csv(paste(p.data.temp, all.years[file.2009], sep="/"))
head(d.2009)
str(d.2009)

# just noticed that 1/2 of this data says 2008 but it is unlcear if that is a typo or actuality. 
# need to figure out a way to find out? 

#selecting the variables I want 

d.2009.c  <- d.2009 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, Spp, PIT.Tag.., SVL, Mass..g., Age.class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2009.c)
str(d.2009.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2009.c <- rename(d.2009.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = Date
                   , person = Researcher 
                   , species = Spp 
                   , PIT.tag = PIT.Tag.. 
                   , length = SVL 
                   , mass = Mass..g.
                   , age.class = Age.class 
                   , sex = Sex  
                   , repro = Gravid 
                   , air.temp = Ta )
str(d.2009.c)


#changing date to correct format 

d.2009.c$catch.date <- as.Date(d.2009.c$catch.date, format="%m/%d/%Y")
class(d.2009.c$catch.date)


# ===  2010 ===== 
file.2010 <- 6
d.2010 <- read.csv(paste(p.data.temp, all.years[file.2010], sep="/"))
head(d.2010)
str(d.2010)

#selecting the variables I want 

d.2010.c  <- d.2010 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, Spp, PIT.Tag.., SVL, Mass, Age.class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2010.c)
str(d.2010.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2010.c <- rename(d.2010.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = Date
                   , person = Researcher 
                   , species = Spp 
                   , PIT.tag = PIT.Tag.. 
                   , length = SVL 
                   , mass = Mass
                   , age.class = Age.class 
                   , sex = Sex  
                   , repro = Gravid 
                   , air.temp = Ta )
str(d.2010.c)


# Thor - there are two different kinds of dates within this one year.. Ugh. Going to circle back to this 
# one later 

## let's try to use lubridate library
# library(lubridate)

# make copy to play with
dates.2010.t <- d.2010.c$catch.date
d.2010 <- as.Date(parse_date_time(dates.2010.t, c('%d%m%y')))
#compare with the 'original'
check.2010<- cbind.data.frame(d.2010.c$catch.date,d.2010)

# looks good so implement it
# use Sage her style: I think you can change all date tranformations to this
# format below using lubridate()
d.2010.c$catch.date <- as.Date(parse_date_time(d.2010.c$catch.date, c('%d%m%y')))


# ===  2011 =====
file.2011 <- 7
d.2011 <- read.csv(paste(p.data.temp, all.years[file.2011], sep="/"))
head(d.2011)
str(d.2011)

#selecting the variables I want 

d.2011.c  <- d.2011 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, Spp, PIT.Tag.., SVL, Mass, Age.class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2011.c)
str(d.2011.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2011.c <- rename(d.2011.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = Date
                   , person = Researcher 
                   , species = Spp 
                   , PIT.tag = PIT.Tag.. 
                   , length = SVL 
                   , mass = Mass
                   , age.class = Age.class 
                   , sex = Sex  
                   , repro = Gravid 
                   , air.temp = Ta )
str(d.2011.c)

#changing date to correct format - commented this out because 2011 is being represented by 11 so I need 
# to figure out how to get R to make it into a 4 digit year 

# d.2011.c$catch.date <- as.Date(d.2011.c$catch.date, format="%m/%d/%Y")
# class(d.2011.c$catch.date)

#install.packages("hydrostats")
#library(hydrostats)



# change the date and time
d.2011.c$catch.date <- as.Date(parse_date_time(d.2011.c$catch.date, 
                                               c('%d%m%y', '%m%d%y')))
head(d.2011.c)





# ===  2012 =====

## The file for this year isn't in the files  - Lindsay doesn't have this year so I guess that will 
# just be missing data 
# ===  2013 =====
file.2013 <- 8
d.2013 <- read.csv(paste(p.data.temp, all.years[file.2013], sep="/"))
head(d.2013)
str(d.2013)

#selecting the variables I want 

d.2013.c  <- d.2013 %>% 
  #arrange(-year) %>% 
  select(Date, Researcher, SPECIES, PIT_Tag_., SVL, Mass, Age_class, Sex, Gravid, Ta)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2013.c)
str(d.2013.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2013.c <- rename(d.2013.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = Date
                   , person = Researcher 
                   , species = SPECIES
                   , PIT.tag = PIT_Tag_.
                   , length = SVL 
                   , mass = Mass
                   , age.class = Age_class 
                   , sex = Sex  
                   , repro = Gravid 
                   , air.temp = Ta
                   )
str(d.2013.c)

# changing date to correct format 
d.2013.c$catch.date <- as.Date(d.2013.c$catch.date, format="%d/%m/%Y")
class(d.2013.c$catch.date)


# ===  2014 =====

file.2014 <- 9 
d.2014 <- read.csv(paste(p.data.temp, all.years[file.2014], sep="/"))
head(d.2014)
str(d.2014)



#selecting the variables I want 

d.2014.c  <- d.2014 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT_TAG_., SVL, MASS, AGE_CLASS, SEX, GRAVID, AIR_TEMP)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2014.c)
str(d.2014.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2014.c <- rename(d.2014.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT_TAG_.
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE_CLASS
                   , sex = SEX  
                   , repro = GRAVID
                   , air.temp = AIR_TEMP)
str(d.2014.c)


# changing date to correct format 
d.2014.c$catch.date <- as.Date(d.2014.c$catch.date, format="%m/%d/%Y")
class(d.2014.c$catch.date)




# ===  2015 =====

file.2015 <- 10
d.2015 <- read.csv(paste(p.data.temp, all.years[file.2015], sep="/"))
head(d.2015)
str(d.2015)

#selecting the variables I want 

d.2015.c  <- d.2015 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, GRAVID, AIR.TEMP)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2015.c)
str(d.2015.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2015.c <- rename(d.2015.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = GRAVID
                   , air.temp = AIR.TEMP)
str(d.2015.c)


# changing date to correct format 
d.2015.c$catch.date <- as.Date(d.2015.c$catch.date, format="%m/%d/%Y")
class(d.2015.c$catch.date)

# ===  2016 =====
file.2016 <- 11
d.2016 <- read.csv(paste(p.data.temp, all.years[file.2016], sep="/"))
head(d.2016)
str(d.2016)



#selecting the variables I want 

d.2016.c  <- d.2016 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, REPRO, AIR.TEMP)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2016.c)
str(d.2016.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


 d.2016.c <- rename(d.2016.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = REPRO
                   , air.temp = AIR.TEMP) 
 
str(d.2016.c)

# changing date to correct format 
d.2016.c$catch.date <- as.Date(d.2016.c$catch.date, format="%m/%d/%Y")
class(d.2016.c$catch.date)

# ===  2017 =====
file.2017 <- 12
d.2017 <- read.csv(paste(p.data.temp, all.years[file.2017], sep="/"))
head(d.2017)
str(d.2017)



#selecting the variables I want 

d.2017.c  <- d.2017 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, REPRO, AIR.TEMP)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2017.c)
str(d.2017.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2017.c <- rename(d.2017.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = REPRO
                   , air.temp = AIR.TEMP) 

str(d.2017.c)


# changing date to correct format 
d.2017.c$catch.date <- as.Date(d.2017.c$catch.date, format="%m/%d/%Y")
class(d.2017.c$catch.date)

# ===  2018 =====
file.2018 <- 13
d.2018 <- read.csv(paste(p.data.temp, all.years[file.2018], sep="/"))
head(d.2018)
str(d.2018)



#selecting the variables I want 

d.2018.c  <- d.2018 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, REPRO,  AIR.TEMP)

# This data frame didn't have ova count or rattle segments 
#??? will this be weird when I try to make one large data frame with all the years? 

head(d.2018.c)
str(d.2018.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2018.c <- rename(d.2018.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = REPRO
                   , air.temp = AIR.TEMP) 

str(d.2018.c)


# changing date to correct format 
#delete this because it didnt work for this year 
# d.2018.c$catch.date <- as.Date(d.2018.c$catch.date, format="%m/%d/%Y")
library(lubridate)

d.2018.c$catch.date <- as.Date(parse_date_time(d.2018.c$catch.date, 
                                               c('%d%m%y', '%m%d%y')))
class(d.2018.c$catch.date)



# ===  2019 =====
file.2019 <- 14
d.2019 <- read.csv(paste(p.data.temp, all.years[file.2019], sep="/"))
head(d.2019)
str(d.2019)



#selecting the variables I want 

d.2019.c  <- d.2019 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, REPRO,  AIR.TEMP)


head(d.2019.c)
str(d.2019.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2019.c <- rename(d.2019.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = REPRO
                   , air.temp = AIR.TEMP) 

str(d.2019.c)

# changing date to correct format 
d.2019.c$catch.date <- as.Date(d.2019.c$catch.date, format="%m-%d-%Y")
class(d.2019.c$catch.date)


# ===  2020 =====
file.2020 <- 15
d.2020 <- read.csv(paste(p.data.temp, all.years[file.2020], sep="/"))
head(d.2020)
str(d.2020)


#selecting the variables I want 

d.2020.c  <- d.2020 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, REPRO, AIR.TEMP)


head(d.2020.c)
str(d.2020.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2020.c <- rename(d.2020.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = REPRO
                   , air.temp = AIR.TEMP) 

str(d.2020.c)


# changing date to correct format 
d.2020.c$catch.date <- as.Date(d.2020.c$catch.date, format="%m-%d-%Y")
class(d.2020.c$catch.date)


# ===  2021 =====
file.2021 <- 16
d.2021 <- read.csv(paste(p.data.temp, all.years[file.2021 ], sep="/"))
head(d.2021)
str(d.2021)


#selecting the variables I want 

d.2021.c  <- d.2021 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, REPRO,  AIR.TEMP)


head(d.2021.c)
str(d.2021.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2021.c <- rename(d.2021.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = REPRO
                   , air.temp = AIR.TEMP) 

str(d.2021.c)


# changing date to correct format 
d.2021.c$catch.date <- as.Date(d.2021.c$catch.date, format="%m-%d-%Y")
class(d.2021.c$catch.date)


# ===  2022 =====
file.2022 <- 17
d.2022 <- read.csv(paste(p.data.temp, all.years[file.2022 ], sep="/"))
head(d.2022)
str(d.2022)


#selecting the variables I want 

d.2022.c  <- d.2022 %>% 
  #arrange(-year) %>% 
  select(DATE, RESEARCHER, SPECIES, PIT.TAG, SVL, MASS, AGE.CLASS, SEX, REPRO,  AIR.TEMP)


head(d.2022.c)
str(d.2022.c)

# renaming variables to be consistent 
# using dplyr to do this with the "rename" function 


d.2022.c <- rename(d.2022.c  # used the same name for the new data frame so I overwrote the last one
                   , catch.date = DATE
                   , person = RESEARCHER 
                   , species = SPECIES
                   , PIT.tag = PIT.TAG
                   , length = SVL 
                   , mass = MASS
                   , age.class = AGE.CLASS
                   , sex = SEX  
                   , repro = REPRO
                   , air.temp = AIR.TEMP) 

str(d.2022.c)

# changing date to correct format 

d.2022.c$catch.date <- as.Date(d.2022.c$catch.date, format="%m/%d/%Y")

class(d.2022.c$catch.date)



# === 2) Combine to one large data frame ==============================================================


#using rbind - note to self: for this function all the columns must be the same. There were three where I inlcuded rattle
#segments and ova count, but as this messed things up in the rbind function I decided to remove them for simplicity 
# sake and since there were only 3 years with that data 


# using do.call so that I can merge multiple dataframes 

d.merged <- do.call("rbind", list(d.2005.c, d.2006.c, d.2007.c, d.2008.c, d.2009.c, d.2010.c, d.2011.c,
                                  d.2013.c, d.2014.c, d.2015.c, d.2016.c, d.2017.c, d.2018.c, d.2019.c,
                                  d.2020.c, d.2021.c, d.2022.c))
class(d.merged$catch.date)
# coming up as date! 


# === 3) FIX TYPOS AND ERRORS IN VARIABLES  ===============================================================================

head(d.merged)
str(d.merged)
nrow(d.merged)

## below are the variables that need to be checked/fixed 

# catch.date = character
# person = character
# species = character
# PIT.tag = character
# length = numeric
# mass = numeric
# age.class = character
# repro = character
# air.temp = numeric


# ===  species  =====

# Non rattlesnakes were named "NA" to make them easier to remove from the dataset 


# look at unique names 
species.names <- unique(d.merged$species)
# number of unigue names
length(unique(species.names)) 
# 35! that is a lot. Indicating  lots of errors

# make a dataframe with the current name and the corrected name
# repeat NA for the length of the amount of species names (right?)

correct.name <- rep(NA, length(species.names))
species.corrections <- cbind.data.frame(species.names, correct.name)
# add the correct  name to the second column
species.corrections[1,2] <-  "rattlesnake"
species.corrections[2,2] <-  "rattlesnake"
species.corrections[7,2] <-  "rattlesnake"
species.corrections[9,2] <-  "rattlesnake"
species.corrections[10,2] <- "rattlesnake"
species.corrections[11,2] <- "rattlesnake"
species.corrections[13,2] <- "rattlesnake"
species.corrections[15,2] <- "rattlesnake"
species.corrections[16,2] <- "rattlesnake"
species.corrections[20,2] <- "rattlesnake"
species.corrections[22,2] <- "rattlesnake"
species.corrections[26,2] <- "rattlesnake"
species.corrections[29,2] <- "rattlesnake"

####  checking some of the typos 
which(d.merged$species == "RA") # unlcear if this is rattle or racer so leaving as NA
which(d.merged$species == "Neonate") # all below are Neonate in the species class. Might be worth fixing this if I have time 

# [1]  772  775  938  940  962  963  964  965  968  972  988  989  990  991  998  999 1000 1001 1002
# [20] 1003 1004 1015 1016 1024 1025 1032 1036 1037

which(d.merged$species == " Neonate") # not fixed yet 
# [1] 1181 
which(d.merged$species == "Juvenile") # not fixed yet 
# [1] 975
which(d.merged$species == ".Juvenile") # not fixed yet 
# [1]  986 1051





### loop for creating these corrections 

for(i in 1:nrow(species.corrections)){
  # i is an index to go from row to row
  # it starts with i <- 1, which is species.corrections row 1
  #    species.names correct.name
  # 1   Rattlesnake  rattlesnake
  
 #   i <- 4
  # determine the location (which  row) where species.names == Rattlesnake
  loc.t  <- which(d.merged$species == species.corrections$species.names[i])  
  # length(loc.t) tells you how many, for i = 1 that is 1622!
  # print the selected names to the screen: just an extra
  d.merged$species[loc.t]
  
  # replace the species name with the correct species name (correct.name[i])
  # selected all the rows with the species name you want to change (loc.t)
  # and replace them with  the corrected name correct.name[i]
  # for the example where i = 1, that is with  "rattlesnake"
  # remember, I do this in d.merged as an extra  step so we can compare
  # with d.merged (unchanged) if it worked
  d.merged$species[loc.t] <- species.corrections$correct.name[i]
  # at the end of the for loop, R now adds automaticall +1 to the counters
  # i become 2. As long as the I is not larger than the end of the loop 
  # (nrow(species.corrections) = 35), it keeps going!
}


## Deleting all NAs - this will change the number of rows though - so when I go back to correct some typos 
# I need to change them before this step 

loc.NA <- which(is.na(d.merged$species))
length(loc.NA) # 739


nrow(d.merged) # 5165 entries. 

# deleting all rows with NA 
d.merged <- d.merged[-loc.NA,]

# how many rows now? 
nrow(d.merged) # 4427

# ===  length   =====

class(d.merged$length) # data is a character 
length.t <- as.numeric(d.merged$length)
### NOTE NAs introduced by coercion warming

# looking for outliers 
hist(length.t, breaks = 1000)
# by setting the breaks really large we see that there are two really 
# large values: ~280 and ~470
#unfortunately max does not work but we can use sort and see what values they are
sort.length <- sort(length.t)
tail(sort.length) # 260.0 467.0 --> these are large remove outliers later 

# but looks like we can convert the length in the dataframe to numeric and no 
# character left to mess it up. Nice
d.merged$length <- as.numeric(d.merged$length)
# Warning message:
# NAs introduced by coercion 

# use the histogram to look at weird outliers and play with the bin
hist(d.merged$length[d.merged$length < 200], breaks = 60)

# one thing that strikes me is a weird subset of very tiny rattlesnakes....
# another pattern is that there seems to be a bimondal distribution: one with
# mean ~28 and one ~ 60.
d.merged$length
sample.t <-(which(d.merged$length=="0"))

# sample.t showing index of where there is a 0
# omit them or replace with NA and then omit 

 # complete.cases(d.merged$length[length==0]<-NA)


# sort length to look at the smallest numbers 

length.t1 <- d.merged[order(d.merged$length),]



# ===  mass  =====

class(d.merged$mass) # data is character but needs to be numeric 
mass.t <- as.numeric(d.merged$mass)

### NAs introduced by coercion here as well
hist(mass.t, breaks = 1000)
#large spread 

# sort mass to look at the smallest numbers 
# omit negatives they are mistakes 
# 0 should have been NA's so omit them 
mass.t1 <- d.merged[order(d.merged$mass),]


sort.mass <- sort(mass.t)
head(sort.mass)

# -122 -122 -120    0    0    0 
# there shouldn't be negatives 

d.merged$mass <- as.numeric(d.merged$mass)
#NA by coercion 

hist(mass.t , breaks = 200)

hist(d.merged$mass[d.merged$mass > 0], breaks = 200)


# ===  age.class  =====

unique(d.merged$age.class)

age.names <- unique(d.merged$age.class)
# number of unigue names
length(unique(age.names))

#doing what we did for species names 
correct.age <- rep(NA, length(age.names))
age.corrections <- cbind.data.frame(age.names, correct.age)

age.corrections[1,2] <-  "adult"
age.corrections[2,2] <-  "sub.adult"
age.corrections[3,2] <-  "juvenile"
age.corrections[4,2] <-  "neonate"
age.corrections[6,2] <-  "adult"
age.corrections[7,2] <-  "juvenile"
age.corrections[8,2] <-  "adult"
age.corrections[9,2] <-  "adult"
age.corrections[10,2] <-  "sub.adult"
age.corrections[11,2] <-  "sub.adult"
age.corrections[14,2] <-  "adult"
age.corrections[15,2] <-  "adult"
age.corrections[17,2] <-  "sub.adult"
age.corrections[18,2] <-  "adult"
age.corrections[19,2] <-  "adult"
age.corrections[20,2] <-  "sub.adult"
age.corrections[21,2] <-  "sub.adult"
age.corrections[22,2] <-  "neonate"
age.corrections[23,2] <-  "juvenile"
age.corrections[24,2] <-  "sub.adult"
age.corrections[25,2] <-  "neonate"
age.corrections[26,2] <-  "neonate"
age.corrections[27,2] <-  "sub.adult"
age.corrections[28,2] <-  "neonate"
age.corrections[29,2] <-  "neonate"
age.corrections[30,2] <-  "juvenile"
age.corrections[31,2] <-  "sub.adult"
age.corrections[32,2] <-  "sub.adult"
age.corrections[33,2] <-  "juvenile"



### checking location of typos 

which(d.merged$age.class == "Juvenile*")
# [1] 1041


# implement the name correction by going through the dataset
for(i in 1:nrow(age.corrections)){
  # i is an index to go from row to row
  # it starts with i <- 1, which is age.corrections row 1
  #    age.names correct.age
  #   i <- 1
  
  aloc.t  <- which(d.merged$age.class == age.corrections$age.names[i])  
  # check number of locations
  #print(length(which(d.merged$age.class == age.corrections$age.names[i])))
  
  # visualise the selected names
  d.merged$age.class[aloc.t]
  
  # substitute these locations with the right name
  d.merged$age.class[aloc.t] <- age.corrections$correct.age[i]
  # check number of locations
  #print(length(which(d.merged$age.class == age.corrections$age.names[i])))
 
}



# ===  sex =====

# look at unique names 
sex.names <- unique(d.merged$sex)
# number of unigue names
length(unique(sex.names)) 

correct.sex <- rep(NA, length(sex.names))
sex.corrections <- cbind.data.frame(sex.names, correct.sex)
# add the correct  name to the second column
sex.corrections[1,2] <- "male" 
sex.corrections[2,2] <- "female"
sex.corrections[4,2] <- "male"
sex.corrections[5,2] <- "male" 
sex.corrections[6,2] <- "female"
sex.corrections[10,2] <- "male" 
sex.corrections[11,2] <- "female" 
sex.corrections[14,2] <- "male"
sex.corrections[15,2] <- "female" 
sex.corrections[16,2] <-  "male" 
sex.corrections[17,2] <- "female" 
sex.corrections[18,2] <- "female" 
sex.corrections[20,2] <-  "female"
sex.corrections[21,2] <- "male" 
sex.corrections[22,2] <- "female" 
sex.corrections[23,2] <-  "female"
sex.corrections[25,2] <-  "male"

# loop for creating these corrections 

for(i in 1:nrow(sex.corrections)){
 
  
  
  loc.t  <- which(d.merged$sex == sex.corrections$sex.names[i])  
  
  d.merged$sex[loc.t]
  
  
  d.merged$sex[loc.t] <- sex.corrections$correct.sex[i]
 
}




# ===  repro  =====


unique(d.merged$repro)


repro.names <- unique(d.merged$repro)

# number of unigue names
length(unique(repro.names))

#doing what we did for species names 
correct.repro <- rep(NA, length(repro.names))
repro.corrections <- cbind.data.frame(repro.names, correct.repro)

# postpartum
# gravid 
# nongravid 
# NA (for males)

# Questions: 
# what does FALSE mean? 
false.data.t <- d.merged[which(d.merged$repro== "FALSE"),]
# false is being used for male and female 
# If false is in females --> non-gravid
# if false is in males --> NA 

repro.corrections[2,2] <-  "gravid"
repro.corrections[4,2] <-  "nongravid"
repro.corrections[7,2] <-  "nongravid"
repro.corrections[8,2] <-  "gravid"
# repro.corrections[10,2] <-  "nongravid"
repro.corrections[11,2] <-  "nongravid"
repro.corrections[12,2] <-  "gravid"
repro.corrections[13,2] <-  "nongravid"
repro.corrections[14,2] <-  "gravid"
repro.corrections[15,2] <-  "gravid"
repro.corrections[16,2] <-  "postpartum"
repro.corrections[18,2] <-  "gravid"
repro.corrections[19,2] <-  "nongravid"
repro.corrections[20,2] <-  "gravid"
repro.corrections[21,2] <-  "nongravid"
repro.corrections[22,2] <-  "postpartum"
repro.corrections[23,2] <-  "gravid"
repro.corrections[24,2] <-  "postpartum"
repro.corrections[25,2] <-  "postpartum"
repro.corrections[26,2] <-  "gravid"
repro.corrections[29,2] <-  "postpartum"
repro.corrections[30,2] <-  "gravid"
repro.corrections[32,2] <-  "postpartum"
repro.corrections[33,2] <-  "postpartum"
repro.corrections[34,2] <-  "nongravid"
repro.corrections[35,2] <-  "gravid"
repro.corrections[36,2] <-  "postpartum"
repro.corrections[37,2] <-  "postpartum"
repro.corrections[38,2] <-  "nongravid"
repro.corrections[39,2] <-  "nongravid"
repro.corrections[40,2] <-  "nongravid"
repro.corrections[41,2] <-  "postpartum"




for(i in 1:nrow(age.corrections)){
  # i is an index to go from row to row
  # it starts with i <- 1, which is repro.corrections row 1
  #    arepro.names correct.repro
  #   i <- 1
  
  aloc.t  <- which(d.merged$repro == repro.corrections$repro.names[i])  
  
  d.merged$repro[aloc.t]
  
  
  d.merged$repro[aloc.t] <- repro.corrections$correct.repro[i]
  
}



# ===  OUTLIERS   =====

# using the z-score method --> removing anything beyond 3 standard deviations 

# Preview the data
head(d.merged)

# **** mass *****
# add a new column to the data frame containing the z-score
d.merged$zscore.mass <- (abs(d.merged$mass-mean(d.merged$mass, na.rm = T))/
                      sd(d.merged$mass, na.rm = T))

# Check the data again. It should now have two columns: X and z-score
str(d.merged)


# **** length *****
# add a new column to the data frame containing the z-score
d.merged$zscore.length <- (abs(d.merged$length-mean(d.merged$length, na.rm = T))/
                      sd(d.merged$length, na.rm = T))
str(d.merged)

d.merged$zscore.max <- as.numeric(rep(NA, nrow(d.merged)))
# new Z score column with maximum Z score of the two
for(i in 1: nrow(d.merged)){
  # i <- 23
  d.merged$zscore.max[i] <- max(c(d.merged$zscore.length[i],
                                  d.merged$zscore.mass[i]), na.rm = T)
  # catcht the double NA's and turn it into an NA
  if(is.na(d.merged$zscore.length[i]) == T && is.na(d.merged$zscore.mass[i]) == T){
    d.merged$zscore.max[i] <- NA
  }
}
# note: the warnings for the - Inf has been addressed.

d.merged[60,]
# create a new dataframe that contains only those rows 
# that have a z-score of below 3
Z.large.1 <- which(d.merged$zscore.max > 3)
d.merged[8:10,]

d.merged.z <- subset(d.merged, d.merged$zscore.max < 3)
which(d.merged.z$zscore.max > 3)
d.merged.z[8:10,]
# check the new dataset
dim(d.merged.z)



# === 5) Send to clean data folder  ==================================================================
# data has been cleaned for:
# name errors (species, sex, repo, age)
# variable type: length and mass now numeric
# outliers: mass and length |Z| > 3
write.csv(d.merged.z, paste(p.data.clean,"snake.all.years.clean.csv", sep = ""))


# === CLIMATE DATA  ==================================================================================


### trying Thor's method 

climate.file.names <- list.files(paste(p.data.raw, "osoyoos.climate.folder", sep="" ))

p.climate.files <- paste(p.data.raw, "osoyoos.climate.folder/", climate.file.names,sep="" )

climate.all <- do.call("rbind", lapply(p.climate.files, function(x) read.csv(x, stringsAsFactors = FALSE)))
str(climate.all)
head(climate.all)
tail(climate.all)
unique(climate.all$Year)

# make a selection of the variables

climate.all.s <- subset(climate.all, select = c(Date.Time, Year, Max.Temp...C., 
                                                 Min.Temp...C., Mean.Temp...C., 
                                                 Total.Precip..mm.))
# check names of variables and rename where needed
colnames(climate.all.s)
clim.var.new.name <- c("Date.Time", "Year", "Max.Temp.C", "Min.Temp.C",
                       "Mean.Temp.C", "Total.Precip.mm")
colnames(climate.all.s) <- clim.var.new.name
head(climate.all.s)
str(climate.all.s)

# save to cleaned data folder
write.csv(climate.all.s, paste(p.data.clean, "climate.years.all.clean.csv", 
                               sep = "") )

#___ end _______________________________________________________________________





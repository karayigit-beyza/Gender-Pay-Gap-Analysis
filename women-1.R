### installing packages 

install.packages("tidyverse")
install.packages("readxl")
install.packages("DT")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("ggthemes")
install.packages("plotly")
install.packages("xlsx")

### libraries 

library(tidyverse)
library(xlsx)
library(readxl)
library(DT)
library(knitr)
library(rmarkdown)
library(ggthemes)
library(plotly)
 

list.of.packages <- c("tidyverse", "readxl", "DT", "knitr", "rmarkdown", "ggthemes", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org" )
installed.packages()[,"Package"]

setwd("C:/Users/Beyza/Documents/MSIS/Courses/Fall 2019-2ndflex/Data Wrangling/Working Directory")

### importing data set 1

### original data set 1 
 age.female.earnings <- read.csv("women/earnings_female.csv", stringsAsFactors = FALSE)

### since I'll be not using total 16 years and older values, I'll skip these rows

headers <- names(read.csv("women/earnings_female.csv",nrows = 1))

new.age.female.earnings <- read.csv("women/earnings_female.csv", stringsAsFactors = FALSE, header = F, col.names = headers, skip = 34 )

#### There are three variables here year, group, and percent. To be more clear, I'll change the variable names. 

names(new.age.female.earnings) <- c("year", "age.group", "female.wage.ratio.to.men")

### changing the values in age.group column 
new.age.female.earnings$age.group[new.age.female.earnings$age.group == "16-19 years"] <- "16-19"
new.age.female.earnings$age.group[new.age.female.earnings$age.group == "20-24 years"] <- "20-24"
new.age.female.earnings$age.group[new.age.female.earnings$age.group == "25-34 years"] <- "25-34"
new.age.female.earnings$age.group[new.age.female.earnings$age.group == "35-44 years"] <- "35-44"
new.age.female.earnings$age.group[new.age.female.earnings$age.group == "45-54 years"] <- "45-54"
new.age.female.earnings$age.group[new.age.female.earnings$age.group == "55-64 years"] <- "55-64"
new.age.female.earnings$age.group[new.age.female.earnings$age.group == "65 years and older"] <- "S" ### Senior

### Examining data set 1 


str(age.female.earnings)



### Check null values 
colSums(is.na(new.age.female.earnings))



### There are not null values in my data set

### Sub-data set 1.1 
 age_subdata1 <- filter(new.age.female.earnings, age.group == "16-19")
 age.subdata1.2 <- filter(new.age.female.earnings, age.group == "20-24")
 age.subdata1.3 <- filter(new.age.female.earnings, age.group == "25-34")
 age.subdata1.4 <- filter(new.age.female.earnings, age.group == "35-44")
 age.subdata1.5 <- filter(new.age.female.earnings, age.group == "45-54")
 age.subdata1.6 <- filter(new.age.female.earnings, age.group == "55-64")
 age.subdata1.7 <- filter(new.age.female.earnings, age.group == "S")
 
 ### For the data sets above, it will be examined how trend went for each group from 1979 to 2011. How each group changed for years.
 
 ### Creating new variable "year.groups" 
 by.year.group.earnings <- new.age.female.earnings %>% filter(year > 1978) %>%
    transmute(age.group, female.wage.ratio.to.men, year.group = year)
 
 by.year.group.earnings$year.group <- ifelse(by.year.group.earnings$year.group %in% c("1979","1980", "1981", "1982", "1983", "1984","1985", "1986", "1987","1988", "1989"), "1.Dec-1980",
                                       ifelse(by.year.group.earnings$year.group %in% c("1990", "1991", "1992", "1993", "1994","1995", "1996", "1997","1998", "1999"), "2.Dec-1990s",
                                       ifelse(by.year.group.earnings$year.group %in% c("2000", "2001", "2002", "2003", "2004","2005", "2006", "2007","2008", "2009","2010", "2011"), "3.Dec-2000s", "NA")))
    
## 2nd new variable created avg.fem.wage.ratio

age.subdata.9 <-  by.year.group.earnings %>% group_by(age.group, year.group) %>% 
transmute( avg.fem.wage.ratio = mean(female.wage.ratio.to.men)) %>% 
   unique() 

##Spreding age.subdata.9 to calculate salary ratio difference
ratio.difference <- age.subdata.9 %>% spread(key = year.group, value = avg.fem.wage.ratio)
ratio.difference <- mutate(ratio.difference, change80sto90s = `2.Dec-1990s`- `1.Dec-1980`) 
ratio.difference <- mutate(ratio.difference, change90sto2000s = `3.Dec-2000s`- `2.Dec-1990s`) %>% 
   select(-`1.Dec-1980`)

## Gathering Ratio Difference 
ratio.difference <- ratio.difference %>% gather(`2.Dec-1990s`, `3.Dec-2000s`, key = year.group, value = avg.fem.wage.ratio) %>% 
   gather(change80sto90s, change90sto2000s, key = change.type, value = wage.percent.change) 
  

 ### Last 5 years age group values --- Remove in the final
 target <- c("2011", "2010", "2009", "2008", "2007")
 age.subdata1.8 <-  new.age.female.earnings %>% 
   filter(new.age.female.earnings$year %in% target) %>% 
   arrange(year)
 
 
 ### For the data set above, it will be examined how women wage ratio to men based on age group.
 ### Which age group has largest gap in the last 5 years how its characteristic changed over years.

 
 ### Graphing age groups female wage ratio by year 
 n <- new.age.female.earnings %>% ggplot(aes(x = year, y = female.wage.ratio.to.men, color = age.group)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~age.group) +
    ggtitle("Female Wage Ratio Change Values by Age Groups Year By Year",
            subtitle = "From 1979 - 2011") +
    labs(y = "Female Wage To Men Wage Percent" ) 
    n + scale_color_discrete(name = "Age Group")
    
 
 names(new.age.female.earnings)
 

 ## Graphing female wage ratio by decades & age 
    a <- ratio.difference %>% ggplot(aes(age.group, wage.percent.change, fill = change.type)) +
    geom_bar(position = "dodge", stat = "identity") +
    ggtitle("Female Wage Ratio Change Values by Decades & Age Groups",
            subtitle = "From 1979 - 2011") +
    labs(x = "Age Group", y = "Wage Percent Change" ) 
    a + scale_fill_discrete(name = " Change Type")
    

 
### Importing data set 2 

occupation.salary <- read.csv("women/jobs_gender.csv", stringsAsFactors = FALSE )

### Examining data set 2

str(occupation.salary)
attributes(occupation.salary)

### Drop columns not necessary
tidy.occupation.salary <- select(occupation.salary, -c(2,5,8,9))

names(tidy.occupation.salary)
### total workers, total earnings, female percent can be extracted from other columns if they are needed.
### However they are not planned to use for now. Also, occupation column will not be used. 

names(tidy.occupation.salary) <- c("year", "maj_cat", "min_cat", "male_worker_no", "female_worker_no", "expected_male_earnings", "expected_fem_earnings", "fem.wage.ratio.to.men")




### Checking unique values for major and minor categories 
sapply(tidy.occupation.salary,function(x) length(table(x)))

### we can see that there are 8 unique major categories and 23 minor categories. 
### Checking the names in each categorical columns whether there is need to change it or nor
major.cat <- tidy.occupation.salary$maj_cat %>% 
   unique()
view(major.cat)

minor.cat <-  tidy.occupation.salary$min_cat %>% 
   unique()
view(minor.cat)

### Truncating major and minor names
### Since the description is very long for major categories, we decided to truncate them but minor categories
### will be left as it is to prevent any confusion. 
colSums(is.na(tidy.occupation.salary))


 
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Management, Business, and Financial" ] <- "MNG, BSN, FN" 
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Computer, Engineering, and Science" ] <- " CMP, ENG, SCI "
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Education, Legal, Community Service, Arts, and Media" ] <- "ED, LG, CM-SER, ART-MED"
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Healthcare Practitioners and Technical" ] <- "HEALTHCARE, PRC,TCH "
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Service" ] <- " SRV"
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Sales and Office" ] <- "SLS -OFC "
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Natural Resources, Construction, and Maintenance" ] <- "NAT-RSC, CONST, MAINT "
tidy.occupation.salary$maj_cat[tidy.occupation.salary$maj_cat == "Production, Transportation, and Material Moving" ] <- " PRD, TRNS, MAT-MOV "

colSums(is.na(tidy.occupation.salary))
### many missing values are in the wage_percent_of_male. However, we can get the values for some of them from 
### total earnings female and total earnings male since it is basically total earnings femle/ total earnings_male


sub.missing.values <- tidy.occupation.salary %>% 
   select(expected_male_earnings, expected_fem_earnings, fem.wage.ratio.to.men)
   
final.sub.missing <- sub.missing.values[rowSums(is.na(sub.missing.values)) > 1,]

### Final.sub.missing.values data frame shows which cells we can't calculate fem.wage.ratio.to men
str(final.sub.missing) 
colSums(is.na(final.sub.missing))

### We can see that 69 observations we are not able to fill. Since the missing values are less t
### than 4% I decided to ignore these 69 observations 

### After checking this data set, I should ignore missing values in expected_male earnings and
### expected_fem_earnings 

final.tidy.occupation.salary <- tidy.occupation.salary[complete.cases(tidy.occupation.salary[, 6:7]),]
   

colSums(is.na(final.tidy.occupation.salary))

final.tidy.occupation.salary <- final.tidy.occupation.salary %>% 
   mutate(calc.fem.wage.ratio.to.men = expected_fem_earnings/expected_male_earnings * 100)
colSums(is.na(final.tidy.occupation.salary))


## Remove the female.wage.ratio. to men since we calculated it 
final.tidy.occupation.salary <- select(final.tidy.occupation.salary, -fem.wage.ratio.to.men)
colSums(is.na(final.tidy.occupation.salary))

### Based on major category and years, creating sub_data 

occp.subdata1 <- filter(final.tidy.occupation.salary, maj_cat == "MNG, BSN, FN")
occp.subdata2 <- filter(final.tidy.occupation.salary, maj_cat == " CMP, ENG, SCI ")  
occp.subdata3 <- filter(final.tidy.occupation.salary, maj_cat == "ED, LG, CM-SER, ART-MED")
occp.subdata4 <- filter(final.tidy.occupation.salary, min_cat == "Healthcare Practitioners and Technical")
occp.subdata5 <- filter(final.tidy.occupation.salary, maj_cat == " SRV")
occp.subdata6 <- filter(final.tidy.occupation.salary, maj_cat == "SLS -OFC ")
occp.subdata7 <- filter(final.tidy.occupation.salary, maj_cat == "NAT-RSC, CONST, MAINT ")
occp.subdata8 <- filter(final.tidy.occupation.salary, maj_cat == " PRD, TRNS, MAT-MOV ")

## Unknown
subdata1.mean <- mean(occp.subdata1$calc.fem.wage.ratio.to.men)

## Graphing Majors that has lowest pay gap 

## Finding highest and lowest minor groups
graph.tidy.occupation <-  final.tidy.occupation.salary %>% 
   group_by(maj_cat, min_cat, year) %>% 
   transmute(med.fem.wage.ratio = median(calc.fem.wage.ratio.to.men)) %>% 
   unique()

agg.graph.tidy.occupation <- graph.tidy.occupation %>% group_by(min_cat) %>% 
   summarise(med.fem.wage.ratio.agg = mean(med.fem.wage.ratio)) %>% 
   arrange(desc(med.fem.wage.ratio.agg))

colSums(is.na(agg.graph.tidy.occupation))

final.tidy.occupation.salary %>% ggplot(aes(x="", y = calc.fem.wage.ratio.to.men, color = min_cat)) +
   geom_boxplot()+
   facet_wrap(~min_cat, labeller = label_wrap_gen(width= .005))+
   theme(text = element_text(size = 8))+
   theme_minimal(base_size = 8)+
   coord_cartesian(ylim = c(0,100)) +
   coord_flip() + guides(colour = FALSE)

arrange(final.tidy.occupation.salary, desc(calc.fem.wage.ratio.to.men))
         
   
str(final.tidy.occupation.salary)

### Median women salar percentage distribitution based on minor categories

occp.subdata1 %>% ggplot(aes(x=maj_cat, y= calc.fem.wage.ratio.to.men, color=min_cat))+
   geom_boxplot()  + 
   scale_y_continuous(breaks = seq(0, 300, 30)) + coord_flip()

occp.subdata2 %>% ggplot(aes(x=maj_cat, y= calc.fem.wage.ratio.to.men, color=min_cat))+
   geom_boxplot() + scale_y_continuous(breaks = seq(0, 300, 30)) + coord_flip()

occp.subdata3 %>% ggplot(aes(x=maj_cat, y= calc.fem.wage.ratio.to.men, color=min_cat))+
   geom_boxplot()  + scale_y_continuous(breaks = seq(0, 300, 30)) + coord_flip()

occp.subdata4 %>% ggplot(aes(x=maj_cat, y= calc.fem.wage.ratio.to.men, color=min_cat))+
   geom_jitter() +
   geom_boxplot()  + scale_y_continuous(breaks = seq(0, 300, 30))+ coord_flip()

occp.subdata5 %>% ggplot(aes(x=maj_cat, y= calc.fem.wage.ratio.to.men, color=min_cat)) +
   geom_jitter() +
   geom_boxplot()  + scale_y_continuous(breaks = seq(0, 300, 10)) + coord_flip()

occp.subdata6 %>% ggplot(aes(x=maj_cat, y = calc.fem.wage.ratio.to.men, color=min_cat))+
  geom_boxplot() + scale_y_continuous(breaks = seq(0, 300, 30)) + coord_flip()

occp.subdata7 %>% ggplot(aes(x=maj_cat, y = calc.fem.wage.ratio.to.men, color=min_cat))+
   geom_boxplot()  + scale_y_continuous(breaks = seq(0, 300, 30)) +
   coord_flip() 

occp.subdata8 %>% ggplot(aes(x=maj_cat, y= calc.fem.wage.ratio.to.men, color=min_cat))+
   geom_boxplot()  + scale_y_continuous(breaks = seq(0, 300, 30)) + coord_flip()

##Graphing min category over time 

graph.tidy.occupation %>% ggplot(aes(x = year, y = med.fem.wage.ratio, color = min_cat)) +
   geom_point() +
   geom_line() +
   facet_wrap(~min_cat) + guides(color = FALSE)

### Change for number of man employee and women employee 

## First graph number of employee change over time for each minor category
str(new.age.female.earnings)

graph.employee.no <- final.tidy.occupation.salary %>% select(year, min_cat, female_worker_no, male_worker_no) %>% 
   group_by(min_cat, year) %>% 
   transmute(male_worker_no, med.no.female = ceiling(median(female_worker_no))) %>% 
   transmute(med.no.female, med.no.male = ceiling(median(male_worker_no))) %>% 
   unique() %>% 
   mutate(ratio.femnum.to.mennum = med.no.female/med.no.male*100)
  
a <- c("Legal","Community and Social Service", "Education, Training, and Library", "Healthcare Practitioners and Technical", "Healthcare Support", "Office and Administrative Support")
 filter(graph.employee.no, !(min_cat %in% a)) %>% 
   ggplot(aes(x = year, y = ratio.femnum.to.mennum , color = min_cat)) +
   geom_line() +
   facet_wrap(~min_cat) + guides(color = FALSE)
 getwd()
 
 write.csv(final.tidy.occupation.salary, "occupation_salary.csv" )

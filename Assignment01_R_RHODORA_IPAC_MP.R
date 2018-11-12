## Assignment No. 1 for Computing for Analytics
## Submitted by Rhodora B. Ipac
## Date : November 7, 2018

## Assignment 
## No. 1 WHO Data Set
## 1.a Load and run Session01.R
## 1.b Country w/ Biggest Population 
## 1.c Population of Malaysia
## 1.d Country with Lowest Literacy Rate
## 1.e Richest country in Europe based on GNI
## 1.f Mean LIfe Expectancy of Countries in Africa
## 1.g Number of Countries with population greater than > 10,000
## 1.h Top 5 countries in US with high child mortality Rate 

## 1 Load .csv file 

WHO <- read.csv("WHO.csv")

## 1.b Country with Biggest Population 
## Answer : China 
MaxPopulation <- subset(WHO, Population == (max(WHO$Population, na.rm = TRUE)))
MaxPopulation$Country

## 1.c Population of Malaysia
## Answer : 29,240
MalaysiaPop <- subset(WHO, Country == "Malaysia")
MalaysiaPop$Population

## 1.d Country with Lowest Literacy Rate 
## Answer : Mali 

MinLiteracy <- subset(WHO, LiteracyRate == min(WHO$LiteracyRate, na.rm = TRUE))
MinLiteracy$Country

## 1.e Richest Country in Europe Based on GNI
## Answer : Luxembourg

Countries_Europe <- subset(WHO, Region == "Europe")
Rich_Country_Europe <- subset(Countries_Europe, GNI == (max(Countries_Europe$GNI, na.rm = TRUE)))
Rich_Country_Europe$Country

## 1.f Mean LIfe Expectancy of Countries in Africa 
## Answer : 57.95652

CountriesAfrica <- subset(WHO, Region == "Africa")
Mean_Life <- mean(CountriesAfrica$LifeExpectancy)
Mean_Life

## 1.g No. of Countries with Population > 10000
## Answer : 86
Countries_10m <- WHO$Population > 10000
sum(Countries_10m)

## 1.h Top 5 countries in US with high child mortality 
## Answer : 1. Haiti, Bolivia, Guyana, Guatemala, Dominican Republic

Countries_US <- subset(WHO, Region == "Americas")
order_countries_US <- order(Countries_US$ChildMortality, decreasing = TRUE)
Top5_Countries <-Countries_US[order_countries_US,]
Top5_Countries
Top5 <- head(Top5_Countries,5)
Top5$Country


## 2 NBA Data Set (Historical NBA Performance)

install.packages("readxl")
library("readxl")
NBA <- read_excel("NBA2.xlsx")
NBA

## 2.a The year Bulls has the highest winning percentage
## Answer : 1995-96
Bulls <- subset(NBA, Team == "Bulls")
Bulls_Highest <- max(Bulls$`Winning Percentage`)
Highest_win <- subset(Bulls,`Winning Percentage` == Bulls_Highest)
Highest_win
Highest_win$Year

## 2.b Team with an even win-loss record in a year
## Answer : 53 Teams 

Even_win <- subset(NBA, `Winning Percentage` =="0.5")
Even_win$Team

## 3 Seasons_Stats.csv

Season_Stats <- read.csv("Seasons_Stats.csv")
Season_Stats

## 3.a Players with the Highest 3-pt attempt rate in a season
## Answer -- 29 Players 
## [1] Dudley Bradley   Walker Russell   Mike Dunleavy   
## [4] Mark Randall     Tom Hovasse      Kevin Pritchard 
## [7] Tyson Wheeler    Jason Miskiri    Jud Buechler    
## [10] Charles Smith    Von Wafer        Dajuan Wagner   
## [13] Billy Thomas     Martell Webster  Marcus Landry   
## [16] Anthony Tolliver Bobby Simmons    Keith Bogans    
## [19] Othyus Jeffers   Andre Dawkins    Jamaal Franklin 
## [22] Jimmer Fredette  Erick Green      Joe Harris      
## [25] Steve Novak      Nate Robinson    R.J. Hunter     
## [28] Chris McCullough Axel Toupane    

Max_3Pt <- max(Season_Stats$X3PAr, na.rm = TRUE)
Max_3Pt
High_3Pt <- subset(Season_Stats, X3PAr == Max_3Pt)
High_3Pt$Player

##3b Player with Highest Free Throw Rate in a Season 
## Answer : Dwayne Jones & Andris Biedrins - FTr = 6 
Max_FT <- max(Season_Stats$FTr, na.rm = TRUE)
Max_FT
Max_FT_Players <- subset(Season_Stats, FTr == Max_FT)
Max_FT_Players$Player

## 3c What year/season does Lebron James scored the highest
## Answer : 2006 

Lebron_stats <- subset(Season_Stats, Player == "LeBron James")
Lebron_max_stats <- max(Lebron_stats$PTS, na.rm = TRUE)
Lebron_max_pts <- subset(Lebron_stats, PTS == Lebron_max_stats)
Lebron_max_pts$Year


## 3d What year/season does Lebron James scored the highest
## Answer = 1987
M_jordan <- subset(Season_Stats, Player == 'Michael Jordan*')
M_jordan_max <- max(M_jordan$PTS, na.rm = TRUE)
M_jordan_max_pts <- subset(M_jordan, PTS == M_jordan_max)
M_jordan_max_pts$Year

## 3e Player efficiency rating of Kobe Bryan in the year where his MP is lowest
## Answer : PER = 10.7

Kobe_ratings <- subset(Season_Stats, Player == "Kobe Bryant")
Kobe_ratings
Kobe_per <- subset(Kobe_ratings, MP == min(Kobe_ratings$MP))
Kobe_per$PER


#4 National University Ranking.csv

NU_rank <- read.csv('National Universities Rankings.csv')

#4a University with most number of undergraduates
#answer : University of Florida 
NU_rank[which.max(NU_rank$Undergrad.Enrollment),]$Name


#4b Top 10 Universities 
# Answer 
#1                   Princeton University    Princeton, NJ    1
#2                     Harvard University    Cambridge, MA    2
#3                  University of Chicago      Chicago, IL    3
#4                        Yale University    New Haven, CT    3
#5                    Columbia University     New York, NY    5
#6                    Stanford University     Stanford, CA    5
#7  Massachusetts Institute of Technology    Cambridge, MA    7
#8                        Duke University       Durham, NC    8
#9             University of Pennsylvania Philadelphia, PA    8
#10              Johns Hopkins University    Baltimore, MD   10

Top_10 <- NU_rank[order(NU_rank$Rank),][1:10,]
print(Top_10)

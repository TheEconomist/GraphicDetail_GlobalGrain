## Ainslie Johnstone 
## Global Food Production

library(tidyverse)
library(countrycode)
library(cowplot)
library(networkD3)
library(htmlwidgets)
library(htmltools)
library(rgdal)
library(httr)
library(readxl)

# Main data sources are: 
# - FoodBalance Sheet from UN
# - Country grain exports from OEC
# - Alexander 2016 data on the amount of dry feed needed to produce a kg of food
# - Herrero 2013 data on the amount of animal's feed that is made up of grain
# - Cassidy 2013 data on the feed conversion ratio based on grain alone

setwd('~/Desktop/GlobalGrain/')

GlobalFood <- read_csv('FoodBalanceSheets_E_All_Data.csv', locale = locale(encoding = "Latin1"))
GNIvals <- read_csv('API_NY.GNP.PCAP.CD_DS2_en_csv_v2_4152200.csv')
Population <- GlobalFood%>%
  filter(., Item=='Population'&`Element`=='Total Population - Both sexes')%>%
  select(., -c(Y2010F,Y2011F,Y2012F,Y2013F,Y2014F,Y2015F,Y2016F,Y2017F,Y2018F,Y2019F))%>%
  pivot_longer(., names_to='Years', values_to='Population', cols=c(Y2010:Y2019))%>%
  select(., Area, Years, Population)

## Line Figures -----

GlobalLine <- GlobalFood%>%
  filter(., Area=='World')%>%
  filter(., Item=='Cereals - Excluding Beer'|Item=='Population')%>%
  mutate(., Item='Cereals - Excluding Beer')%>%
  select(., -c(Y2010F,Y2011F,Y2012F,Y2013F,Y2014F,Y2015F,Y2016F,Y2017F,Y2018F,Y2019F))%>%
  filter(., !(Element %in% c('Production', 'Import Quantity', 'Stock Variation', 'Export Quantity', 'Tourist consumption',  'Domestic supply quantity', 'Food supply quantity (kg/capita/yr)', 'Food supply (kcal/capita/day)', 'Protein supply quantity (g/capita/day)', 'Residual','Fat supply quantity (g/capita/day)','Tourist consumption')))%>%
  pivot_longer(., names_to='Years', values_to='Value', cols=c(Y2010:Y2019))%>%
  select(., -c(Unit, `Element Code`, `Item Code`))%>%
  pivot_wider(., names_from=Element, values_from=Value)%>%
  rename(., Population=`Total Population - Both sexes`)%>%
  mutate(., Feed=Feed/Population, Seed=Seed/Population, Losses=Losses/Population, Processing=Processing/Population, 
         Food=Food/Population, Biofuel=`Other uses (non-food)`/ Population)%>%
  select(., -`Other uses (non-food)` , -Residuals)%>%
  pivot_longer(., names_to='Breakdown', values_to='Value_perThosandPeople', cols=c(Feed:Biofuel))%>%
  mutate(., Years=parse_number(Years))%>%
  mutate(., Value=Value_perThosandPeople*Population)%>%
  group_by(., Breakdown)%>%
  arrange(., Breakdown, Years)%>%
  mutate(., ChangeValuePP=(Value_perThosandPeople-Value_perThosandPeople[1L]), ChangeValuePP_PC=(Value_perThosandPeople-Value_perThosandPeople[1L])/Value_perThosandPeople[1L]*100 )%>%
  mutate(., ChangeValue=(Value-Value[1L]), ChangeValuePC=(Value-Value[1L])/Value[1L]*100 )%>%
  group_by(.,Years)%>%
  mutate(., PCofYear=Value/sum(Value))
  
GlobalLineLPcombine <- GlobalFood%>%
  filter(., Area=='World')%>%
  filter(., Item=='Cereals - Excluding Beer'|Item=='Population')%>%
  mutate(., Item='Cereals - Excluding Beer')%>%
  select(., -c(Y2010F,Y2011F,Y2012F,Y2013F,Y2014F,Y2015F,Y2016F,Y2017F,Y2018F,Y2019F))%>%
  filter(., !(Element %in% c('Production', 'Import Quantity', 'Stock Variation', 'Export Quantity', 'Tourist consumption',  'Domestic supply quantity', 'Food supply quantity (kg/capita/yr)', 'Food supply (kcal/capita/day)', 'Protein supply quantity (g/capita/day)', 'Residual','Fat supply quantity (g/capita/day)','Tourist consumption')))%>%
  pivot_longer(., names_to='Years', values_to='Value', cols=c(Y2010:Y2019))%>%
  select(., -c(Unit, `Element Code`, `Item Code`))%>%
  pivot_wider(., names_from=Element, values_from=Value)%>%
  rename(., Population=`Total Population - Both sexes`)%>%
  mutate(., `Processing and losses`=`Losses`+`Processing`)%>%
  mutate(., Feed=Feed/Population, Seed=Seed/Population, `Processing and losses`=`Processing and losses`/Population, 
         Food=Food/Population, Biofuel=`Other uses (non-food)`/ Population)%>%
  select(., -`Other uses (non-food)`, -Losses, -Processing, -Residuals)%>%
  pivot_longer(., names_to='Breakdown', values_to='Value_perThosandPeople', cols=c(Feed:Biofuel))%>%
  mutate(., Years=parse_number(Years))%>%
  mutate(., Value=Value_perThosandPeople*Population)%>%
  group_by(., Breakdown)%>%
  arrange(., Breakdown, Years)%>%
  mutate(., ChangeValuePP=(Value_perThosandPeople-Value_perThosandPeople[1L]), ChangeValuePP_PC=(Value_perThosandPeople-Value_perThosandPeople[1L])/Value_perThosandPeople[1L]*100 )%>%
  mutate(., ChangeValue=(Value-Value[1L]), ChangeValuePC=(Value-Value[1L])/Value[1L]*100 )%>%
  group_by(.,Years)%>%
  mutate(., PCofYear=Value/sum(Value))%>%
  mutate(., ChangeValuePP_inKg=ChangeValuePP*1000)


# Global, all grain
ggplot(GlobalLine, aes(y=Value, x=Years, fill=fct_reorder(Breakdown, Value)))+
    geom_area(position='fill')+labs(y='Grain supply kTns/cpt')
ggplot(GlobalLine, aes(y=ChangeValue, x=Years, colour=Breakdown))+
  geom_line()+labs(title='All Grain', y='Change in grain supply kTns/capita')+theme_minimal()

(A <- ggplot(GlobalLineLPcombine, aes(y=ChangeValuePP_PC, x=Years, colour=Breakdown))+
  geom_line()+labs(title='All Grain, Percentage change in grain supply usage', y='%')+theme_minimal())
(B <- ggplot(GlobalLineLPcombine, aes(y=ChangeValuePP, x=Years, colour=Breakdown))+
  geom_line()+labs(title='All Grain, Change in grain supply usage', y='Ktonnes/ thousand people')+theme_minimal())

plot_grid(A,B, nrow=2)


## Animal Calories-----

AnimalConversionKg <- read.csv('Alexander2016_feed-required-to-produce-one-kilogram-of-meat-or-dairy-product.csv')
AnimalConversionKcal <- read.csv('Alexander2016_energy-efficiency-of-meat-and-dairy-production.csv')

AnimalFoodRatios <- read_excel('Herrero2013Data.xlsx')%>%
  mutate(., Grains==as.numeric(Grains), NonGrains=(as.numeric(Stover)+as.numeric(Occasional)+as.numeric(Grazing)))%>%
  select(., Entity, Grains, NonGrains)%>%
  pivot_longer(., names_to='FoodType', values_to='Value', cols=c('Grains', 'NonGrains'))%>%
  group_by(., Entity)%>%
  mutate(., GrainsPC=Value/sum(Value))%>%
  select(., -Value)%>%
  filter(., FoodType=='Grains')

GrainFeedWeight <- GlobalFood%>%
  filter(., `Area`=='World')%>%
  filter(., `Item`=='Cereals - Excluding Beer')%>%
  filter(., Element=='Feed')%>%
  select(., Y2019)%>%
  as.numeric(.)
  
GrainKcalsPerKg<- GlobalFood%>%
  filter(., `Area`=='World')%>%
  filter(., `Item`=='Wheat and products'|`Item`=='Maize and products'|`Item`=='Millet and products'|`Item`=='Oats'|`Item`=='Rye and products'|`Item`=='Sorghum and products'|`Item`=='Rice and products'|`Item`=='Cereals, Other')%>%
  filter(., Element=='Food supply quantity (kg/capita/yr)'|Element=='Food supply (kcal/capita/day)'|Element=='Feed')%>%
  select(., Area, Item, Element, Y2019)%>%
  pivot_wider(., names_from=Element, values_from=Y2019 )%>%
  mutate(., Kcal_Per_Kg=(`Food supply (kcal/capita/day)`*365.25)/`Food supply quantity (kg/capita/yr)`)%>%
  summarise(., Kcal_Per_Kg_weighted=weighted.mean(Kcal_Per_Kg, Feed))%>%
  as.numeric(.)
  

AnimalCalories<- GlobalFood%>%
  filter(., `Area`=='World')%>%
  filter(., `Item`=='Bovine Meat'|`Item`=='Mutton & Goat Meat'|`Item`=='Pigmeat'|`Item`=='Poultry Meat'|Item=='Milk - Excluding Butter'|Item=='Eggs')%>%
  filter(., `Element Code`==645)%>%
  select(., Area, Item, Element, Y2019)%>%
  unique(.)%>%
  mutate(., Item=ifelse(Item=='Bovine Meat', 'Beef', 
                        ifelse(Item=='Mutton & Goat Meat', 'Lamb/mutton',
                               ifelse(Item=='Pigmeat', 'Pork',
                                      ifelse(Item=='Milk - Excluding Butter', 'Whole Milk',
                                             ifelse(Item=='Poultry Meat', 'Poultry',Item))))))%>%
  left_join(.,AnimalConversionKg, by=c('Item'='Entity'))%>%
  left_join(.,AnimalConversionKcal, by=c('Item'='Entity', 'Year', 'Code'))%>%
  left_join(., AnimalFoodRatios, by=c('Item'='Entity'))%>%
  select(.,-Year, -Code)%>%
  mutate(., totalFeed=Y2019*FeedConversion*GrainsPC)%>%
  mutate(., FeedPC=totalFeed/sum(totalFeed))%>%
  mutate(., FeedWeight=FeedPC*GrainFeedWeight)%>%
  mutate(., FeedKcal=FeedWeight*(GrainKcalsPerKg*1000*1000))%>%
  mutate(., FoodKcal=FeedKcal*(EnergyConversionCassidyPlus/100))


PopulationGrainCalories<- GlobalFood%>%
  filter(.,`Area`=='World'| `Area`=='India'|`Area`=='United States of America'|`Area`=='China')%>%
  filter(., Item=='Population'|Item=='Cereals - Excluding Beer')%>%
  filter(., `Element`=='Total Population - Both sexes'|Element=='Food supply (kcal/capita/day)'|Element=='Food supply quantity (kg/capita/yr)')%>%
  select(., Area, Element, Y2019)%>%
  pivot_wider(.,  names_from=Element, values_from=Y2019)%>%
  mutate(., GrainKcal=(`Total Population - Both sexes`*1000)*(`Food supply (kcal/capita/day)`*365.25))%>%
  mutate(., GrainWeight=(`Total Population - Both sexes`*1000)*(`Food supply quantity (kg/capita/yr)`))


## Russia and Ukraine Grain end-use FULL METHOD--------
# Using OEC data on export countries

for (Grain in c('Wheat', 'Maize', 'Barley','Oats','Rice', 'Rye')){
  

UkrGrainLoopExpKeep <- GlobalFood%>%
  filter(., Area=='Ukraine'|Area=='Russian Federation')%>%
  mutate(., Item=ifelse(Item=='Oats', 'Oats and products', Item))%>%
  filter(., Item==paste0(Grain,' and products'))%>%
  filter(., Element=='Production'|Element=='Export Quantity')%>%
  select(., Area, Item, Element, Y2019)%>%
  group_by(., Item, Element)%>%
  summarise(., Area=Area,`Y2019`=`Y2019`, TotalElement=sum(`Y2019`))%>%
  ungroup(.)%>%
  mutate(., TotalProd=max(TotalElement), TotalExp=min(TotalElement))%>%
  select(., -TotalElement)%>%
  pivot_wider(., names_from=Element, values_from=Y2019)%>%
  mutate(., Keep=Production-`Export Quantity`, PC=Keep/TotalExp)%>%
  filter(., Area=='Ukraine')%>%
  select(., PC)%>%
  as.numeric()

RusGrainLoopExpKeep <- GlobalFood%>%
  filter(., Area=='Ukraine'|Area=='Russian Federation')%>%
  mutate(., Item=ifelse(Item=='Oats', 'Oats and products', Item))%>%
  filter(., Item==paste0(Grain,' and products'))%>%
  filter(., Element=='Production'|Element=='Export Quantity')%>%
  select(., Area, Item, Element, Y2019)%>%
  group_by(., Item, Element)%>%
  summarise(., Area=Area,`Y2019`=`Y2019`, TotalElement=sum(`Y2019`))%>%
  ungroup(.)%>%
  mutate(., TotalProd=max(TotalElement), TotalExp=min(TotalElement))%>%
  select(., -TotalElement)%>%
  pivot_wider(., names_from=Element, values_from=Y2019)%>%
  mutate(., Keep=Production-`Export Quantity`, PC=Keep/TotalExp)%>%
  filter(., Area=='Russian Federation')%>%
  select(., PC)%>%
  as.numeric()

BothGrainLoopExport <- GlobalFood%>%
  filter(., Area=='Ukraine'|Area=='Russian Federation')%>%
  mutate(., Item=ifelse(Item=='Oats', 'Oats and products', Item))%>%
  filter(., Item==paste0(Grain,' and products'))%>%
  filter(., Element=='Export Quantity')%>%
  select(., Y2019)%>%
  summarise(.,TotalElement=sum(`Y2019`))%>%
  as.numeric()

GrainSankeyStage1 <-  GlobalFood%>%
  mutate(., Item=ifelse(Item=='Oats', 'Oats and products', Item))%>%
  filter(., Item==paste0(Grain,' and products'))%>%
  filter(., `Element Code` %in% c(5521, 5527,5123,5131,5154,5142))%>%
  select(., Area, Item, Element, Y2019)%>%
  group_by(., Area)%>%
  mutate(., total=sum(Y2019, na.rm=TRUE))%>%
  mutate(., pcUsage=ifelse(!is.na(Y2019), Y2019/total, 0))%>%
  select(., Area, Item, Element, pcUsage)%>%
  pivot_wider(., names_from=Element, values_from=pcUsage)%>%
  replace(is.na(.), 0)%>%
  mutate(., ISO=countrycode(Area, origin = "country.name", destination = "iso3c"))%>%
  filter(., !is.na(ISO))

filename=paste0('Both',Grain,'.csv')
BothGrainExp <- read.csv(filename)%>%
  mutate(., ISO=toupper(ISO.3))%>%
  mutate(., pcTrade=Trade.Value/sum(Trade.Value))%>%
  rbind(., c('Europe', 'eu', 'Ukraine', 'ukr',NA,  NA, 2020, 'UKR', UkrGrainLoopExpKeep ))%>%
  rbind(., c('Europe', 'eu', 'Russian Federation', 'rus',NA,  NA, 2020, 'RUS', RusGrainLoopExpKeep ))%>%
  mutate(., isRusUkr=ifelse(ISO=='RUS'|ISO=='UKR', 'Y', 'N'))%>%
  mutate(., totalExp= BothGrainLoopExport)%>%
  mutate(., valExp=totalExp*as.numeric(pcTrade))

GrainSankeyFinal <- BothGrainExp %>%
  left_join(., GrainSankeyStage1)%>%
  select(., ISO, Area, valExp, Feed,Food,Processing, `Other uses (non-food)`, Seed, Losses, isRusUkr)%>%
  pivot_longer(., names_to='Usage', values_to='RelValue', cols=c(Feed,Food,Processing, `Other uses (non-food)`, Seed, Losses))%>%
  mutate(., Value=RelValue*valExp)%>%
  group_by(., Usage)%>%
  summarise(., TotalValue=sum(Value, na.rm=TRUE))%>%
  mutate(., ThisGrain=Grain)

if (Grain=="Wheat"){
  OverallSankeyFinal <- GrainSankeyFinal
} else {
OverallSankeyFinal <- bind_rows(GrainSankeyFinal,OverallSankeyFinal)
}

}

OverallSankey <- OverallSankeyFinal%>%
  group_by(., Usage)%>%
  summarise(Final=sum(TotalValue))%>%
  ungroup(.)%>%
  mutate(., pc=Final/sum(Final))

ggplot(OverallSankeyFinal ,aes(y=Usage, x=TotalValue))+
  geom_bar(stat='identity')+facet_wrap(~ThisGrain)

write.csv(OverallSankey, 'RusUkr_AllGrainFinalUsage.csv')
write.csv(OverallSankeyFinal, 'RusUkr_FinalUsage.csv')


## Single countries Grain end-use FULL METHOD--------
# Using OEC data on export countries

country='Ukraine'
countryname='Ukraine'
for (Grain in c('Wheat', 'Maize', 'Barley','Oats','Rice', 'Rye')){
  
  
  ThisGrainLoopExpKeep <- GlobalFood%>%
    filter(., Area==country)%>%
    mutate(., Item=ifelse(Item=='Oats', 'Oats and products', Item))%>%
    filter(., Item==paste0(Grain,' and products'))%>%
    filter(., Element=='Production'|Element=='Export Quantity')%>%
    select(., Area, Item, Element, Y2019)%>%
    group_by(., Item, Element)%>%
    summarise(., Area=Area,`Y2019`=`Y2019`, TotalElement=sum(`Y2019`))%>%
    ungroup(.)%>%
    mutate(., TotalProd=max(TotalElement), TotalExp=min(TotalElement))%>%
    select(., -TotalElement)%>%
    pivot_wider(., names_from=Element, values_from=Y2019)%>%
    mutate(., Keep=Production-`Export Quantity`, PC=Keep/TotalExp)%>%
    select(., PC)%>%
    as.numeric()

  
  ThisGrainLoopExport <- GlobalFood%>%
    filter(., Area==country)%>%
    mutate(., Item=ifelse(Item=='Oats', 'Oats and products', Item))%>%
    filter(., Item==paste0(Grain,' and products'))%>%
    filter(., Element=='Export Quantity')%>%
    select(., Y2019)%>%
    summarise(.,TotalElement=sum(`Y2019`))%>%
    as.numeric()
  
  GrainSankeyStage1 <-  GlobalFood%>%
    mutate(., Item=ifelse(Item=='Oats', 'Oats and products', Item))%>%
    filter(., Item==paste0(Grain,' and products'))%>%
    filter(., `Element Code` %in% c(5521, 5527,5123,5131,5154,5142))%>%
    select(., Area, Item, Element, Y2019)%>%
    group_by(., Area)%>%
    mutate(., total=sum(Y2019, na.rm=TRUE))%>%
    mutate(., pcUsage=ifelse(!is.na(Y2019), Y2019/total, 0))%>%
    select(., Area, Item, Element, pcUsage)%>%
    pivot_wider(., names_from=Element, values_from=pcUsage)%>%
    replace(is.na(.), 0)%>%
    mutate(., ISO=countrycode(Area, origin = "country.name", destination = "iso3c"))%>%
    filter(., !is.na(ISO))
  
  filename=paste0(countryname,Grain,'.csv')
  GrainExp <- read.csv(filename)%>%
    mutate(., ISO=toupper(ISO.3))%>%
    mutate(., pcTrade=Trade.Value/sum(Trade.Value))%>%
    rbind(., c(NA, NA, country, NA ,NA,  NA, 2020, countrycode(country,origin = "country.name", destination = "iso3c") , ThisGrainLoopExpKeep ))%>%
    mutate(., totalExp= ThisGrainLoopExport)%>%
    mutate(., valExp=totalExp*as.numeric(pcTrade))
  
  GrainSankeyFinal <- GrainExp %>%
    left_join(., GrainSankeyStage1)%>%
    select(., ISO, Area, valExp, Feed,Food,Processing, `Other uses (non-food)`, Seed, Losses)%>%
    pivot_longer(., names_to='Usage', values_to='RelValue', cols=c(Feed,Food,Processing, `Other uses (non-food)`, Seed, Losses))%>%
    mutate(., Value=RelValue*valExp)%>%
    group_by(., Usage)%>%
    summarise(., TotalValue=sum(Value, na.rm=TRUE))%>%
    mutate(., ThisGrain=Grain)
  
  if (Grain=="Wheat"){
    OverallSankeyFinal <- GrainSankeyFinal
  } else {
    OverallSankeyFinal <- bind_rows(GrainSankeyFinal,OverallSankeyFinal)
  }
  
}

OverallSankey <- OverallSankeyFinal%>%
  group_by(., Usage)%>%
  summarise(Final=sum(TotalValue))%>%
  ungroup(.)%>%
  mutate(., pc=Final/sum(Final))

ggplot(OverallSankeyFinal ,aes(y=Usage, x=TotalValue))+
  geom_bar(stat='identity')+facet_wrap(~ThisGrain)

write.csv(OverallSankey, 'Ukraine_AllGrainFinalUsage.csv')


## FC numbers -----

World_pc_usageALL <-  GlobalFood%>%
  filter(., Area=='World')%>%
  filter(., Item=='Cereals - Excluding Beer')%>%
  filter(., (Element %in% c('Feed','Food','Processing', 'Other uses (non-food)', 'Seed', 'Losses')))%>%
  select(., Area, Item, Element, Y2019)%>%
  mutate(., PC=Y2019/sum(Y2019)*100)%>%
  print(.)
  
World_pc_usageWheat <-  GlobalFood%>%
  filter(., Area=='World')%>%
  filter(., Item=='Wheat and products')%>%
  filter(., (Element %in% c('Feed','Food','Processing', 'Other uses (non-food)', 'Seed', 'Losses')))%>%
  select(., Area, Item, Element, Y2019)%>%
  mutate(., PC=Y2019/sum(Y2019)*100)%>%
  print(.)

World_pc_usageMaize <-  GlobalFood%>%
  filter(., Area=='World')%>%
  filter(., Item=='Maize and products')%>%
  filter(., (Element %in% c('Feed','Food','Processing', 'Other uses (non-food)', 'Seed', 'Losses')))%>%
  select(., Area, Item, Element, Y2019)%>%
  mutate(., PC=Y2019/sum(Y2019)*100)%>%
  print(.)




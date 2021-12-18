#https://www.kaggle.com/stefanoleone992/tripadvisor-european-restaurants 



library(tidyverse);



mydata1 <- read.csv('/Users/hugoarrondelle/Documents/Insa/5SDBD/R/tripadvisor_european_restaurants.csv', sep = ",")

################################################################
#Restaurant Claimed en fonction des régions d'angletrre
################################################################

claimedToSouthWestEngland  <- mydata1 %>%
  filter(region == 'Cornwall' | region == 'Devon' | region == 'Somerset' | region == 'Dorset' | region == 'Gloucestershire' | region == 'Bristol' | region == 'Wiltshire' | region == 'Cotswolds' | region == 'Exmoor National Park') %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "South West")

claimedToSouthWestEngland


claimedToSouthEastEngland  <- mydata1 %>%
  filter(region == 'Oxfordshire' | region == 'Berkshire' | region == 'Hampshire' | region == 'Buckingghamshire' | region == 'Surrey' | region == 'West Sussex' | region == 'East Sussex' | region == 'Kent' | region == 'Isle of Wight') %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "South East")

claimedToSouthEastEngland


claimedToNorthWestEngland  <- mydata1 %>%
  filter(region == 'Cumbria' | region == 'Lancashire' | region == 'Manchester' | region == 'Greater Manchester' | region == 'Peak District National Park' | region == 'Merseyside' | region == 'Cheshire ' ) %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "North West ")

claimedToNorthWestEngland


claimedToWestMidlandsEngland  <- mydata1 %>%
  filter(region == 'Shropshire' | region == 'Staffordshire' | region == 'West midlands' | region == 'Warwickshire' | region == 'Herefordshire' | region == 'Worcesterhire' ) %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "West Midlands")

claimedToWestMidlandsEngland


claimedToEastMidlandsEngland  <- mydata1 %>%
  filter(region == 'Lincolnshire' | region == 'Nottinghamshire' | region == 'Northamptonshire' | region == 'Leicestershire' | region == 'Derbyshire' | region == 'Rutland'  ) %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "East Midlands")

claimedToEastMidlandsEngland


claimedToLondonEngland<- mydata1 %>%
  filter(region == 'Greater London' | region == 'London'  ) %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "London ")

claimedToLondonEngland


claimedToNorthEastEngland  <- mydata1 %>%
  filter(region == 'Northumberland' | region == 'Durham' | region == 'Stockton-on-Tees' | region == 'Tyne and Wear'  ) %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "North East")

claimedToNorthEastEngland


claimedToYorksHumberEngland  <- mydata1 %>%
  filter(region == 'Yorkshire'  ) %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "Yorks Humber")

claimedToYorksHumberEngland


claimedToAngliaEngland  <- mydata1 %>%
  filter(region == 'Bedfordshire' | region == 'Hertfordshire' | region == 'Cambridgeshire' | region == 'Essex' | region == 'East Angila'  ) %>%
  count(claimed) %>% 
  mutate(Ratio = n /sum(n)) %>%
  filter(claimed == 'Claimed')%>%
  mutate(regionType = "Anglia")

claimedToAngliaEngland


################################################################
#Graphe Restaurant Claimed en fonction des régions d'angletrre
################################################################

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


region_scores <- data.frame(
  row.names = c("Claimed"),
  South_West = c(0.5836533),
  South_East = c(0.5475078),
  North_West = c(0.4369565),
  West_Midlands = c(0.5213722),
  East_Midlands = c(0.4946401),
  London = c(0.4604941),
  North_East = c(0.4845153),
  Yorks_Humber = c(0.491155),
  Anglia = c(0.5078991)
)
region_scores

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  South_West = c(0,1),
  South_East = c(0,1),
  North_West = c(0,1),
  West_Midlands = c(0,1),
  East_Midlands = c(0,1),
  London = c(0,1),
  North_East = c(0,1),
  Yorks_Humber = c(0,1),
  Anglia = c(0,1)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)



################################################################
# Moyenne des notes des restaurants en fonction des régions d'angletrre
################################################################




noteToSouthWestEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Cornwall' | region == 'Devon' | region == 'Somerset' | region == 'Dorset' | region == 'Gloucestershire' | region == 'Bristol' | region == 'Wiltshire' | region == 'Cotswolds' | region == 'Exmoor National Park') %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "South West")%>%
  summarize(avg_rating,country,med,regionType)

noteToSouthWestEngland

TypeRegion <- rbind(TypeRegion, noteToSouthWestEngland)

noteToSouthEastEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Oxfordshire' | region == 'Berkshire' | region == 'Hampshire' | region == 'Buckingghamshire' | region == 'Surrey' | region == 'West Sussex' | region == 'East Sussex' | region == 'Kent' | region == 'Isle of Wight') %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "South East")%>%
  summarize(avg_rating,country,med,regionType)

noteToSouthEastEngland

TypeRegion <- rbind(TypeRegion, noteToSouthEastEngland)

noteToNorthWestEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Cumbria' | region == 'Lancashire' | region == 'Manchester' | region == 'Greater Manchester' | region == 'Peak District National Park' | region == 'Merseyside' | region == 'Cheshire ' ) %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "North West ")%>%
  summarize(avg_rating,country,med,regionType)

noteToNorthWestEngland

TypeRegion <- rbind(TypeRegion, noteToNorthWestEngland)

noteToWestMidlandsEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Shropshire' | region == 'Staffordshire' | region == 'West midlands' | region == 'Warwickshire' | region == 'Herefordshire' | region == 'Worcesterhire' ) %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "West Midlands")%>%
  summarize(avg_rating,country,med,regionType)

noteToWestMidlandsEngland

TypeRegion <- rbind(TypeRegion, noteToWestMidlandsEngland)

noteToEastMidlandsEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Lincolnshire' | region == 'Nottinghamshire' | region == 'Northamptonshire' | region == 'Leicestershire' | region == 'Derbyshire' | region == 'Rutland'  ) %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "East Midlands")%>%
  summarize(avg_rating,country,med,regionType)

noteToEastMidlandsEngland


TypeRegion <- rbind(TypeRegion, noteToEastMidlandsEngland)

noteToLondonEngland<- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Greater London' | region == 'London'  ) %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "London ")%>%
  summarize(avg_rating,country,med,regionType)

noteToLondonEngland


TypeRegion <- rbind(TypeRegion, noteToLondonEngland)

noteToNorthEastEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Northumberland' | region == 'Durham' | region == 'Stockton-on-Tees' | region == 'Tyne and Wear'  ) %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "North East")%>%
  summarize(avg_rating,country,med,regionType)

noteToNorthEastEngland


TypeRegion <- rbind(TypeRegion, noteToNorthEastEngland)

noteToYorksHumberEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Yorkshire'  ) %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "Yorks Humber")%>%
  summarize(avg_rating,country,med,regionType)

noteToYorksHumberEngland

TypeRegion <- rbind(TypeRegion, noteToYorksHumberEngland)

noteToAngliaEngland  <- mydata1 %>%
  drop_na(avg_rating)%>%
  filter(region == 'Bedfordshire' | region == 'Hertfordshire' | region == 'Cambridgeshire' | region == 'Essex' | region == 'East Angila'  ) %>%
  mutate(med = mean(avg_rating))%>%
  mutate(regionType = "Anglia")%>%
  summarize(avg_rating,country,med,regionType)

noteToAngliaEngland

TypeRegion <- rbind(TypeRegion, noteToAngliaEngland)

TypeRegion

ggplot(TypeRegion, aes(x = avg_rating , y = regionType,color=regionType)) + 
  geom_boxplot() +
  xlab("Note restaurant") +
  geom_text(data = TypeRegion, aes(x = med, y = regionType, label = round(med,digits = 3)), size = 3, vjust = -1, hjust = -0.3)




mydata1 %>%  
  filter (country == 'England') %>%
  count(region)
##############################################################
# Carte de l'union européenne des pays ayant des restaurants récompensés
##############################################################
mydata1_awards <- filter(mydata1, awards != "")
mydata1_awards <- count(mydata1_awards, country)
mydata1_awards

mydata3_awards <- mydata1_awards[-c(24, 19, 15, 7),]
mydata2_awards <- c("country" = "United Kingdom", "n" = int(51585))
mydata3_awards <- rbind(mydata3_awards,mydata2_awards)
mydata3_awards
library(ggplot2)
library(grid)
library(rworldmap)
# Get the world map
worldMap <- getMap()
# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia",
                   "Czech Republic.","Denmark","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy",
                   "Poland","Portugal","Romania","Slovakia",
                   "Spain","Sweden","The Netherlands", "United Kingdom")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

value <- sample(x = seq(0,65000,by = 1000), size = length(europeanUnion),
                replace = TRUE)
europeanUnionTable <- data.frame(country = europeanUnion, value = as.numeric(as.character(mydata3_awards$n)))
europeanUnionTable
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]
# Plot the map
P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))
P
P <- P + scale_fill_gradient(name = "Nb de restaurants récompensés par pays", low = "#E0EDC6", high = "#006400", na.value = "grey50")

P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
# Affichage de la map
P

##############################################################
# Nombre de restaurants ayant reçus des récompenses
mydata1 %>%
  filter(awards != "") %>%
  count(country) %>%
  ggplot(aes( x = n, y= country)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#7CFC00')+
  ggtitle("Nombre de restaurants ayant reçu des récompenses selon les pays") +
  theme(plot.title = element_text(hjust = 0.7))

# Nombre de restaurants selon regime alimentaire et pays
mydata_gluten <- mydata1 %>%
  filter(country== 'England' | country=='Spain' |country == 'Italy' |country == 'France') %>%
  group_by(country) %>%
  count(gluten_free) %>%
  pivot_wider(names_from = gluten_free, values_from = n) %>%
  mutate(Type = 'Sans gluten') %>%
  mutate(Ratio = Y/(N+Y) *100)

mydata_vegan <- mydata1 %>%
  filter(country== 'England' | country=='Spain' |country == 'Italy' |country == 'France') %>%
  group_by(country) %>%
  count(vegan_options) %>%
  pivot_wider(names_from = vegan_options, values_from = n) %>%
  mutate(Type = 'Vegan') %>%
  mutate(Ratio = Y/(N+Y) *100)

mydata_vegetarian <- mydata1 %>%
  filter(country== 'England' | country=='Spain' |country == 'Italy' |country == 'France') %>%
  group_by(country) %>%
  count(vegetarian_friendly) %>%
  pivot_wider(names_from = vegetarian_friendly, values_from = n) %>%
  mutate(Type = 'Vegetarien') %>%
  mutate(Ratio = Y/(N+Y) *100)

mydata_merge <- rbind(mydata_gluten, mydata_vegan)
mydata_merge <- rbind(mydata_merge, mydata_vegetarian)
mydata_merge


ggplot(data=mydata_merge, aes(x=country, y=Y, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() +
  # Afficher les pourcentages
  geom_text( aes(label=sprintf("(%1.1f%%)",round(Ratio,digits = 2))), vjust=-0.4, position = position_dodge(0.9), color="darkgray", size=3.5)+
  xlab('Pays') + ylab('Nombre')

##############################################################
# Diagramme circulaire sur les types de cuisine

library(stringr)
library(data.table)
# Calcul de la fréquence des types de cusines
types <- filter(mydata1, country == 'England' & cuisines != '')
types <- paste(types$cuisines, sep=', ', collapse = ", ")
types <- data.frame(x=unlist( str_split(types, " ")))
types <- setDT(types)[, .(freq = .N), x]
types <- types[order(-types$freq)]
types <- types[-c(6)]
types <- mutate(types, Ratio = round((freq/sum(freq) *100),digits = 3))
types

# Pasrsing du type de cuisine, on sélectionne les 8 premiers types
# et on met le reste des types de cuisine dans le nouveau type others
types_9 <- rbind(types[1:8])
other_row <- test[9:nrow(test)]
types_9 <- types_9 %>%
  add_row(x = "Others", freq = sum(others_row$freq), Ratio = round(sum(others_row$Ratio),digits = 3))
types_9 <- types_9[order(types_9$freq)]
types_9


category = types_9$x
percent = types_9$Ratio
color = rev(rainbow(length(percent)))

library(circlize)
circos.par("start.degree" = 90, cell.padding = c(0, 0, 0, 0))
circos.initialize("a", xlim = c(0, 45)) # 'a` just means there is one sector
circos.track(ylim = c(0.5, length(percent)+0.5), track.height = 0.8,
             bg.border = NA, panel.fun = function(x, y) {
               xlim = CELL_META$xlim
               circos.segments(rep(xlim[1], 9), 1:9,
                               rep(xlim[2], 9), 1:9,
                               col = "#CCCCCC")
               circos.rect(rep(0, 9), 1:9 - 0.45, percent, 1:9 + 0.45,
                           col = color, border = "white")
               circos.text(rep(xlim[1], 9), 1:9,
                           paste(category, " : ", percent, "%"),
                           facing = "downward", adj = c(1.05, 0.5), cex = 0.8)
               breaks = seq(0, 85, by = 5)
               circos.axis(h = "top", major.at = breaks, labels = paste0(breaks, "%"),
                           labels.cex = 0.6)
               
             })











x# Spatial Regression
bra0 <-readOGR("BRA_adm/BRA_adm0.shp", layer='BRA_adm0')
bra1 <-readOGR("BRA_adm/BRA_adm1.shp", layer='BRA_adm1')
bra2 <-readOGR("BRA_adm/BRA_adm2.shp", layer='BRA_adm2')

shape <- readShapeSpatial("BRA_adm/BRA_adm0.shp")

shape@data$id = rownames(shape@data)
shape@data$id = rownames(shape@data)
shape.points = fortify(shape, region="id")
shape.df = join(shape.points, shape@data, by="id")

shape.fort <- fortify(shape, region='id') 
shape.fort<-shape.fort[order(shape.fort$order), ] 
ggplot(data=shape.fort, aes(long, lat, group=group)) + 
  geom_polygon(colour='black',
               fill='white') +
  theme_bw()

b_mod <- fortify(b)? 
? fortify

good_loans = subset(data, y==1)
bad_loans = subset(data, y==-1)

plot(bra0)
points(x=good_loans$Longitude, y=good_loans$Latitude, col='green', cex=0.25)
points(x=bad_loans$Longitude, y=bad_loans$Latitude, col='red', cex=0.25)

brazil <- readOGR('BRA_adm/BRA_adm0.shp', layer='BRA_adm0')
ggplot() + geom_polygon(data=brazil, aes(x=long, y=lat, group=group))

ggplot() +  geom_polygon(data=bra0, aes(x=Longitude, y=Latitude))
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y==1), color="red")
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y==-1), color="blue")

data$good <- ifelse(data$y==1, 'Good', 'Bad')
data$good <- factor(data$good)

plot(factor(data$Title), factor(data$y))
sort(table(data$City), decreasing=TRUE)
RJ <- subset(data, City=='Rio de Janeiro')
ggplot(RJ, aes(x=raw_lexisnexis_score, fill=factor(y))) + geom_density(alpha=0.5)
table(RJ$y)

cities <- levels(data$City)


SP <- subset(data, City==cities[183])
ggplot(SP, aes(x=raw_lexisnexis_score, fill=factor(y))) + geom_density(alpha=0.5)
table(SP$y)

Sal <- subset(data, City=='Salvador')
RJSPSL <- rbind(RJ, SP, Sal)

RJSP <- rbind(RJ, SP)
RJSP_good <- subset(RJSP, y==1)
RJSP_bad <- subset(RJSP, y==-1)

ggplot(RJSP, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSP_good, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSP_bad, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)

RJSPSL_good <- subset(RJSPSL, y==1)
RJSPSL_bad <- subset(RJSPSL, y==-1)

ggplot(RJSPSL, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSPSL_good, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSPSL_bad, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)

plot(bra0)
points(x=RJSPSL_good$Longitude, y=RJSPSL_good$Latitude, col='green', cex=0.25)
points(x=RJSPSL_bad$Longitude, y=RJSPSL_bad$Latitude, col='red', cex=0.25)

dim(RJSPSL_good)
dim(RJSPSL_bad)

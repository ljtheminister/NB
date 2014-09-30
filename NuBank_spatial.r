# Spatial Regression
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
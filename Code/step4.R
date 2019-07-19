
#Geographical view using maps
crime$STATES<-as.character(crime$STATES)
 for (i in 1:nrow(crime)) {
                
                          latlon = geocode(crime[i,1])
                         
                                   crime$lon[i] = as.numeric(latlon[1])
                                    
                                            crime$lat[i] = as.numeric(latlon[2])
                                       
                                                     }


mv_num_collisions = data.frame(ktest$cluster, crime$lon, crime$lat)
 colnames(mv_num_collisions) = c('collisions','lon','lat')
 uk_center = as.numeric(geocode("United kingdom"))



UkMap = ggmap(get_googlemap(center=uk_center, scale=2, zoom=4), extent="normal")

UkMap +
  + geom_point(aes(x=lon, y=lat), data=mv_num_collisions, col="orange", alpha=0.4, size=mv_num_collisions$collisions*circle_scale_amt) + 
  +     
  + scale_size_continuous(range=range(mv_num_collisions$collisions))


nat <- function(zip) {
    require(acs)
    require(ggplot2)
    require(reshape2)
    
    zip <- as.character(zip)
    g <- geo.make(zip.code = zip)
    
    i <- acs.fetch(geo = g, table.number = "B05001", col.names = "pretty")
    d <- data.frame(estimate(i))
    
    d <- as.data.frame(t(d))
    d$nativity <- sub("Nativity.and.Citizenship.Status.in.the.United.States...", "", row.names(d))
    d$nativity <- gsub("\\.", " ", d$nativity); names(d) <- c("count", "nativity")
    row.names(d) <- NULL 
    total <- d[1,1]
    d <- d[-1, ];
    options(digits = 2)
    d$normalized <- d$count / total
    d <- d[order(d$count), ]
    d$nativity <- factor(d$nativity, levels = d$nativity)
    d <- d[ , c(3,1,2,4)]
    
    # dMelt <- melt(d, id = "nativity", variable.name = "count_method", value.name = "population")
    gg <- ggplot(d, aes(x = nativity, y = count))
    gg + geom_bar(stat = "identity", fill = "steelblue") + coord_flip() 
        
}
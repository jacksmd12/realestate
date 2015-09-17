earn_all <- function(zip) {
    require(acs)
    require(reshape2)
    require(ggplot2)
    
    level <- NULL
    zip <- as.character(zip)
    d <- NULL
    
    for (j in 1:length(zip)) {
        g <- geo.make(zip.code = zip[j])
        i <- acs.lookup(table.number = "B20001")
        m <- i[3:22, ]
        f <- i[24:43,]
        temp <- NULL
        
        dm <- acs.fetch(geo = g, variable = m, col.names = "pretty")
        dm <- data.frame(estimate(dm))
        dm <- as.data.frame(t(dm))
        row.names(dm) <- sub("Sex.by.Earnings.for.the.Population.16..Years..With.Earnings...Male...", "", row.names(dm))
        
        if (is.null(level)) {
            level <- levels(factor(row.names(dm), levels = row.names(dm)))
            #print(level)
        }
        
        df <- acs.fetch(geo = g, variable = f, col.names = "pretty")
        df <- as.data.frame(t(data.frame(estimate(df))))
        row.names(df) <- sub("Sex.by.Earnings.for.the.Population.16..Years..With.Earnings...Female...", "", row.names(df))
        
        if (identical(row.names(dm), row.names(df))) {
            temp <- data.frame(cbind(row.names(dm), dm[ ,1], df[ ,1]), stringsAsFactors = F)
            names(temp) <- c("earning_all", "male", "female")
            
            
        } else {print("male and female data don't match")}
        
        temp$zip <- zip[j]
        d <- rbind(d, temp)
    }
    
    d$earning_all <- factor(d$earning_all, levels = level)
    d$male <- as.numeric(d$male)
    d$female <- as.numeric(d$female)
    d <<- d
    dM <- melt(d, id = c("earning_all", "zip"), variable.name = "gender", value.name = "count")
    gg <- ggplot(dM, aes(x = earning_all, y = count))
    gg + geom_bar(stat = "identity") + facet_grid(gender ~ zip) + coord_flip() + theme_bw()
    
}
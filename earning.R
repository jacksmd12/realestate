earn <- function(zip) {
    require(acs)
    require(reshape2)
    require(ggplot2)
    
    level <- NULL
    zip <- as.character(zip)
    d <- NULL
    
    for (j in 1:length(zip)) {
        g <- geo.make(zip.code = zip[j])
        i <- acs.lookup(table.number = "B20005")
        m <- i[6:25, ]
        f <- i[53:72,]
        temp <- NULL
        
        dm <- acs.fetch(geo = g, variable = m, col.names = "pretty")
        dm <- data.frame(estimate(dm))
        dm <- as.data.frame(t(dm))
        row.names(dm) <- sub("Sex.by.Work.Experience.by.Earnings.for.the.Population.16.Years.and.Over...Male..Worked.full.time..year.round.in.the.past.12.months..With.earnings...", "", row.names(dm))

        if (is.null(level)) {
            level <- levels(factor(row.names(dm), levels = row.names(dm)))
            #print(level)
        }

        df <- acs.fetch(geo = g, variable = f, col.names = "pretty")
        df <- as.data.frame(t(data.frame(estimate(df))))
        row.names(df) <- sub("Sex.by.Work.Experience.by.Earnings.for.the.Population.16.Years.and.Over...Female..Worked.full.time..year.round.in.the.past.12.months..With.earnings...", "", row.names(df))
        
        if (identical(row.names(dm), row.names(df))) {
            temp <- data.frame(cbind(row.names(dm), dm[ ,1], df[ ,1]), stringsAsFactors = F)
            names(temp) <- c("full_time_earning", "male", "female")
            
            
        } else {print("male and female data don't match")}

        temp$zip <- zip[j]
        d <- rbind(d, temp)
    }
    
    d$full_time_earning <- factor(d$full_time_earning, levels = level)
    d$male <- as.numeric(d$male)
    d$female <- as.numeric(d$female)
    
    dM <- melt(d, id = c("full_time_earning", "zip"), variable.name = "gender", value.name = "count")
    gg <- ggplot(dM, aes(x = full_time_earning, y = count))
    gg + geom_bar(stat = "identity") + facet_grid(gender ~ zip) + coord_flip() + theme_bw()
    
}
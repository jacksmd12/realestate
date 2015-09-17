# topic can be: "occupiedby", "household" and "housing"

housing <- function(zip, topic = "occupiedby") {
    require(acs)
    require(reshape2)
    require(ggplot2)
    
    zip <- as.character(zip)
    d <- NULL
    level <- NULL
    
    for (i in 1:length(zip)) {
        g <- geo.make(zip.code = zip[i])
        temp <- NULL
        
        if (topic == "occupiedby") {
            t <- acs.lookup(table.number = "B25007")
            o <- t[3:11]
            r <- t[13:21]
            
            owner <- acs.fetch(geo = g, variable = o, col.names = "pretty")
            owner <- as.data.frame(t(data.frame(estimate(owner))))
            row.names(owner) <- sub("TENURE.BY.AGE.OF.HOUSEHOLDER...Owner.occupied..Householder.", "", row.names(owner))
            if(is.null(level)) {level <- levels(factor(row.names(owner), levels = row.names(owner)))}
            owner$age <- row.names(owner); row.names(owner) <- NULL; owner$occupant <- "owner"; names(owner) <- c("count", "age", "occupant")

            renter <- acs.fetch(geo = g, variable = r, col.names = "pretty")
            renter <- as.data.frame(t(data.frame(estimate(renter))))
            row.names(renter) <- sub("TENURE.BY.AGE.OF.HOUSEHOLDER...Renter.occupied..Householder.", "", row.names(renter))
            renter$age <- row.names(renter); row.names(renter) <- NULL; renter$occupant <- "renter"; names(renter) <- c("count", "age", "occupant")

            temp <- rbind(owner, renter); temp$zip <- zip[i]

        } else if (topic == "household") {
            t <- acs.lookup(table.number = "B25009")
            o <- t[3:9]
            r <- t[11:17]
            
            owner <- acs.fetch(geo = g, variable = o, col.names = "pretty")
            owner <- as.data.frame(t(data.frame(estimate(owner))))
            row.names(owner) <- sub("TENURE.BY.HOUSEHOLD.SIZE...Owner.occupied..", "", row.names(owner))
            if(is.null(level)) {level <- levels(factor(row.names(owner), levels = row.names(owner)))}
            owner$age <- row.names(owner); row.names(owner) <- NULL; owner$occupant <- "owner"; names(owner) <- c("count", "age", "occupant")
            
            renter <- acs.fetch(geo = g, variable = r, col.names = "pretty")
            renter <- as.data.frame(t(data.frame(estimate(renter))))
            row.names(renter) <- sub("TENURE.BY.HOUSEHOLD.SIZE...Renter.occupied..", "", row.names(renter))
            renter$age <- row.names(renter); row.names(renter) <- NULL; renter$occupant <- "renter"; names(renter) <- c("count", "age", "occupant")

            temp <- rbind(owner, renter); temp$zip <- zip[i]
            
        } else if (topic == "housing") {
            tr <- acs.lookup(table.number = "B25056")
            r <- tr[3:23]
            
            to <- acs.lookup(table.number = "B25061")
            o <- to[2:22]
            
            owner <- acs.fetch(geo = g, variable = o, col.names = "pretty")
            owner <- as.data.frame(t(data.frame(estimate(owner))))
            row.names(owner) <- sub("RENT.ASKED...", "", row.names(owner))
            if(is.null(level)) {level <- levels(factor(row.names(owner), levels = row.names(owner)))}
            owner$rent <- row.names(owner); row.names(owner) <- NULL; owner$type <- "asking rent"; names(owner) <- c("count", "rent", "type")

            renter <- acs.fetch(geo = g, variable = r, col.names = "pretty")
            renter <- as.data.frame(t(data.frame(estimate(renter))))
            row.names(renter) <- sub("CONTRACT.RENT...With.cash.rent..", "", row.names(renter))
            renter$rent <- row.names(renter); row.names(renter) <- NULL; renter$type <- "contract rent"; names(renter) <- c("count", "rent", "type")

            if (!(identical(owner$rent, renter$rent))) { print("data don't match)")}
            temp <- rbind(owner, renter); temp$zip <- zip[i]
            
        }
        
        d <- rbind(d, temp)
    }


    # data was put together in long form, hence no need for melt
    if (topic == "housing") {
        d$rent <- factor(d$rent, levels = level)
        d$type <- factor(d$type); d$zip <- factor(d$zip)
        gg <- ggplot(d, aes(x = rent, count))
        gg + geom_bar(stat = "identity") + facet_grid(type ~ zip) + coord_flip() + theme_bw()
        
    } else {
        d$age <- factor(d$age, levels = level)
        d$occupant <- factor(d$occupant); d$zip <- factor(d$zip)
        # d <<- d
        gg <- ggplot(d, aes(x = age, count))
        gg + geom_bar(stat = "identity") + facet_grid(occupant ~ zip) + coord_flip() + theme_bw()
        
    }

    
}

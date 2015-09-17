graph_demo <- function(zip) {
    require(dplyr)
    require(lattice)
    
    d <- NULL
    for (i in 1:length(zip)) {
        temp <- demo(zip[i])
        d <- rbind(d, select(temp, age, zip, total))
        
    }
    
    d$zip <- factor(d$zip)
    gg <- ggplot(d, aes(x = age, y = total))
    gg + geom_bar(stat = "identity") + facet_wrap( ~ zip, ncol = 3) + coord_flip() + theme_bw()
    
}



demo <- function(zip) {
    d <- NULL
    
    require(acs)
    require(reshape2)
    require(ggplot2)
    
    zip <- as.character(zip)
    g <- geo.make(zip.code = zip)
    
    i <- acs.lookup(table.number = "B01001")
    m <- i[3:25]
    f <- i[27:49]
    
    dm <- acs.fetch(geo = g, variable = m, col.names = "pretty")
    df <- acs.fetch(geo = g, variable = f, col.names = "pretty")
    
    dm <- as.data.frame(t(data.frame(estimate(dm))))
    row.names(dm) <- gsub("Sex.by.Age...Male..", "", row.names(dm))
    names(dm) <- "male"
    
    df <- as.data.frame(t(data.frame(estimate(df))))
    row.names(df) <- gsub("Sex.by.Age...Female..", "", row.names(df))
    names(df) <- "female"
    
    if(identical(row.names(dm), row.names(df))) {
        d <- cbind(dm, df)
    } else {print("male and female data don't match")}
    
    d$age <- gsub("\\.", "_", row.names(d)); d$age <- sub("Under", "0", d$age); row.names(d) <- NULL
    d$age <- factor(d$age, levels = d$age)
    d$total <- d$male + d$female
    d$zip <- zip
    d

    #dMelt <- melt(d, id = "age", variable.name = "gender", value.name = "count")
    #gg <- ggplot(data = dMelt, aes(x = age, y = count))
    #gg + geom_bar(stat = "identity", aes(fill = gender)) + coord_flip() + facet_grid(. ~ gender, scales = "free_y") + ggtitle(zip)
    
}
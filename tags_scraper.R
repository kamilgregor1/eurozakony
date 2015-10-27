directory <- "C:/Users/NB/Desktop/eurozákony/"
setwd(directory)

rm(directory)

bills <- read.delim("bills.txt")

for (i in 1:nrow(bills)) {
  
  print(paste0("Scraping tags from a bill: ",i,"/",nrow(bills)))
  
  tryCatch({

code <- paste0('html <- readLines("http://www.psp.cz/sqw/historie.sqw?o=',bills[i,1],'&T=',bills[i,2],'")')
eval(parse(text = code))

html <- data.frame(html)
html$included <- sapply(html$html, function(x) ifelse(grepl('Deskriptory EUROVOCu',x)==T,1,0))
html <- data.frame(html[which(html$included==1),1])
html <- data.frame(do.call('rbind', strsplit(as.character(html$html),'Deskriptory EUROVOCu:</b>',fixed=TRUE)))
html <- data.frame(do.call('rbind', strsplit(as.character(html$X2),'<p>',fixed=TRUE)))
html <- data.frame(do.call('rbind', strsplit(as.character(html$X1),', ',fixed=TRUE)))
html <- data.frame(t(html))
names(html) <- "html"

tags_id <- data.frame(do.call('rbind', strsplit(as.character(html$html),'=',fixed=TRUE)))
tags_id$X3 <- as.character(tags_id$X3)
tags_id <- data.frame(do.call('rbind', strsplit(as.character(tags_id$X3),'"',fixed=TRUE)))

tags_name <- data.frame(do.call('rbind', strsplit(as.character(html$html),'=',fixed=TRUE)))
tags_name <- data.frame(do.call('rbind', strsplit(as.character(tags_name$X3),'>',fixed=TRUE)))
tags_name <- data.frame(do.call('rbind', strsplit(as.character(tags_name$X2),'<',fixed=TRUE)))

tags_provisional <- cbind(tags_id[1],tags_name[1])
names(tags_provisional) <- c("tag:id","tag:name")

tags_provisional$period <- bills[i,1]
tags_provisional$"bill:number" <- bills[i,2]
tags_provisional$"bill:eu" <- bills[i,3]

if (i == 1) {
  
  tags <- tags_provisional
  
} else {
  
  tags <- rbind(tags, tags_provisional)
  
} # if (i == 1)

rm(code)
rm(tags_id)
rm(tags_name)
rm(tags_provisional)
rm(html)

  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  })

Sys.sleep(1)

} # for (i in 1:nrow(bills))

rm(i)

write.csv(tags,"tags.csv",row.names=FALSE)
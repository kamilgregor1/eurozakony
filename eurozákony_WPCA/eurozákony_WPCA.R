library("reshape2")

directory <- "C:/Users/NB/Desktop/eurozákony/eurozákony_WPCA"
setwd(directory)

vote_events <- read.delim("vote_events.txt")
voters <- read.delim("voters.txt")
groups <- read.delim("groups.txt")
people <- read.delim("people.txt")

hl2002h1 <- read.delim("hl2002h1.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))
hl2002h2 <- read.delim("hl2002h2.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))
hl2002h3 <- read.delim("hl2002h3.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))
hl2006h1 <- read.delim("hl2006h1.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))
hl2006h2 <- read.delim("hl2006h2.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))
hl2010h1 <- read.delim("hl2010h1.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))
hl2010h2 <- read.delim("hl2010h2.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))
hl2013h1 <- read.delim("hl2013h1.unl", header = FALSE, sep = "|", col.names=c("id_poslanec","vote_event_id","option","drop"))

hl2002h1$term <- 4
hl2002h2$term <- 4
hl2002h3$term <- 4
hl2006h1$term <- 5
hl2006h2$term <- 5
hl2010h1$term <- 6
hl2010h2$term <- 6
hl2013h1$term <- 7

hl2002h1 <- merge(hl2002h1,vote_events,by=c("vote_event_id"),all=FALSE)
hl2002h2 <- merge(hl2002h2,vote_events,by=c("vote_event_id"),all=FALSE)
hl2002h3 <- merge(hl2002h3,vote_events,by=c("vote_event_id"),all=FALSE)
hl2006h1 <- merge(hl2006h1,vote_events,by=c("vote_event_id"),all=FALSE)
hl2006h2 <- merge(hl2006h2,vote_events,by=c("vote_event_id"),all=FALSE)
hl2010h1 <- merge(hl2010h1,vote_events,by=c("vote_event_id"),all=FALSE)
hl2010h2 <- merge(hl2010h2,vote_events,by=c("vote_event_id"),all=FALSE)
hl2013h1 <- merge(hl2013h1,vote_events,by=c("vote_event_id"),all=FALSE)

hl2002h1 <- merge(hl2002h1,voters,by=c("id_poslanec","term"),all=FALSE)
hl2002h2 <- merge(hl2002h2,voters,by=c("id_poslanec","term"),all=FALSE)
hl2002h3 <- merge(hl2002h3,voters,by=c("id_poslanec","term"),all=FALSE)
hl2006h1 <- merge(hl2006h1,voters,by=c("id_poslanec","term"),all=FALSE)
hl2006h2 <- merge(hl2006h2,voters,by=c("id_poslanec","term"),all=FALSE)
hl2010h1 <- merge(hl2010h1,voters,by=c("id_poslanec","term"),all=FALSE)
hl2010h2 <- merge(hl2010h2,voters,by=c("id_poslanec","term"),all=FALSE)
hl2013h1 <- merge(hl2013h1,voters,by=c("id_poslanec","term"),all=FALSE)

votes <- rbind(hl2002h1,hl2002h2)
votes <- rbind(votes,hl2002h3)
votes <- rbind(votes,hl2006h1)
votes <- rbind(votes,hl2006h2)
votes <- rbind(votes,hl2010h1)
votes <- rbind(votes,hl2010h2)
votes <- rbind(votes,hl2013h1)

rm(list=ls()[!ls()%in%c("votes","groups","voters","people")])

votes <- votes[c("voter_id","vote_event_id","option","term","is_eu","group_id")]
lo_limit <- 0.1

votes$option_numeric <- votes$option
votes$option_numeric <- as.character(votes$option_numeric)
votes$option_numeric[votes$option_numeric=='A'] = 1
votes$option_numeric[votes$option_numeric=='@'] = 0
votes$option_numeric[votes$option_numeric=='B'] = -1
votes$option_numeric[votes$option_numeric=='F'] = -1
votes$option_numeric[votes$option_numeric=='C'] = -1
votes$option_numeric[votes$option_numeric=='W'] = -1
votes$option_numeric[votes$option_numeric=='K'] = -1
votes$option_numeric <- as.numeric(votes$option_numeric)
votes <- votes[order(votes$vote_event_id,votes$voter_id),]

for (i in 4:7) {
  
  print(paste0("Estimating the W-PCA model of the ",i,"th term"))
  
  X_source <- votes[which(votes$term == i),]
  X_source$vote_event_id = as.factor(X_source$vote_event_id)
  X_source$voter_id = as.factor(X_source$voter_id)
  X_source$voter_id = factor(X_source$voter_id, levels=unique(X_source$voter_id))
  X_raw = t(acast(X_source,voter_id~vote_event_id,fun.aggregate=mean,value.var='option_numeric'))
  X_people = dimnames(X_raw)[[2]]
  X_vote_events = dimnames(X_raw)[[1]]
  mode(X_raw) = 'numeric'
  w1 = apply(abs(X_raw)==1,1,sum,na.rm=TRUE)/max(apply(abs(X_raw)==1,1,sum,na.rm=TRUE))
  w1[is.na(w1)] = 0
  w2 = 1 - abs(apply(X_raw==1,1,sum,na.rm=TRUE) - apply(X_raw==-1,1,sum,na.rm=TRUE))/apply(!is.na(X_raw),1,sum)
  w2[is.na(w2)] = 0
  w = w1 * w2
  I = X_raw
  I[!is.na(X_raw)] = 1
  I[is.na(X_raw)] = 0
  I_w = I*w
  s = apply(I_w,2,sum)
  person_w = s/sum(w)
  person_I = person_w > lo_limit
  X_c = X_raw[,person_I]
  X_c_scaled = t(scale(t(X_c),scale=TRUE))
  X_c_scaled_0 = X_c_scaled
  X_c_scaled_0[is.na(X_c_scaled_0)] = 0
  X = X_c_scaled_0 * sqrt(w)
  C = t(X) %*% X
  Xe=eigen(C)
  V = Xe$vectors
  Xy = X %*% V
  sigma = sqrt(Xe$values)
  sigma[is.na(sigma)] = 0
  lambda = diag(sigma)
  X_proj = V %*% lambda
  X_proj_unit = X_proj / sqrt(apply(X_proj^2,1,sum))
  lambda_1 = diag(sqrt(1/Xe$values))
  lambda_1[is.na(lambda_1)] = 0
  U = X %*% V %*% lambda_1
  X_proj2 = t(X) %*% U
  X_proj2_unit = X_proj2 / sqrt(apply(X_proj2^2,1,sum))
  plot(X_proj2_unit[,1],X_proj2_unit[,2])
  
  TI <- X_source[!duplicated(X_source$vote_event_id),]
  TI <- TI$is_eu > 0
  lo_limit_T <- lo_limit
  
  X_raw_T_c = X_raw[,person_I]
  X_raw_T_c[!TI,] = NA
  TI_c = X_raw_T_c
  TI_c[!is.na(TI_c)] = 1
  TI_c[is.na(TI_c)] = 0
  TI_c_w = TI_c * w
  s = apply(TI_c_w,2,sum)
  person_T_w = s/max(s)
  person_TI = person_T_w > lo_limit_T
  XT_c_scaled = (X_raw_T_c-attr(X_c_scaled_0,"scaled:center"))/attr(X_c_scaled_0,"scaled:scale")
  XT_c_scaled_w_0 = XT_c_scaled * sqrt(w)
  XT_c_scaled_w_0[is.na(XT_c_scaled_w_0)] = 0
  XT = XT_c_scaled_w_0[,person_TI]
  TI_cc = TI_c[,person_TI]
  X_cc = X[,person_TI]
  aU = abs(U)
  aX_cc = abs(X_cc)
  aX_proj_cc = t(aX_cc) %*% aU
  aXT_cc_scaled_w_0 = abs(XT)
  aXT_proj_cc = t(aXT_cc_scaled_w_0) %*% aU
  div_person_weights = aXT_proj_cc/aX_proj_cc
  XT_proj = t(XT) %*% U / div_person_weights
  XT_proj_unit = XT_proj / sqrt(apply(XT_proj^2,1,sum))
  plot(XT_proj[,1],XT_proj[,2])
  
  voters_term <- voters[which(voters$term==i),]
  
  output <- data.frame(row.names(XT_proj))
  output <- cbind(output,XT_proj[,1:2])
  names(output) <- c("id","wpca:d1","wpca:d2")
  output$result <- 0.05
  output$r <- 0.05
  output <- merge(output,voters_term[c("voter_id","group_id")],by.x=c("id"),by.y=c("voter_id"),all=FALSE)
  output <- merge(output,groups[c("group_id","group_abbreviation","color")],by=c("group_id"),all=FALSE)
  output <- merge(output,people[c("voter_id","voter_name","voter_surname")],by.x=c("id"),by.y=c("voter_id"),all=FALSE)
  output$name <- paste0(output$voter_surname,", ",output$voter_name," (",output$group_abbreviation,")")
  output$opacity <- 0.7
  output <- output[c("id","name","wpca:d1","wpca:d2","result","r","opacity","color")]
  output <- output[!duplicated(output$id),]
  
  TI <- X_source[!duplicated(X_source$vote_event_id),]
  TI <- TI$is_eu <= 0
  lo_limit_T <- lo_limit
  
  X_raw_T_c = X_raw[,person_I]
  X_raw_T_c[!TI,] = NA
  TI_c = X_raw_T_c
  TI_c[!is.na(TI_c)] = 1
  TI_c[is.na(TI_c)] = 0
  TI_c_w = TI_c * w
  s = apply(TI_c_w,2,sum)
  person_T_w = s/max(s)
  person_TI = person_T_w > lo_limit_T
  XT_c_scaled = (X_raw_T_c-attr(X_c_scaled_0,"scaled:center"))/attr(X_c_scaled_0,"scaled:scale")
  XT_c_scaled_w_0 = XT_c_scaled * sqrt(w)
  XT_c_scaled_w_0[is.na(XT_c_scaled_w_0)] = 0
  XT = XT_c_scaled_w_0[,person_TI]
  TI_cc = TI_c[,person_TI]
  X_cc = X[,person_TI]
  aU = abs(U)
  aX_cc = abs(X_cc)
  aX_proj_cc = t(aX_cc) %*% aU
  aXT_cc_scaled_w_0 = abs(XT)
  aXT_proj_cc = t(aXT_cc_scaled_w_0) %*% aU
  div_person_weights = aXT_proj_cc/aX_proj_cc
  XT_proj = t(XT) %*% U / div_person_weights
  XT_proj_unit = XT_proj / sqrt(apply(XT_proj^2,1,sum))
  plot(XT_proj[,1],XT_proj[,2])
  
  output_provisional <- data.frame(row.names(XT_proj))
  output_provisional <- cbind(output_provisional,XT_proj[,1:2])
  names(output_provisional) <- c("id","wpca:d1","wpca:d2")
  output_provisional$result <- 0.05
  output_provisional$r <- 0.05
  output_provisional <- merge(output_provisional,voters_term[c("voter_id","group_id")],by.x=c("id"),by.y=c("voter_id"),all=FALSE)
  output_provisional <- merge(output_provisional,groups[c("group_id","group_abbreviation","color")],by=c("group_id"),all=FALSE)
  output_provisional <- merge(output_provisional,people[c("voter_id","voter_name","voter_surname")],by.x=c("id"),by.y=c("voter_id"),all=FALSE)
  output_provisional$name <- paste0(output_provisional$voter_surname,", ",output_provisional$voter_name," (",output_provisional$group_abbreviation,")")
  output_provisional$opacity <- 0.1
  output_provisional <- output_provisional[c("id","name","wpca:d1","wpca:d2","result","r","opacity","color")]
  output_provisional <- output_provisional[!duplicated(output_provisional$id),]
  
  output <- rbind(output,output_provisional)
  
  code <- paste0("dir.create('eurozakony_wpca_",i,"')")
  eval(parse(text = code))
  
  code <- paste0("write.csv(output,'eurozakony_wpca_",i,"/voters.csv',row.names=FALSE,fileEncoding='utf-8')")
  eval(parse(text = code))
  
  rm(list=ls()[!ls()%in%c("votes","groups","voters","people","lo_limit")])
  
} # for (i in 4:7)
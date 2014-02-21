#FUNZIONE 1

#tw: file direttamente dal dump
#S: Lunghezza del subset
#L: lag (periodo precedente)
#I replace verranno fatti su un nuovo dataset tw.

RTHound=function(tw,S,L){  
  require(RecordLinkage)
  require(TextWiller)
  tw.=subset(tw, !is.na(tw$text))
  c=(round(nrow(tw.)/S)-1)
  s=c(0:c)
  for(l in 1:length(s))    
  {         
    cat("\ngruppo", l)
    if(l!=length(s))
    {
      ids=c(((S)*s[l]):((S)*s[l+1]))
      select=tw.[ids,] 
    }
    else
    {
      select=tw.[((S)*s[l]):nrow(tw.),]  
    }
    if(l>1)  
    {         
      selectPeriodoPrima=tw.[((S)*s[l]-(L+1)):((S)*s[l]-1),] 
      select=rbind(selectPeriodoPrima,select)
    }
    m=matrix(ncol=nrow(select),nrow=nrow(select))                   
    for(i in 1:(nrow(select)-1))
    {
      for(j in (i+1):nrow(select))
      {                                                           
        m[i,j]=levenshteinDist(select$text[i],select$text[j])
      }
    }
    m= as.dist(t(m))                                                       
    h=hclust(dist(m),method="complete")                                 
    tree100=cutree(h,h=100)                                            
    idClusters=sapply(unique(tree100), function(x) which(tree100==x))
    
    for (i in 1:length(idClusters))
      tw.$text[ idClusters[[i]]]=tw.$text[idClusters[[i]][1] ]
  }
  cat("\nfrequenza dei RW più significativi\n",sort(table(tw.$text),decreasing=T)[1:5])
}

#========================================================================================================

#FUNZIONE 2

install.packages("stringr")


short2longURL=function (url, ...){
    require(stringr)
    url=str_extract(tw$text,"http([[:graph:]]+)|www\\.([[:graph:]]+)")
    request_url = paste("http://expandurl.appspot.com/expand?url=",
                      url, "&format=json", sep = "")
    return(fromJSON(getURL(request_url, useragent = "twitteR",
                         ...))[["end_url"]])
}


#FUNZIONE 2.1
#la funzione da molti warning, credo uno per ciclo, ma non ho capito quale sia il problema

url=unique(subset(urls,!is.na(urls)))

dec=c(1:length(url))
for(i in 1:length(url)){
  cat(" ciclo ",i)
  if(url=="NA"){
    dec[i]=0
  }
  else{
      dec[i]=short2longURL(url[i])       #senza fare due funzione inserisco direttamente qui short2longURL in modo che funzioni per generici vettori?
  }
  save(dec,file="dec.RData")
}


#FUNZIONE 2.2

content=c(1:length(unique(decode)))
for(i in 1:length(unique(decode))){
  content[i]=try(getURL(unique(decode[i])))
  if(!is(content,"try-error")){
  }
  else{
    content[i]=0
  }
}

#FUNZIONE 2.3

extract=c(1:length(content)
for(i in 1:length(content)){
  extract[i]=ArticleExtractor(content[i])
}

corpus=normalizzaTesti(corpus,normalizzacaratteri=TRUE,tolower=TRUE,perl=FALSE,fixed=FALSE)



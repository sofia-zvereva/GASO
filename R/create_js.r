setwd("/home/regul/upload/Минцифры")

fid<-3

txt1id<-"RBi6WWqx8Ai8CfmHA1AbF3E"
txt2id<-"RCUhLPfEPL5YNaT2spdLOpb"

if (fid==1)   lce<-read.csv("lucene-export2.csv",stringsAsFactors = F) #1

if (fid==2) lce<-read.csv("input2_new.csv",stringsAsFactors = F) #2

if (fid==3)  lce<-read.csv("input3_new.csv",stringsAsFactors = F) #3

lce<-lce[lce$Type=="Class",]

names(lce)

for (i in 1:nrow(lce))
  for (j in 1:ncol(lce))
  {
    str<-lce[i,j]
    str2<-gsub("http://webprotege.stanford.edu/", "", str)
    str3<-gsub("'", "", str2)
    lce[i,j]<-str3
  }

names(lce)<-gsub("http...webprotege.stanford.edu.", "", names(lce))

names(lce)<-gsub("http...www.w3.org.2002.07.owl.", "", names(lce))
names(lce)<-gsub("http...www.w3.org.2000.01.rdf.schema.", "", names(lce))


id1<-lce$Entity
id2<-lce$Superclass.es.

id3<-names(lce)

if (fid==1) id3<-id3[c(-1,-2,-3,-120,-123,-124)]
if (fid==2) id3<-id3[c(-1,-2,-3,-150,-153,-154)]
if (fid==3) id3<-id3[c(-1,-2,-3,-194,-195)]

id_all<-sort(unique(c(id1,id2,id3)))

idn<-paste0("id",1:length(id_all))
names(idn)<-id_all

idn[txt1id]

require(stringr)

lce2<-lce
for (i in 1:nrow(lce))
  for (j in 1:ncol(lce))
  {
    str<-lce[i,j]
    #str<-"RPtbga8P9x3I3xB2B31FJ4	RDK6PsME9gz9kUub5o5LL52"
    symb<-"\t"
    str_detect(str, "\t", negate = FALSE)
    
    if (!is.na(str))
    {
            #if (grepl(str, "R", fixed = F)) # несколько idшников
            if(str_detect(str, symb, negate = FALSE))
            {
              a<- strsplit(str,split=symb, fixed=TRUE)[[1]]
              id3<-c()
              for (ai in a)
              {
                if (ai %in% id_all) id3<-c(id3,idn[ai])
              }
              str2<-paste(id3,collapse = "|")
              lce2[i,j]<-str2
            } else if (str %in% id_all) lce2[i,j]<-idn[str]
    }
  }

nn<-names(lce2)
for (i in 1:length(nn)) if (nn[i] %in% id_all) nn[i]<-idn[nn[i]]
names(lce2)<-nn


tt<-table(lce2$Superclass.es.)

cols<-rainbow(6)

tt<-sort(tt,decreasing = T)

lce2$col<-cols[6]

cols2<-cols[1:5]

names(cols2)<-names(tt)[1:5]

for (id in names(tt)[1:5]) lce2$col[lce2$Superclass.es.==id]<-cols2[id]

js<-c()
js<-c(js,"export default {","  nodes: [")
nodes<-sort(unique(lce2$Entity,lce2$Superclass.es.))
for (n in nodes){
  #col<-"#101010"
  id<-substring(n, 3)
  
  r<-lce2[which(lce2$Entity==n),]
  if (nrow(r)>0) 
  {
    l<-r$label
    col<-r$col
  }
  
  #txt1<-"123" #str_remove(r[idn[txt1id]],"'")
  #txt2<-"456" #str_remove(r[idn[txt2id]],"'")
  
  txt1<-str_remove(r[idn[txt1id]],"'")
  txt2<-str_remove(r[idn[txt2id]],"'")
  
  txt1<-str_remove(txt1,"\n")
  txt2<-str_remove(txt2,"\n")
  txt1<-str_remove(txt1,"\r")
  txt2<-str_remove(txt2,"\r")
  
  txt1<-gsub("[^[:alnum:][:blank:]+?().,&/\\-]", "", txt1)
  txt2<-gsub("[^[:alnum:][:blank:]+?().,&/\\-]", "", txt2)
  #txt2<-"222"
  
  s<-paste0("{ id: ",id,", l: '",l,"', c: '",col,"', txt1: '",txt1,"', txt2: '",txt2,"' }")
  #s<-paste0("{ id: ",id,", l: '",l,"', c: '",col,"' }")
  if (n!=nodes[length(nodes)])  s<-paste0(s,",")
    
  js<-c(js,s)
}
js<-c(js,"  ],","  edges: [")

for (i in 1:nrow(lce2))
{
  r<-lce2[i,]
  id1<-substring(r$Entity, 3)
  id2<-substring(r$Superclass.es., 3)
  
  s<-paste0("    { id: ",i-1,", s: ",id1,", t: ",id2,"}")
  if (i!=nrow(lce2)) s<-paste0(s,",")
  js<-c(js,s)
}
js<-c(js,"  ]","}")

fileConn<-file(paste0("SampleGraph",fid,".js"))
writeLines(js, fileConn)
close(fileConn)


if (fid==3) fid<-4
fileConn<-file(paste0("SampleGraph",fid,".js"))
writeLines(js, fileConn)
close(fileConn)


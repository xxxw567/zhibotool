
###############################################################
#bilibili new use  
###############################################################

savehtml<-function(sec,rmid,visitid) {
  library("httr")
  library("jsonlite")
  ################################################
  page<-list()
  i=1
  time=Sys.time()
  while (i <=sec ) {
    URL="https://api.live.bilibili.com/ajax/msg"
    result<-POST(URL, body = list(
      "roomid"   = rmid,
      "visit_id" =visitid,
      'csrf_token'=''
      )
    )
    outcome<-jsonlite::fromJSON(txt=content(result, as="text"),simplifyDataFrame=TRUE)
    result<-as.data.frame(outcome$data$room)
    #pagename<-paste0("pg",format(Sys.time(), "%H_%M_%S"))
    print(Sys.time()) #record
    page[[i]]<-result
    if ((Sys.time()-time)==1) i=i+1
  }
  ##################################################################
  
  temp=page[[1]]
  temp$medal<-as.character(temp$medal)
  temp$title<-as.character(temp$title)
  temp$user_level<-as.character(temp$user_level)
  temp$activity_info<-as.character(temp$activity_info)

  for (i in 2:length(page)) {
    temp2<-page[[i]]
    temp2$medal<-as.character(temp2$medal)
    temp2$title<-as.character(temp2$title)
    temp2$user_level<-as.character(temp2$user_level)
    temp2$activity_info<-as.character(temp2$activity_info)
    #rownames(temp2)<-1*10-10+(1:nrow(temp2))
    temp<-rbind(temp,temp2)
  }

  temp<-temp[!duplicated(temp), ]
  return(temp)
} 

  

###############################################################
#danmuji  
###############################################################
setwd("C:/Users/xiayi_000/Documents/弹幕姬")


danmuji<-function(filename) {
library("stringr")
type<-vector()
time<-vector()
id<-vector()
item<-vector()
quant<-vector()
text<-vector()
idye<-vector()
idguan<-vector()

data<-readLines(filename,encoding = "UTF-8")
for (i in 1:length(data)) {
  if (!str_detect(data[i],"收到")) next
  if ( str_detect(data[i],"道具")) {
    type[i]="daoju"
    time[i]=str_extract(data[i],"[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")
    id[i]=str_trim(str_sub(data[i],
                                str_locate(data[i],"道具:")[2]+1,
                                str_locate(data[i],"赠送")[1]-1))	
    id[i]=str_replace_all(id[i],"[爷]|[管]","")	
    
    item[i]=str_trim(str_sub(data[i],
                                  str_locate(data[i],"赠送的:")[2]+1,
                                  str_locate(data[i],"x")[1]-1))
    quant[i]=str_trim(str_sub(data[i],
                                  str_locate(data[i],"x")[2]+1))
    idye[i]=str_detect(data[i],"[爷]")
    idguan[i]=str_detect(data[i],"[管]")
    text[i]=NA
  } else if (str_detect(data[i],"彈幕")) {
    type[i]="danmu"
    time[i]=str_extract(data[i],"[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")
    id[i]=str_trim(str_sub(data[i],
                                str_locate(data[i],"彈幕:")[2]+1,
                                str_locate(data[i],"說")[1]-1))	
    id[i]=str_replace_all(id[i],"[爷]|[管]","")	
    item[i]=NA
    quant[i]=NA
    text[i]=str_trim(str_sub(data[i],str_locate(data[i],"說:")[2]+1))
    idye[i]=str_detect(data[i],"[爷]")
    idguan[i]=str_detect(data[i],"[管]")
  }
}

datafin<-as.data.frame(cbind(type,time,id,item,quant,text,idye,idguan)) 
datafin<-datafin[!is.na(datafin$type),]

return(datafin)
}


#this function cut the file into image
cutvideo<-function(filename,fps) {
  unlink('photos', recursive=TRUE)
  dir.create(file.path("photos"), showWarnings = FALSE)
  cmd=paste0("ffmpeg -i \"",filename,"\" -r ",fps," photos/output_%05d.png")
  system(cmd)
}


###############################################################
#test Zone

############use bilibili API 


page<-savehtml(sec=600,rmid=5096,visitid="anwxaam4gnb4")

setwd("C:/Users/xiayi_000/OneDrive/主播视频分析/test")

#####################################################
#do it 

cutvideo(filename="test.flv" ,fps=1)
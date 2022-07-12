#load dataframe "daramad khanevar99"
#The data of this project are related to Kermanshah, Kurdistan, Chaharmahal-o- Bakhtiari, Lorestan, Ilam, Kohkiluyeh-va-Boyer-Ahmad provinces.
khanevar.df<-read.csv("Data6.csv") 
library(dplyr)
final.df<-select(khanevar.df,-hichkodam,-bargh,-address,-Ostan,-gorooh)
final.df$SarparstTahsil[is.na(final.df$SarparstTahsil)]<-0
final.df$SarparstMadrak[is.na(final.df$SarparstMadrak)]<-0
summary(final.df)
#calculate deciles of dataset
quantile(final.df$Target, probs = seq(.9, by = .1))
#place each value into a decile
final.df$decile <- ntile(final.df$Target, 10)
as.data.frame(table(final.df$decile))
final.df$decile[final.df$decile=="1"]<-0
final.df$decile[final.df$decile=="2"]<-0
final.df$decile[final.df$decile=="3"]<-0
final.df$decile[final.df$decile=="4"]<-0
final.df$decile[final.df$decile=="5"]<-0
final.df$decile[final.df$decile=="6"]<-0
final.df$decile[final.df$decile=="7"]<-0
final.df$decile[final.df$decile=="8"]<-0
final.df$decile[final.df$decile=="9"]<-0
final.df$decile[final.df$decile=="10"]<-1
as.data.frame(table(final.df$decile))
#barchart of Tedadaza vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$TedadAza),FUN=mean)
names(data.for.plot)<-c("TedadAza","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$TedadAza,xlab="TedadAza",ylab="%of decile")
#barchart of NoeKhanevar vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$NoeKhanevar),FUN=mean)
names(data.for.plot)<-c("NoeKhanevar","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$NoeKhanevar,xlab="NoeKhanevar",ylab="%of decile")
#barchart of SarparstJensiat vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstJensiat),FUN=mean)
names(data.for.plot)<-c("SarparstJensiat","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstJensiat,xlab="SarparstJensiat",ylab="%of decile")
#barchart of SarparstSen vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstSen),FUN=mean)
names(data.for.plot)<-c("SarparstSen","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstSen,xlab="SarparstSen",ylab="%of decile")
final.df$SarparstSen[final.df$SarparstSen=="18"]<-24
final.df$SarparstSen[final.df$SarparstSen=="19"]<-24
final.df$SarparstSen[final.df$SarparstSen=="20"]<-24
final.df$SarparstSen[final.df$SarparstSen=="21"]<-24
final.df$SarparstSen[final.df$SarparstSen=="22"]<-24
final.df$SarparstSen[final.df$SarparstSen=="23"]<-24
final.df$SarparstSen[final.df$SarparstSen=="24"]<-24
final.df$SarparstSen[final.df$SarparstSen=="25"]<-24
final.df$SarparstSen[final.df$SarparstSen=="26"]<-24
final.df$SarparstSen[final.df$SarparstSen=="27"]<-24
final.df$SarparstSen[final.df$SarparstSen=="28"]<-24
final.df$SarparstSen[final.df$SarparstSen=="29"]<-24
final.df$SarparstSen[final.df$SarparstSen=="30"]<-24
final.df$SarparstSen[final.df$SarparstSen=="31"]<-35
final.df$SarparstSen[final.df$SarparstSen=="32"]<-35
final.df$SarparstSen[final.df$SarparstSen=="33"]<-35
final.df$SarparstSen[final.df$SarparstSen=="34"]<-35
final.df$SarparstSen[final.df$SarparstSen=="35"]<-35
final.df$SarparstSen[final.df$SarparstSen=="36"]<-35
final.df$SarparstSen[final.df$SarparstSen=="37"]<-35
final.df$SarparstSen[final.df$SarparstSen=="38"]<-35
final.df$SarparstSen[final.df$SarparstSen=="39"]<-35
final.df$SarparstSen[final.df$SarparstSen=="40"]<-35
final.df$SarparstSen[final.df$SarparstSen=="41"]<-50
final.df$SarparstSen[final.df$SarparstSen=="42"]<-50
final.df$SarparstSen[final.df$SarparstSen=="43"]<-50
final.df$SarparstSen[final.df$SarparstSen=="44"]<-50
final.df$SarparstSen[final.df$SarparstSen=="45"]<-50
final.df$SarparstSen[final.df$SarparstSen=="46"]<-50
final.df$SarparstSen[final.df$SarparstSen=="47"]<-50
final.df$SarparstSen[final.df$SarparstSen=="48"]<-50
final.df$SarparstSen[final.df$SarparstSen=="49"]<-50
final.df$SarparstSen[final.df$SarparstSen=="50"]<-50
final.df$SarparstSen[final.df$SarparstSen=="51"]<-50
final.df$SarparstSen[final.df$SarparstSen=="52"]<-50
final.df$SarparstSen[final.df$SarparstSen=="53"]<-50
final.df$SarparstSen[final.df$SarparstSen=="54"]<-50
final.df$SarparstSen[final.df$SarparstSen=="55"]<-50
final.df$SarparstSen[final.df$SarparstSen=="56"]<-50
final.df$SarparstSen[final.df$SarparstSen=="57"]<-50
final.df$SarparstSen[final.df$SarparstSen=="58"]<-50
final.df$SarparstSen[final.df$SarparstSen=="59"]<-50
final.df$SarparstSen[final.df$SarparstSen=="60"]<-50
final.df$SarparstSen[final.df$SarparstSen=="61"]<-80
final.df$SarparstSen[final.df$SarparstSen=="62"]<-80
final.df$SarparstSen[final.df$SarparstSen=="63"]<-80
final.df$SarparstSen[final.df$SarparstSen=="64"]<-80
final.df$SarparstSen[final.df$SarparstSen=="65"]<-80
final.df$SarparstSen[final.df$SarparstSen=="66"]<-80
final.df$SarparstSen[final.df$SarparstSen=="67"]<-80
final.df$SarparstSen[final.df$SarparstSen=="68"]<-80
final.df$SarparstSen[final.df$SarparstSen=="69"]<-80
final.df$SarparstSen[final.df$SarparstSen=="70"]<-80
final.df$SarparstSen[final.df$SarparstSen=="71"]<-80
final.df$SarparstSen[final.df$SarparstSen=="72"]<-80
final.df$SarparstSen[final.df$SarparstSen=="73"]<-80
final.df$SarparstSen[final.df$SarparstSen=="74"]<-80
final.df$SarparstSen[final.df$SarparstSen=="75"]<-80
final.df$SarparstSen[final.df$SarparstSen=="76"]<-80
final.df$SarparstSen[final.df$SarparstSen=="77"]<-80
final.df$SarparstSen[final.df$SarparstSen=="78"]<-80
final.df$SarparstSen[final.df$SarparstSen=="79"]<-80
final.df$SarparstSen[final.df$SarparstSen=="80"]<-80
final.df$SarparstSen[final.df$SarparstSen=="81"]<-80
final.df$SarparstSen[final.df$SarparstSen=="82"]<-80
final.df$SarparstSen[final.df$SarparstSen=="83"]<-80
final.df$SarparstSen[final.df$SarparstSen=="84"]<-80
final.df$SarparstSen[final.df$SarparstSen=="85"]<-80
final.df$SarparstSen[final.df$SarparstSen=="86"]<-80
final.df$SarparstSen[final.df$SarparstSen=="87"]<-80
final.df$SarparstSen[final.df$SarparstSen=="88"]<-80
final.df$SarparstSen[final.df$SarparstSen=="89"]<-80
final.df$SarparstSen[final.df$SarparstSen=="90"]<-80
final.df$SarparstSen[final.df$SarparstSen=="91"]<-80
final.df$SarparstSen[final.df$SarparstSen=="92"]<-80
final.df$SarparstSen[final.df$SarparstSen=="93"]<-80
final.df$SarparstSen[final.df$SarparstSen=="94"]<-80
final.df$SarparstSen[final.df$SarparstSen=="95"]<-80
final.df$SarparstSen[final.df$SarparstSen=="96"]<-80
final.df$SarparstSen[final.df$SarparstSen=="97"]<-80
final.df$SarparstSen[final.df$SarparstSen=="98"]<-80
final.df$SarparstSen[final.df$SarparstSen=="99"]<-80
final.df$SarparstSen[final.df$SarparstSen=="100"]<-80
##barchart of SarparstSen vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstSen),FUN=mean)
names(data.for.plot)<-c("SarparstSen","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstSen,xlab="SarparstSen",ylab="%of decile")
##barchart of Sarparsavad vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstSavad),FUN=mean)
names(data.for.plot)<-c("SarparstSavad","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstSavad,xlab="SarparstSavad",ylab="%of decile")
##barchart of Sarparttahsil vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstTahsil),FUN=mean)
names(data.for.plot)<-c("SarparstTahsil","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstTahsil,xlab="SarparstTahsil",ylab="%of decile")
##barchart of Sarpartmadrak vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstMadrak),FUN=mean)
names(data.for.plot)<-c("SarparstMadrak","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstMadrak,xlab="SarparstMadrak",ylab="%of decile")
##barchart of SarpartFaaliat vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstFaaliat),FUN=mean)
names(data.for.plot)<-c("SarparstFaaliat","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstFaaliat,xlab="SarparstFaaliat",ylab="%of decile")
##barchart of SarpartZanashoyi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SarparstZanashoyi),FUN=mean)
names(data.for.plot)<-c("SarparstZanashoyi","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$SarparstZanashoyi,xlab="SarparstZanashoyi",ylab="%of decile")
##barchart of NahveTasarof vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$NahveTasarof),FUN=mean)
names(data.for.plot)<-c("NahveTasarof","meandecile")
barplot(data.for.plot$meandecile*100,names.arg=data.for.plot$NahveTasarof,xlab="NahveTasarof",ylab="%of decile")
##barchart of TedadOtagh vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$TedadOtagh),FUN=mean)
names(data.for.plot)<-c("TedadOtagh","decile")
barplot(data.for.plot$decile*100,names.arg=data.for.plot$TedadOtagh,xlab="TedadOtagh",ylab="%of decile")
##barchart of SatheZirbana vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$SatheZirbana),FUN=mean)
names(data.for.plot)<-c("SatheZirbana","decile")
barplot(data.for.plot$decile*100,names.arg=data.for.plot$SatheZirbana,xlab="SatheZirbana",ylab="%of decile")
##barchart of NoeEskelet vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$NoeEskelet),FUN=mean)
names(data.for.plot)<-c("NoeEskelet","decile")
barplot(data.for.plot$decile*100,names.arg=data.for.plot$NoeEskelet,xlab="NoeEskelet",ylab="%of decile")
##barchart of mashin vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$mashin),FUN=sum)
names(data.for.plot)<-c("mashin","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$mashin,xlab="mashin",ylab="freq decile")
##barchart of motor vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$motor),FUN=sum)
names(data.for.plot)<-c("motor","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$motor,xlab="motor",ylab="freq decile")
##barchart of docharkhe vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$docharkhe),FUN=sum)
names(data.for.plot)<-c("docharkhe","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$docharkhe,xlab="docharkhe",ylab="freq decile")
##barchart of radio vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$radio),FUN=sum)
names(data.for.plot)<-c("radio","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$radio,xlab="radio",ylab="freq decile")
##barchart of radiozabt vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$radiozabt),FUN=sum)
names(data.for.plot)<-c("radiozabt","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$radiozabt,xlab="radiozabt",ylab="freq decile")
##barchart of tv vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$tv),FUN=sum)
names(data.for.plot)<-c("tv","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$tv,xlab="tv",ylab="freq decile")
##barchart of tvrangi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$tvrangi),FUN=sum)
names(data.for.plot)<-c("tvrangi","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$tvrangi,xlab="tvrangi",ylab="freq decile")
##barchart of video vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$video),FUN=sum)
names(data.for.plot)<-c("video","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$video,xlab="video",ylab="freq decile")
##barchart of computer vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$computer),FUN=sum)
names(data.for.plot)<-c("computer","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$computer,xlab="computer",ylab="freq decile")
##barchart of mobile vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$mobile),FUN=sum)
names(data.for.plot)<-c("mobile","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$mobile,xlab="mobile",ylab="freq decile")
##barchart of freezer vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$freezer),FUN=sum)
names(data.for.plot)<-c("freezer","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$freezer,xlab="freezer",ylab="freq decile")
##barchart of yakhchal vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$yakhchal),FUN=sum)
names(data.for.plot)<-c("yakhchal","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$yakhchal,xlab="yakhchal",ylab="freq decile")
##barchart of yakhchalfreezer vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$yakhchalfreezer),FUN=sum)
names(data.for.plot)<-c("yakhchalfreezer","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$yakhchalfreezer,xlab="yakhchalfreezer",ylab="freq decile")
##barchart of ojaghgaz vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$ojaghgaz),FUN=sum)
names(data.for.plot)<-c("ojaghgaz","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$ojaghgaz,xlab="ojaghgaz",ylab="freq decile")
##barchart of jarobarghi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$jarobarghi),FUN=sum)
names(data.for.plot)<-c("jarobarghi","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$jarobarghi,xlab="jarobarghi",ylab="freq decile")
##barchart of lebasshoyi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$lebasshoyi),FUN=sum)
names(data.for.plot)<-c("lebasshoyi","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$lebasshoyi,xlab="lebasshoyi",ylab="freq decile")
##barchart of khayati vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$khayati),FUN=sum)
names(data.for.plot)<-c("khayati","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$khayati,xlab="khayati",ylab="freq decile")
##barchart of panke vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$panke),FUN=sum)
names(data.for.plot)<-c("panke","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$panke,xlab="panke",ylab="freq decile")
##barchart of coolerabimoteharek vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$coolerabimoteharek),FUN=sum)
names(data.for.plot)<-c("coolerabimoteharek","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$coolerabimoteharek,xlab="coolerabimoteharek",ylab="freq decile")
##barchart of coolergazimoteharek vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$coolergazimoteharek),FUN=sum)
names(data.for.plot)<-c("coolergazimoteharek","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$coolergazimoteharek,xlab="coolergazimoteharek",ylab="freq decile")
##barchart of zarfshoyi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$zarfshoyi),FUN=sum)
names(data.for.plot)<-c("zarfshoyi","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$zarfshoyi,xlab="zarfshoyi",ylab="freq decile")
##barchart of microfer vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$microfer),FUN=sum)
names(data.for.plot)<-c("microfer","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$microfer,xlab="microfer",ylab="freq decile")
##barchart of lolekeshiab vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$lolekeshiab),FUN=sum)
names(data.for.plot)<-c("lolekeshiab","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$lolekeshiab,xlab="lolekeshiab",ylab="freq decile")
##barchart of gazlolekeshi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$gazlolekeshi),FUN=sum)
names(data.for.plot)<-c("gazlolekeshi","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$gazlolekeshi,xlab="gazlolekeshi",ylab="freq decile")
##barchart of telephone vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$telephone),FUN=sum)
names(data.for.plot)<-c("telephone","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$telephone,xlab="telephone",ylab="freq decile")
##barchart of internet vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$internet),FUN=sum)
names(data.for.plot)<-c("internet","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$internet,xlab="internet",ylab="freq decile")
##barchart of hamam vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$hamam),FUN=sum)
names(data.for.plot)<-c("hamam","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$hamam,xlab="hamam",ylab="freq decile")
##barchart of ashpazkhane vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$ashpazkhane),FUN=sum)
names(data.for.plot)<-c("ashpazkhane","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$ashpazkhane,xlab="ashpazkhane",ylab="freq decile")
##barchart of coolerabisabet vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$coolerabisabet),FUN=sum)
names(data.for.plot)<-c("coolerabisabet","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$coolerabisabet,xlab="coolerabisabet",ylab="freq decile")
##barchart of borodatmarkazi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$borodatmarkazi),FUN=sum)
names(data.for.plot)<-c("borodatmarkazi","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$borodatmarkazi,xlab="borodatmarkazi",ylab="freq decile")
##barchart of package vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$package),FUN=sum)
names(data.for.plot)<-c("package","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$package,xlab="package",ylab="freq decile")
##barchart of coolergazisabet vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$coolergazisabet),FUN=sum)
names(data.for.plot)<-c("coolergazisabet","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$coolergazisabet,xlab="coolergazisabet",ylab="freq decile")
##barchart of fazelabshahri vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$fazelabshahri),FUN=sum)
names(data.for.plot)<-c("fazelabshahri","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$fazelabshahri,xlab="fazelabshahri",ylab="freq decile")
##barchart of hararatmarkazi vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$hararatmarkazi),FUN=sum)
names(data.for.plot)<-c("hararatmarkazi","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$hararatmarkazi,xlab="hararatmarkazi",ylab="freq decile")
##barchart of nsokhtpokhtpaz vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$nsokhtpokhtpaz),FUN=sum)
names(data.for.plot)<-c("nsokhtpokhtpaz","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$nsokhtpokhtpaz,xlab="nsokhtpokhtpaz",ylab="freq decile")
##barchart of nsokhtgarma vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$nsokhtgarma),FUN=sum)
names(data.for.plot)<-c("nsokhtgarma","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$nsokhtgarma,xlab="nsokhtgarma",ylab="freq decile")
##barchart of nsokhtabgarm vs decile
data.for.plot<-aggregate(final.df$decile,by=list(final.df$nsokhtabgarm),FUN=sum)
names(data.for.plot)<-c("nsokhtabgarm","decile")
barplot(data.for.plot$decile,names.arg=data.for.plot$nsokhtabgarm,xlab="nsokhtabgarm",ylab="freq decile")
##boxplot of decile for different values of HazineKhanevar
boxplot(final.df$HazineKhoraki/10000000~final.df$decile,xlab = "decile",ylab = "HazineKhoraki")
boxplot(final.df$HazineNoshidani/10000000~final.df$decile,xlab = "decile",ylab = "HazineNoshidani")
boxplot(final.df$HazinePoshak/10000000~final.df$decile,xlab = "decile",ylab = "HazinePoshak")
boxplot(final.df$HazineMaskan/10000000~final.df$decile,xlab = "decile",ylab = "HazineMaskan")
boxplot(final.df$HazineLavazemKhanegi/10000000~final.df$decile,xlab = "decile",ylab = "HazineLavazemKhanegi")
boxplot(final.df$HazineDarmani/10000000~final.df$decile,xlab = "decile",ylab = "HazineDarmani")
boxplot(final.df$HazineHamlonaghl/10000000~final.df$decile,xlab = "decile",ylab = "HazineHamlonaghl")
boxplot(final.df$HazineErtebatat/10000000~final.df$decile,xlab = "decile",ylab = "HazineErtebatat")
boxplot(final.df$HazineTafrihatFarhangi/10000000~final.df$decile,xlab = "decile",ylab = "HazineTafrihatFarhangi")
boxplot(final.df$HazineGhazaAmade/10000000~final.df$decile,xlab = "decile",ylab = "HazineGhazaAmade")
boxplot(final.df$HazineKalaMotefaregheh/10000000~final.df$decile,xlab = "decile",ylab = "HazineKalaMotefaregheh")
boxplot(final.df$HazineKalaBadavam/10000000~final.df$decile,xlab = "decile",ylab = "HazineKalaBadavam")
##drop some variable
names(final.df)
data.df<-select(final.df,-SarparstTahsil,-motor,-docharkhe,-radio,-radiozabt,
                 -tv,-coolerabimoteharek,-coolergazimoteharek,-zarfshoyi,
                 -microfer,-borodatmarkazi,-package,-coolergazisabet,-hararatmarkazi)
data.df$nsokhtgarma[data.df$nsokhtgarma=="11"]<-10
data.df$nsokhtgarma[data.df$nsokhtgarma=="13"]<-10
data.df$nsokhtabgarm[data.df$nsokhtabgarm=="21"]<-20
data.df$nsokhtabgarm[data.df$nsokhtabgarm=="23"]<-20
data.df$nsokhtabgarm[data.df$nsokhtabgarm=="25"]<-20
data.df$TotalHazine<-(data.df$HazineKalaBadavam/12
                      +data.df$HazineKalaMotefaregheh
                      +data.df$HazineGhazaAmade
                      +data.df$HazineTafrihatFarhangi
                      +data.df$HazineErtebatat
                      +data.df$HazineHamlonaghl
                      +data.df$HazineDarmani
                      +data.df$HazineLavazemKhanegi
                      +data.df$HazineMaskan
                      +data.df$HazinePoshak
                      +data.df$HazineNoshidani
                      +data.df$HazineKhoraki)
data.df<-select(data.df,-c(38,39,40,41,42,43,44,45,46,47,48,49))
##simple heatmap of correlations (with values)
library(gplots)
heatmap.2(cor(data.df),Rowv = FALSE, Colv = FALSE, dendrogram = "none"
          ,cellnote =round(cor(data.df),2),notecol = "black",key=FALSE
          ,trace='none',margins = c(5,5))
data.df<-select(data.df,-yakhchal)
library(MASS)
par(mfcol=c(2,1))
parcoord(data.df[data.df$decile==0,-39],main="decil=0")
parcoord(data.df[data.df$decile==1,-39],main="decil=1")
#use set seed() to get the same partitions when re-running the R code.
set.seed(12345)
#partitioning into training(%60),validation(%30) and test(%10)
#randomly sample 60% of the row IDs for training
train.rows<-sample(rownames(data.df),dim(data.df)[1]*0.6)
#sample 30%of the row IDs into the validation set, drawing only from record
#not already in the training set
#use setdiff() to find record not already in the training set
valid.rows<-sample(setdiff(rownames(data.df),train.rows),dim(data.df)[1]*0.3)
#assign the remaining 10% row IDs serve as test
test.rows<-setdiff(rownames(data.df),union(train.rows,valid.rows))
#create the 3 data frames by collecting all columns from the appropriate rows
train.data<-data.df[train.rows,]
valid.data<-data.df[valid.rows,]
test.data<-data.df[test.rows,]
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
# use rpart() to run a classification tree.
default.ct<-rpart(as.factor(decile)~.,data = train.data,method = "class")
#use prp() to plot the tree.
prp(default.ct,type = 1,extra = 1,under=TRUE,split.font = 1,varlen = -10)
#classify record in the validation data.
#set argument type="class" in predict() to generate predicted class mumbership.
default.ct.point.pred.train<-predict(default.ct,train.data,type="class")
confusionMatrix(default.ct.point.pred.train,as.factor(train.data$decile))
default.ct.point.pred.valid<-predict(default.ct,valid.data,type="class")
confusionMatrix(default.ct.point.pred.valid,as.factor(valid.data$decile))
#deeper tree
deeper.ct<-rpart(as.factor(decile)~.,data=train.data,method = "class",cp=0,minsplit=1)
#count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var=="<leaf>"])
#plot deep tree
prp(deeper.ct,type = 1,extra = 1,under = TRUE,split.font = 1,varlen = -10,
    box.col=ifelse(deeper.ct$frame$var=="<leaf>",'gray','white'))
deeper.ct.point.pred.train<-predict(deeper.ct,train.data,type="class")
confusionMatrix(deeper.ct.point.pred.train,as.factor(train.data$decile))
deeper.ct.point.pred.valid<-predict(deeper.ct,valid.data,type="class")
confusionMatrix(deeper.ct.point.pred.valid,as.factor(valid.data$decile))
###
train.data$SarparstSen<-as.factor(train.data$SarparstSen)
valid.data$SarparstSen<-as.factor(valid.data$SarparstSen)
test.data$SarparstSen<-as.factor(test.data$SarparstSen)
# use rpart() to run a classification tree.
default.ct<-rpart(SarparstSen~.,data = train.data,method = "class")
#use prp() to plot the tree.
prp(default.ct,type = 1,extra = 1,under=TRUE,split.font = 1,varlen = -10)
#classify record in the validation data.
#set argument type="class" in predict() to generate predicted class mumbership.
default.ct.point.pred.train<-predict(default.ct,train.data,type="class")
confusionMatrix(default.ct.point.pred.train,train.data$SarparstSen)
default.ct.point.pred.valid<-predict(default.ct,valid.data,type="class")
confusionMatrix(default.ct.point.pred.valid,valid.data$SarparstSen)
#deeper tree
deeper.ct<-rpart(SarparstSen~.,data=train.data,method = "class",cp=0,minsplit=1)
#count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var=="<leaf>"])
#plot deep tree
prp(deeper.ct,type = 1,extra = 1,under = TRUE,split.font = 1,varlen = -10,
    box.col=ifelse(deeper.ct$frame$var=="<leaf>",'gray','white'))
deeper.ct.point.pred.train<-predict(deeper.ct,train.data,type="class")
confusionMatrix(deeper.ct.point.pred.train,train.data$SarparstSen)
deeper.ct.point.pred.valid<-predict(deeper.ct,valid.data,type="class")
confusionMatrix(deeper.ct.point.pred.valid,valid.data$SarparstSen)
library(randomForest)
rf<-randomForest(as.factor(decile)~.,data=train.data,ntree=500,mtry=4,
                 nodesize=5,importance=TRUE)
varImpPlot(rf,type=1)
rf.pred<-predict(rf,valid.data)
confusionMatrix(rf.pred,as.factor(valid.data$decile))
###logit-reg
logit.reg<-glm(as.factor(decile)~.,data = train.data,family = "binomial")
options(scipen = 999)
summary(logit.reg)
logit.reg.pred<-predict(logit.reg,valid.data[,-38],type = "response")
data.frame(actual=as.factor(valid.data$decile[1:10]),predicted=logit.reg.pred[1:10])
library(gains)
gain<-gains(valid.data$decile,logit.reg.pred,groups = length(logit.reg.pred))
#plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.data$decile))~c(0,gain$cume.obs),
       xlab="# cases",ylab="cumulative",main="",type="l")
lines(c(0,sum(valid.data$decile))~c(0,dim(valid.data)[1]),lty=2)
#decile..
heights<-gain$mean.resp/mean(valid.data$decile)
midpoints<-barplot(heights,names.arg = gain$depth,ylim=c(0,39)
                   ,xlab="percential",ylab = "mean decile",main="decile-wise lift chart")

#K-NN
#initialize normalized training.valid,complate data frames to orginals
#use preprocess() from the caret package to normalize decile and sarparstsen 
norm.values<-preProcess(train.data[,1:2],method =c("center", "scale"))
train.data[,1:2]<-predict(norm.values,train.data[,1:2])
valid.data[,1:2]<-predict(norm.values,valid.data[,1:2])
data.df[,1:2]<-predict(norm.values,data.df[,1:2])
#initilize a dataframe with two columns: k, and accuracy.
new.df<-data.frame(test.data[,1:2])
new.norm.df<-predict(norm.values,new.df)
library(class)
library(FNN)
#use knn()
nn<-knn(train=train.data[,1:2],test=new.norm.df,cl=train.data[, 3],k=3)
row.names(train.data)[attr(nn,"nn.index")]
nn
accuracy.df<-data.frame(k=seq(1,14,1),accuracy=rep(0,14))
#compute knn for different k on validation
for(i in 1:14){
  knn.pred<-knn(train.data[,1:2],valid.data[,1:2],
                             cl=train.data[, 3],k=i)
  accuracy.df[i,2]<-confusionMatrix(knn.pred,valid.data[, 3])$overall[1]}
########################################
library(neuralnet)
Nn<-neuralnet(as.factor(decile)~NoeKhanevar+TedadAza+SarparstJensiat+SarparstSen+
                SarparstSavad+SarparstMadrak+SarparstFaaliat+SarparstZanashoyi
              +NahveTasarof+TedadOtagh+SatheZirbana+mashin+tvrangi+video+
                computer+mobile+freezer+yakhchalfreezer+ojaghgaz+jarobarghi
              +lebasshoyi+khayati+panke+lolekeshiab+gazlolekeshi+telephone+
                internet+hamam+ashpazkhane+coolerabisabet+fazelabshahri+
                nsokhtpokhtpaz+nsokhtgarma+nsokhtabgarm+Target+TotalHazine,
              data = train.data,linear.output = F,hidden =c(10,5))
plot(Nn)
trai.data<-select(train.data,-decile)
training.prediction=compute(Nn,trai.data[,1:38])
training.class=apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(training.class),as.factor(train.data$decile))

valid.prediction=compute(Nn,valid.data[,1:39])
valid.class=apply(valid.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(valid.class),as.factor(valid.data$decile))
##final model.
test.prediction=compute(Nn,test.data[,1:39])
test.class=apply(test.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(test.class),as.factor(test.data$decile))

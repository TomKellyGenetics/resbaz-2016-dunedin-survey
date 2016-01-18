setwd("~/Downloads/ResBaz2016/")
pdf("Research_Bazaar_Dunedin_Survey.pdf", width=12, height=8)
data <- read.csv("ResBaz Dunedin Global Form Data Request - Request.csv", check.names=FALSE)
sex<-table(data[,4])[c(2:4)]
barplot(sex)
title(colnames(data)[4])
age<-table(data[,5])[c(4:8,2,1)]
age[6]<-age[6]+1
names(age)<-lapply(strsplit(names(age), split=" "), function(x) x[5])
names(age)[6]<-"41+"
names(age)[7]<-"N/A"
barplot(age)
title(colnames(data)[5])
institution <- table(data[,7])[c(2:5, 1)]
names(institution)[5]<-"N/A"
barplot(institution)
title(colnames(data)[7])
stage<-NA
stage[1]<-sum(table(data[,8])[c(2,4,5)]) #early career
stage[2]<-sum(table(data[,8])[c(3,10,11)]) # mid career
stage[3]<-sum(table(data[,8])[c(21,26)]) # research assistant
stage[4]<-sum(table(data[,8])[c(23)]) # undergrad
stage[5]<-sum(table(data[,8])[c(6,7,24)]) # honours
stage[6]<-sum(table(data[,8])[c(8,9,26)]) # masters
stage[7]<-sum(table(data[,8])[c(12:18)]) # phd
stage[8]<-sum(table(data[,8])[c(19:20)]) # postdoc
stage[9]<-sum(table(data[,8])[c(22)]) # senior
stage[10]<-sum(table(data[,8])[c(11,13)]) # professional
stage[11]<-sum(table(data[,8])[c(1)]) # na
names(stage)<-c("early career", "mid career", "research assistant", "undergrad", "honours", "masters", "phd", "postdoc", "senior", "professional", "N/A")
#barplot(stage)
x <- barplot(stage, xaxt="n")
labs <- names(stage)
text(cex=0.8, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[8])
stage2<-NA
stage2[1]<-sum(table(data[,8])[c(6,7,24)]) # honours
stage2[2]<-sum(table(data[,8])[c(8,9,26)]) # masters
stage2[3]<-sum(table(data[,8])[c(12:13)]) # phd-1
stage2[4]<-sum(table(data[,8])[c(14:16)]) # phd-2
stage2[5]<-sum(table(data[,8])[c(17:18)]) # phd-3+
stage2[6]<-sum(table(data[,8])[c(19:20)]) # postdoc
names(stage2)<-c("hons", "masters", "phd-1", "phd-2", "phd-3+", "postdoc")
barplot(stage2)
title(colnames(data)[8])
years<-data[,9]
years[1]<-0
years[12]<-2
years[42]<-1
years[64]<-12
years[66]<-5
years[83]<-2
years[93]<-3
years<-sapply(strsplit(as.character(years), split="year"), function(x) x[1])
years_research<-table(floor(as.numeric(years)))
years_research[23]<-sum(is.na(years))
years_research[22]<-1
years_research[21]<-1
names(years_research)[23]<-"N/A"
names(years_research)[22]<-"Depends"
names(years_research)[21]<-"Many"
barplot(as.vector(years_research), names.arg = names(years_research), cex.names=0.6)
title(colnames(data)[9])
table(data[,10])
discipline <- table(data[10])[c(2:14)]
discipline<-sort(discipline)
discipline[14] <- table(data[10])[1]
names(discipline)[14]<-"N/A"
#barplot(discipline)
x <- barplot(discipline, xaxt="n")
labs <- names(discipline)
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[10])
analysis<-table(data[,18])[c(4,2,3,1)]
names(analysis)[4]<-"N/A"
barplot(analysis)
title(colnames(data)[18])
visual<-table(data[,20])[c(4,2,3,1)]
names(visual)[4]<-"N/A"
barplot(visual)
title(colnames(data)[20])

collect_tools<-data[,22]
collect_tools<-unlist(strsplit(as.character(collect_tools), split=", "))
sort(table(collect_tools))
x <- barplot(sort(table(collect_tools)), xaxt="n")
labs <- names(sort(table(collect_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[22])

cleaning_tools<-data[,23]
cleaning_tools<-unlist(strsplit(as.character(cleaning_tools), split=", "))
sort(table(cleaning_tools))
x <- barplot(sort(table(cleaning_tools)), xaxt="n")
labs <- names(sort(table(cleaning_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[23])

analysis_tools<-data[,24]
analysis_tools<-unlist(strsplit(as.character(analysis_tools), split=", "))
sort(table(analysis_tools))
x <- barplot(sort(table(analysis_tools)), xaxt="n")
labs <- names(sort(table(analysis_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[24])

writing_tools<-data[,26]
writing_tools<-unlist(strsplit(as.character(writing_tools), split=", "))
sort(table(writing_tools))
x <- barplot(sort(table(writing_tools)), xaxt="n")
labs <- names(sort(table(writing_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[26])

reference_tools<-data[,27]
reference_tools<-unlist(strsplit(as.character(reference_tools), split=", "))
sort(table(reference_tools))
x <- barplot(sort(table(reference_tools)), xaxt="n")
labs <- names(sort(table(reference_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[27])

graphics_tools<-data[,28]
graphics_tools<-unlist(strsplit(as.character(graphics_tools), split=", "))
sort(table(graphics_tools))
x <- barplot(sort(table(graphics_tools)), xaxt="n")
labs <- names(sort(table(graphics_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[28])

training_tools<-data[,29]
training_tools<-unlist(strsplit(as.character(training_tools), split=", "))
sort(table(training_tools))
x <- barplot(sort(table(training_tools)), xaxt="n")
labs <- names(sort(table(training_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[29])

os_tools<-data[,30]
os_tools<-unlist(strsplit(as.character(os_tools), split=", "))
sort(table(os_tools))
x <- barplot(sort(table(os_tools)), xaxt="n")
labs <- names(sort(table(os_tools)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[30])

research_use<-data[,31]
research_use<-unlist(strsplit(as.character(research_use), split=", "))
sort(table(research_use))
barplot(table(research_use))
title(colnames(data)[31])

research_dev<-data[,32]
research_dev<-unlist(strsplit(as.character(research_dev), split=", "))
sort(table(research_dev))
barplot(table(research_dev))
title(colnames(data)[32])

software_training<-data[,34]
software_training<-unlist(strsplit(as.character(software_training), split=", "))
sort(table(software_training))
barplot(table(software_training))
title(colnames(data)[34])

software_funding<-data[,35]
software_funding<-unlist(strsplit(as.character(software_funding), split=", "))
sort(table(software_funding))
barplot(table(software_funding))
title(colnames(data)[35])

code_often<-data[,36]
code_often<-unlist(strsplit(as.character(code_often), split=", "))
x <- barplot(table(code_often)[c(1,2,5,6,3,4)], xaxt="n")
labs <- names(table(code_often)[c(1,2,5,6,3,4)])
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[36])

code_complex<-data[,37]
code_complex<-unlist(strsplit(as.character(code_complex), split=", "))
x <- barplot(table(code_complex)[c(2,5,3,4,1)], xaxt="n")
labs <- names(table(code_complex)[c(2,5,3,4,1)])
text(cex=0.5, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[37])

code_lang<-data[,38]
code_lang<-unlist(strsplit(as.character(code_lang), split=", "))
x <- barplot(sort(table(code_lang)), xaxt="n")
labs <- names(sort(table(code_lang)))
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[38])

version_familiar<-data[,39]
version_familiar<-unlist(strsplit(as.character(version_familiar), split=", "))
x <- barplot(table(version_familiar)[c(4,1,3,2)], xaxt="n")
labs <- names(table(version_familiar)[c(4,1,3,2)])
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[39])

command_familiar<-data[,40]
command_familiar<-unlist(strsplit(as.character(command_familiar), split=", "))
x <- barplot(table(command_familiar)[c(4,1,3,2)], xaxt="n")
labs <- names(table(command_familiar)[c(4,1,3,2)])
text(cex=0.6, x=x+0.25, y=-.5, labs, xpd=TRUE, srt=25, pos=2)
title(colnames(data)[40])

dev.off()

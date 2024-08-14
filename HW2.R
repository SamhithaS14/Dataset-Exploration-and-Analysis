colors<- c('red','blue','cyan','yellow','green') #colors to columns

#total number of students
nrow(moodyJanuary31b)

#frequency of grades
barplot(table(moodyJanuary31b$Grade), main='Frequency of Grades', xlab='Grade', ylab='Frequency', col=colors)

mosaicplot(moodyJanuary31b$Grade~moodyJanuary31b$Asking,xlab = 'Grade',ylab = 'Asking habit', main = "Mosiac of grade vs Asking habit in class",col=colors,border="black")

#questions attribute frequency
barplot(table(moodyJanuary31b$Asking), main='Frequency of Questions', xlab='Questions', ylab='Frequency', col=colors)

asking_never <- subset(moodyJanuary31b,Asking =="Never")
nrow(asking_never)
barplot(table(asking_never$Grade), main='Frequency of Grades of Students who Never ask questions (813 students)', xlab='Grade', ylab='Frequency', col=colors)

asking_sometimes <- subset(moodyJanuary31b,Asking =="Sometimes")
nrow(asking_sometimes)
barplot(table(asking_sometimes$Grade), main='Frequency of Grades of Students who Sometimes ask questions (605 students)', xlab='Grade', ylab='Frequency', col=colors)

asking_often <- subset(moodyJanuary31b,Asking=="Often")
nrow(asking_often)
barplot(table(asking_often$Grade), main='Frequency of Grades of Students who Often ask questions (582 students)', xlab='Grade', ylab='Frequency', col=colors)

questions <- tapply(moodyJanuary31b$Score, moodyJanuary31b$Asking, mean)
barplot(questions, main='Mean Score for Questions', xlab='Questions', ylab='Mean Score', col=colors)


#texting attribute
barplot(table(moodyJanuary31b$Texting), main='Frequency of Texting', xlab='Texting', ylab='Frequency', col=colors)

mosaicplot(moodyJanuary31b$Grade~moodyJanuary31b$Texting,xlab = 'Grade',ylab = 'Texting habit', main = "Mosiac of grade vs Texting habit in class",col=colors,border="black")

always_texting <- subset(moodyJanuary31b,Texting=="Always")
barplot(table(always_texting$Grade), main='Frequency of Grades of Students who Always text', xlab='Grade', ylab='Frequency', col=colors)

sometimes_texting <- subset(moodyJanuary31b,Texting=="Sometimes")
barplot(table(sometimes_texting$Grade), main='Frequency of Grades of Students who Sometimes text', xlab='Grade', ylab='Frequency', col=colors)

often_texting <- subset(moodyJanuary31b,Texting=="Often")
barplot(table(often_texting$Grade), main='Frequency of Grades of Students who Often text', xlab='Grade', ylab='Frequency', col=colors)

never_texting <- subset(moodyJanuary31b,Texting=="Never")
barplot(table(never_texting$Grade), main='Frequency of Grades of Students who Never text', xlab='Grade', ylab='Frequency', col=colors)

texting <- tapply(moodyJanuary31b$Score, moodyJanuary31b$Texting, mean)
barplot(texting, main='Mean Score for Texting', xlab='Texting', ylab='Mean Score', col=colors)


#dozing attribute
barplot(table(moodyJanuary31b$Dozing), main='Frequency of Dozing', xlab='Dozing', ylab='Frequency', col=colors)

mosaicplot(moodyJanuary31b$Grade~moodyJanuary31b$Dozing,xlab = 'Grade',ylab = 'Dozing habit', main = "Mosiac of grade vs Dozing habit in class",col=colors,border="black")

always_dozing <- subset(moodyJanuary31b,Dozing=="Always")
barplot(table(always_dozing$Grade), main='Frequency of Grades of Students who Always doze', xlab='Grade', ylab='Frequency', col=colors)

sometimes_dozing <- subset(moodyJanuary31b,Dozing=="Sometimes")
barplot(table(sometimes_dozing$Grade), main='Frequency of Grades of Students who Sometimes doze', xlab='Grade', ylab='Frequency', col=colors)

never_dozing <- subset(moodyJanuary31b,Dozing=="Never")
barplot(table(never_dozing$Grade), main='Frequency of Grades of Students who Never doze', xlab='Grade', ylab='Frequency', col=colors)

dozing <- tapply(moodyJanuary31b$Score, moodyJanuary31b$Dozing, mean)
barplot(texting, main='Mean Score for Dozing', xlab='Dozing', ylab='Mean Score', col=colors)

#scatterplot comparison of GPA and Grade
plot(moodyJanuary31b$Score,moodyJanuary31b$GPA,ylab="GPA",xlab="Score",main=" Score vs GPA",col="red")

#distributions of score by different categorial attributes
boxplot(asking_never$Score~asking_never$Grade, main='Distribution of Score by Grade for those who Never ask questions', xlab='Grade', ylab='Score', col=colors)

boxplot(asking_sometimes$Score~asking_sometimes$Grade, main='Distribution of Score by Grade for those who Sometimes ask questions', xlab='Grade', ylab='Score', col=colors)

boxplot(asking_often$Score~asking_often$Grade, main='Distribution of Score by Grade for those who Often ask questions', xlab='Grade', ylab='Score', col=colors)

boxplot(never_texting$Score~never_texting$Grade, main='Distribution of Score by Grade for those who Never text', xlab='Grade', ylab='Score', col=colors)

boxplot(often_texting$Score~often_texting$Grade, main='Distribution of Score by Grade for those who Often text', xlab='Grade', ylab='Score', col=colors)

boxplot(sometimes_texting$Score~sometimes_texting$Grade, main='Distribution of Score by Grade for those who Sometimes text', xlab='Grade', ylab='Score', col=colors)

boxplot(always_texting$Score~always_texting$Grade, main='Distribution of Score by Grade for those who Always text', xlab='Grade', ylab='Score', col=colors)

boxplot(always_dozing$Score~always_dozing$Grade, main='Distribution of Score by Grade for those who Always doze', xlab='Grade', ylab='Score', col=colors)

boxplot(sometimes_dozing$Score~sometimes_dozing$Grade, main='Distribution of Score by Grade for those who Sometimes doze', xlab='Grade', ylab='Score', col=colors)

boxplot(never_dozing$Score~never_dozing$Grade, main='Distribution of Score by Grade for those who Never doze', xlab='Grade', ylab='Score', col=colors)


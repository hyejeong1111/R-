# 필요한 패키지 로드
install.packages("openxlsx")
library(openxlsx)

# 엑셀 파일 읽어오기
data <- read.xlsx("C:/Users/LG gram/Desktop/movie data.xlsx", startRow = 2)
data$개봉일 <- factor(data$개봉일, levels = c(1:4),labels = c("1","0","1","0"))
write.xlsx(data,"C:/Users/LG gram/Desktop/moviesss.data.xlsx")

movie <- read.xlsx("C:/Users/LG gram/Desktop/3학년1학기/다차원자료분석PBL/R을 활용한 다변량 자료분석 방법론 - 데이터파일/movie.xlsx")
movie.lm <- lm(x4~x1+x2+x3+x5+x6+x7+x8+x9+x10,data = movie)
summary(movie.lm)

movie.lm2 <- lm(x4~x1+x2+x3,data = movie)
summary(movie.lm2)

movie.lm3 <- lm(x4~x5+x6+x7+x8+x9,data = movie)
summary(movie.lm3)

###################
library(openxlsx)
movies <- read.xlsx("C:/Users/LG gram/Desktop/3학년1학기/다차원자료분석PBL/R을 활용한 다변량 자료분석 방법론 - 데이터파일/movies.xlsx")
movies.X <- movies[-1] # 첫 번째 변수 제거
selected_cols <- c('상영횟수','평점','유명출연배우.수','관객수')
selected_data <- movies.X[,selected_cols]
cor(selected_data)
cor(movies.X)
pairs(selected_data)
pairs(movies.X)




##################
airplane <- read.csv("C:/Users/LG gram/Desktop/airplane.csv")

airplane <- airplane[rowSums(airplane[,8:21] == 0) == 0,]  # 0인 값이 있는 행 다 제거 
airplane <- airplane[rowSums(is.na(airplane[,1:24])) == 0,] # 결측값 다 제거 
write.csv(airplane, "C:/Users/LG gram/Desktop/airplanes.csv")




airplanes <- read.csv("C:/Users/LG gram/Desktop/airplanes.csv")
airplane.X <- airplanes[7:20]
cor(airplane.X)
library(psych)

print(describe(airplanes[,7:20]))  # 기초 통계량 


heatmap(cor(airplane.X), col = colorRampPalette(c("blue", "white", "red"))(50))


# 열 분석표 그리기
library(corrplot)
corrplot(cor(airplane.X), method = "color", type = "full", order = "hclust", 
         addCoef.col = "black", number.cex = 0.5)



# 막대 그래프 그리기1
freq_table1 <- table(airplanes$satisfaction, airplanes$Customer.Type)

# 
bar_colors <- c("red", "blue")
barplot(as.matrix(freq_table1), beside = TRUE, legend = rownames(freq_table1),
        xlab = "Customer Type", ylab = "Count", main = "satisfaction",args.legend = list(x = 3, y = 40000), col = bar_colors, cex.names = 2,  cex.lab = 1.5)

# 막대 그래프 그리기2
freq_table2 <- table(airplanes$satisfaction, airplanes$Type.of.Travel)

# 
bar_colors <- c("red", "blue")
barplot(as.matrix(freq_table2), beside = TRUE, 
        xlab = "Type of Travel", ylab = "Count", main = "satisfaction", col = bar_colors, cex.names = 2,  cex.lab = 1.5)

# 막대 그래프 그리기3
freq_table3 <- table(airplanes$satisfaction, airplanes$Class)

# 
bar_colors <- c("red", "blue")
barplot(as.matrix(freq_table3), beside = TRUE,
        xlab = "Class", ylab = "Count", main = "satisfaction", col = bar_colors, cex.names = 2,  cex.lab = 1.5)


# 막대 그래프 그리기4
freq_table4 <- table(airplanes$satisfaction, airplanes$Gender)

# 
bar_colors <- c("red", "blue")
barplot(as.matrix(freq_table4), beside = TRUE,
        xlab = "Gender", ylab = "Count", main = "satisfaction", col = bar_colors, cex.names = 2,  cex.lab = 1.5)





# 전체 만족도 비율 파악 
barplot(table(airplanes$satisfaction), main = "satisfaction", col = bar_colors, cex.names = 2,  cex.lab = 1.5)


########상관행렬에 기초한 주성분분석
airplane.cor.prcomp <- prcomp(airplane.X, center = TRUE, scale. = TRUE) # 분석
print(airplane.cor.prcomp) # 리스트 객체(고유벡터 등) 출력
airplane.cor.prcomp$sdev^2 # 고유값 출력
summary(airplane.cor.prcomp) # 설명분산 요약



######인자분석(회전X) 
library(psych)
airplane.X.pm <- principal(airplane.X, cor = "cor", nfactors = 4, rotate = "none")
print(airplane.X.pm,sort = TRUE, digits = 3)

#####인자분석(직교회전)
airplane.X.pm.varimax <- principal(airplane.X, cor = "cor", nfactors = 4, rotate = "varimax")
print(airplane.X.pm.varimax,sort = TRUE, digits = 3)
print(airplane.X.pm.varimax$scores,digit=3)

print(airplane.X.pm.varimax$scores[,c('RC1','RC2', 'RC3', 'RC4')],digit=3)  # 인자점수 출력 
logit <- airplane.X.pm.varimax$scores[,c('RC1','RC2', 'RC3', 'RC4')] 
logits <- cbind(logit,airplane.Q$satisfaction) # satisfaction열 붙이기
colnames(logits)[5] <- "satisfaction"  # 열 이름 -> satisfaction으로 바꾸기기
View(logits)



print(airplane.X.pm.varimax, digits = 3)

#####인자분석(사각회전)
airplane.X.pm.promax <- principal(airplane.X, cor = "cor", nfactors = 4, rotate = "promax")
print(airplane.X.pm.promax,sort = TRUE, digits = 3)



#####인자구조 다이어그램(회전X)
library(psych)
fa.diagram(airplane.X.pm, simple = FALSE, cut = 0.55, digits = 3)

#####인자구조 다이어그램(직교회전)
library(psych)
fa.diagram(airplane.X.pm.varimax, simple = FALSE, cut = 0.52, digits = 3)
fa.diagram(airplane.X.pm.varimax, simple = TRUE, cut = 0.52, digits = 3)

#####인자구조 다이어그램(사각회전)
library(psych)
fa.diagram(airplane.X.pm.promax, simple = FALSE, cut = 0.52, digits = 3)





###KMO 표본적합성 측도
library(psych)
KMO(airplane.X)

###Bartlett의 구형성 검정
airplane.X.cor <- cor(airplane.X)
cortest.bartlett(airplane.X.cor, n=nrow(airplane.X))




airplane.X <- airplane.X[complete.cases(airplane.X), ] # 결측치 있는 행 없앰
cor(airplane.X)

# 열의 결측치 확인
airplane_na <- sum(is.na(airplane.X$Arrival.Delay.in.Minutes))  # "columnName"에는 확인하려는 열의 이름을 넣어주세요.

# 결측치 여부 출력
print(airplane_na)

View(airplane.X)






















##############################################################################
airplanes.A <- airplanes[airplanes$Age > 19, ]  #나이가 19세 이상인 관측치들만 선택
airplane.A.X <- airplanes.A[7:20]

print(describe(airplanes.A[,7:20]))  # 기초 통계량 


# 열 분석표 그리기
library(corrplot)
corrplot(cor(airplane.A.X), method = "color", type = "full", order = "hclust", 
         addCoef.col = "black", number.cex = 0.5)


######인자분석(회전X) 
airplane.A.X.pm <- principal(airplane.A.X, cor = "cor", nfactors = 4, rotate = "none")
print(airplane.A.X.pm,sort = TRUE, digits = 3)

#####인자분석(직교회전)
airplane.A.X.pm.varimax <- principal(airplane.A.X, cor = "cor", nfactors = 4, rotate = "varimax")
print(airplane.A.X.pm.varimax,sort = TRUE, digits = 3)

#####인자분석(사각회전)
airplane.A.X.pm.promax <- principal(airplane.A.X, cor = "cor", nfactors = 4, rotate = "promax")
print(airplane.A.X.pm.promax,sort = TRUE, digits = 3)



#####인자구조 다이어그램(회전X)
library(psych)
fa.diagram(airplane.A.X.pm, simple = FALSE, cut = 0.55, digits = 3)

#####인자구조 다이어그램(직교회전)
library(psych)
fa.diagram(airplane.A.X.pm.varimax, simple = FALSE, cut = 0.52, digits = 3)

#####인자구조 다이어그램(사각회전)
library(psych)
fa.diagram(airplane.A.X.pm.promax, simple = FALSE, cut = 0.52, digits = 3)





##################################군집분석#######################################
#최적분리 군집방법(k-평균 군집분석)  -> 군집의 개수 정하기
airplane.X.S <- airplane.X[1:1000, ]

library(factoextra)
fviz_nbclust(airplane.X.S,kmeans,method = "wss", k.max = 10)
fviz_nbclust(airplane.X.S,kmeans,method = "silhouette", k.max = 10)
fviz_nbclust(airplane.X.S,kmeans,method = "gap_stat", nboot = 200) # 너무 오래걸림,,, 


library(NbClust)
NbClust(data=airplane.X.S, distance = "euclidean", min.nc=2, max.nc = 7, method = "kmeans")

airplane.X.ccc <- NbClust(data=airplane.X.S, distance = "euclidean", min.nc=2, max.nc = 7, method = "kmeans", index = "ccc")
airplane.X.ccc




#최적분리 군집방법
airplane.X.Z <- scale(airplane.X, center = TRUE, scale = TRUE) # 표준화
airplanes.X.Z.kmeans <- kmeans(airplane.X.Z, centers =4, nstart = 30) #군집분석
airplanes.X.Z.kmeans$iter
airplanes.X.Z.kmeans$size # 군집별 개체수 출력
print(airplanes.X.Z.kmeans$centers,digits = 4) # 군집별 중심(평균 벡터) 출력 




###############판별분석##################
#모공분산행렬의 동일성에 대한 검정
library(biotools)

airplanes$satisfaction <- ifelse(airplanes$satisfaction == "satisfied", 1, 0) # 최종 만족도 0,1로 치환
airplane.Q <- airplanes[7:21]


#####제 1인자 ###
airplane.Q.1 <- airplane.Q[c("Cleanliness","Food.and.drink","Seat.comfort","Inflight.entertainment")]
airplance.boxM1 <- boxM(airplane.Q.1,airplanes$satisfaction) # Box's M-test
print(airplance.boxM1)

#이차 판별분석
library(MASS)
airplane.Q.1.qda <- qda(satisfaction~Cleanliness+Food.and.drink+Seat.comfort+Inflight.entertainment,data = airplanes)
pred1 <- predict(airplane.Q.1.qda, airplanes)
airplane.ctbl1 <- table(airplanes$satisfaction,pred1$class)
library(DescTools)
Desc(airplane.ctbl1,digits = 2)



#####제 2인자 ###
airplane.Q.2 <- airplane.Q[c("Ease.of.Online.booking","Gate.location","Departure.Arrival.time.convenient","Inflight.wifi.service")]
airplance.boxM2 <- boxM(airplane.Q.2,airplanes$satisfaction) # Box's M-test
print(airplance.boxM2)

#이차 판별분석
library(MASS)
airplane.Q.2.qda <- qda(satisfaction~Ease.of.Online.booking+Gate.location+Departure.Arrival.time.convenient+Inflight.wifi.service,data = airplanes)
pred2 <- predict(airplane.Q.2.qda, airplanes)
airplane.ctbl2 <- table(airplanes$satisfaction,pred2$class)
library(DescTools)
Desc(airplane.ctbl2,digits = 2)





#####제 3인자 ###
airplane.Q.3 <- airplane.Q[c("Inflight.service","Baggage.handling","On.board.service","Leg.room.service")]
airplance.boxM3 <- boxM(airplane.Q.3,airplanes$satisfaction) # Box's M-test
print(airplance.boxM3)

#이차 판별분석
library(MASS)
airplane.Q.3.qda <- qda(satisfaction~Inflight.service+Baggage.handling+On.board.service+Leg.room.service,data = airplanes)
pred3 <- predict(airplane.Q.3.qda, airplanes)
airplane.ctbl3 <- table(airplanes$satisfaction,pred3$class)
library(DescTools)
Desc(airplane.ctbl3,digits = 2)




#####제 4인자 ###
airplane.Q.4 <- airplane.Q[c("Online.boarding","Checkin.service")]
airplance.boxM4 <- boxM(airplane.Q.4,airplanes$satisfaction) # Box's M-test
print(airplance.boxM4)

#이차 판별분석
library(MASS)
airplane.Q.4.qda <- qda(satisfaction~Online.boarding+Checkin.service,data = airplanes)
pred4 <- predict(airplane.Q.4.qda, airplanes)
airplane.ctbl4 <- table(airplanes$satisfaction,pred4$class)
library(DescTools)
Desc(airplane.ctbl4,digits = 2)



#######################################################
# Business에 해당하는 행 제거

airplanes.V <- airplanes[airplanes$Class != "Business", ] 
airplanes.I <- airplanes.V[airplanes.V$Type.of.Travel != "Business travel", ] 
airplane.O <- airplanes.I[7:21]
write.csv(airplanes.I, "C:/Users/LG gram/Desktop/airplanesss.csv")

#####제 1인자 ###
airplane.O.1 <- airplane.O[c("Cleanliness","Food.and.drink","Seat.comfort","Inflight.entertainment")]
airplance.boxM1.O <- boxM(airplane.O.1,airplane.O$satisfaction) # Box's M-test
print(airplance.boxM1.O)


airplane.O.1.O <- airplane.O[c("Cleanliness","Food.and.drink","Seat.comfort","Inflight.entertainment","satisfaction")]

#선형 판별분석(lda함수)
library(MASS)
airplane.O.1.lda <- lda(satisfaction~Cleanliness+Food.and.drink+Seat.comfort+Inflight.entertainment,data = airplane.O.1.O)
pred1.O <- predict(airplane.O.1.lda, airplane.O.1.O)
airplane.ctbl1.O <- table(airplane.O.1.O$satisfaction,pred1.O$class)
library(DescTools)
Desc(airplane.ctbl1.O,digits = 2)


####################################################
airplanes <- read.csv("C:/Users/LG gram/Desktop/airplanes.csv")
airplane.X <- airplanes[7:20]                  
library(psych)

library(biotools)

airplanes$satisfaction <- ifelse(airplanes$satisfaction == "satisfied", 1, 0) # 최종 만족도 0,1로 치환
airplane.Q <- airplanes[7:21]


airplane.X.pm.varimax <- principal(airplane.X, cor = "cor", nfactors = 4, rotate = "varimax")
print(airplane.X.pm.varimax,sort = TRUE, digits = 3)
print(airplane.X.pm.varimax$scores,digit=3)

print(airplane.X.pm.varimax$scores[,c('RC1','RC2', 'RC3', 'RC4')],digit=3)  # 인자점수 출력 
logit <- airplane.X.pm.varimax$scores[,c('RC1','RC2', 'RC3', 'RC4')] 
logits <- cbind(logit,airplane.Q$satisfaction) # satisfaction열 붙이기
colnames(logits)[5] <- "satisfaction"  # 열 이름 -> satisfaction으로 바꾸기기
View(logits)


logits  <- as.data.frame(logits)  # 데이터 프레임으로 전환


airplane.logit <- glm(satisfaction~RC1+RC2+RC3+RC4, data = logits, family = binomial) # 로지스틱 판별분석
summary(airplane.logit)


airplane.logit2 <- glm(satisfaction~Checkin.service+Online.boarding, data = airplane.Q, family = binomial) # (제4인자)로지스틱 판별분석
summary(airplane.logit2)

airplane.logit3 <- glm(satisfaction~Cleanliness+Food.and.drink+Seat.comfort+Inflight.entertainment, data = airplane.Q, family = binomial) # (제1인자)로지스틱 판별분석
summary(airplane.logit3)

airplane.logit4 <- glm(satisfaction~Inflight.service+Baggage.handling+On.board.service+Leg.room.service, data = airplane.Q, family = binomial) # (제3인자)로지스틱 판별분석
summary(airplane.logit4)






library(car)
library(rcompanion)
Anova(airplane.logit, type = "II", test="Wald") # Wald's test


library(MASS)
airplane.step <- stepAIC(airplane.logit,direction = c("backward"))  # AIC 통계량을 이용한 변수선택


summary(airplane.step)



# 샤피로-윌크 검정 수행
airplane.XS <- airplane.X[1:5000,14]
shapiro.test(airplane.XS)
ks.test(airplane.X, "pnorm", mean(airplane.X), sd(airplane.X)) # x에는 검정하고자 하는 데이터를 입력합니다.



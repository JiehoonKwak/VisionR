# TidyData
# parameter review ->결측치, 이상치 제거거

### 1. Data 불러오기
df = read.csv("originaldata0526.csv", fileEncoding = "CP949", encoding = "UTF-8") 
d = cbind(df[,c(3,27,28,29,30)], df[,43:71])

### 2. Type 변환
d[,1] = as.factor(d[,1])  
d[545,9] = NA # LV
d[,9] = as.numeric(d[,9]) %>% abs()
d[,"ARA500"] = as.numeric(d[,"ARA500"])
d[,"ARA750"] = as.numeric(d[,"ARA750"])
d[,"IT500.m"] = as.numeric(d[,"IT500.m"])
d[,"KT500.m"] = as.numeric(d[,"KT500.m"])
d[,"pupil.distance"] = as.numeric(d[,"pupil.distance"])
d[,24] = as.numeric(d[,24])
for (i in 25:33) {
  d[,i] = as.numeric(d[,i])
}

library(lubridate)
d[,4] = ymd(d[,4])
d[,6] = ymd(ymd_hm(d[,6]))
d["time"] = d[,6]-d[,4] # Visit - PI : 음수 -> pre, 14근처, 365*4 근처


### 3. 직접 측정 데이터  :21,22,23,31,32,33 - IT500, KT500, KT750
d[21] = d[21]*d[7]/d[20]
d[22] = d[22]*d[7]/d[20]
d[23] = d[23]*d[7]/d[20]
d[31] = d[31]*d[7]/d[20]
d[32] = d[32]*d[7]/d[20]
d[33] = d[33]*d[7]/d[20]
d["PD"] = d[12]*d[7]/d[20]/1000 # 36은 머임


### 4. Renaming
d = rename(d, Lens = Lens.status..phakic.0..pseudophakic.1..aphakic...2.)
names(d)
spl = str_split(names(d), pattern = fixed("."), simplify = TRUE)
names(d)[20:23] = spl[20:23]
names(d)[24:33] = paste(spl[24:33],"N",sep = "") ## table시 수정
d = subset(d, select = -20) # drop CCT.m


### 5. 평균치 만들기 20221031 :결과값 : newd
newd = copy(d)

newd = subset(newd, select = c(1,3,4,5,6,7,8,9,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35))


newd$AOD500 = ifelse(!is.na(newd$AOD500)&!is.na(newd$AOD500N), 
                     (newd$AOD500 + newd$AOD500N)/2, 
                     ifelse(is.na(newd$AOD500)&!is.na(newd$AOD500N), 
                            newd$AOD500N, 
                            ifelse(!is.na(newd$AOD500)&is.na(newd$AOD500N),
                                   newd$AOD500, NA)))

newd$AOD750 = ifelse(!is.na(newd$AOD750)&!is.na(newd$AOD750N), 
                     (newd$AOD750 + newd$AOD750N)/2, 
                     ifelse(is.na(newd$AOD750)&!is.na(newd$AOD750N), 
                            newd$AOD750N, 
                            ifelse(!is.na(newd$AOD750)&is.na(newd$AOD750N),
                                   newd$AOD750, NA)))

newd$ARA500 = ifelse(!is.na(newd$ARA500)&!is.na(newd$ARA500N), 
                     (newd$ARA500 + newd$ARA500N)/2, 
                     ifelse(is.na(newd$ARA500)&!is.na(newd$ARA500N), 
                            newd$ARA500N, 
                            ifelse(!is.na(newd$ARA500)&is.na(newd$ARA500N),
                                   newd$ARA500, NA)))

newd$ARA750 = ifelse(!is.na(newd$ARA750)&!is.na(newd$ARA750N), 
                     (newd$ARA750 + newd$ARA750N)/2, 
                     ifelse(is.na(newd$ARA750)&!is.na(newd$ARA750N), 
                            newd$ARA750N, 
                            ifelse(!is.na(newd$ARA750)&is.na(newd$ARA750N),
                                   newd$ARA750, NA)))


newd$TISA500 = ifelse(!is.na(newd$TISA500)&!is.na(newd$TISA500N), 
                     (newd$TISA500 + newd$TISA500N)/2, 
                     ifelse(is.na(newd$TISA500)&!is.na(newd$TISA500N), 
                            newd$TISA500N, 
                            ifelse(!is.na(newd$TISA500)&is.na(newd$TISA500N),
                                   newd$TISA500, NA)))

newd$TISA750 = ifelse(!is.na(newd$TISA750)&!is.na(newd$TISA750N), 
                     (newd$TISA750 + newd$TISA750N)/2, 
                     ifelse(is.na(newd$TISA750)&!is.na(newd$TISA750N), 
                            newd$TISA750N, 
                            ifelse(!is.na(newd$TISA750)&is.na(newd$TISA750N),
                                   newd$TISA750, NA)))

newd$SSA = ifelse(!is.na(newd$SSA)&!is.na(newd$SSAN), 
                     (newd$SSA + newd$SSAN)/2, 
                     ifelse(is.na(newd$SSA)&!is.na(newd$SSAN), 
                            newd$SSAN, 
                            ifelse(!is.na(newd$SSA)&is.na(newd$SSAN),
                                   newd$SSA, NA)))

newd$IT500 = ifelse(!is.na(newd$IT500)&!is.na(newd$IT500N), 
                     (newd$IT500 + newd$IT500N)/2, 
                     ifelse(is.na(newd$IT500)&!is.na(newd$IT500N), 
                            newd$IT500N, 
                            ifelse(!is.na(newd$IT500)&is.na(newd$IT500N),
                                   newd$IT500, NA)))

newd$KT500 = ifelse(!is.na(newd$KT500)&!is.na(newd$KT500N), 
                     (newd$KT500 + newd$KT500N)/2, 
                     ifelse(is.na(newd$KT500)&!is.na(newd$KT500N), 
                            newd$KT500N, 
                            ifelse(!is.na(newd$KT500)&is.na(newd$KT500N),
                                   newd$KT500, NA)))

newd$KT750 = ifelse(!is.na(newd$KT750)&!is.na(newd$KT750N), 
                     (newd$KT750 + newd$KT750N)/2, 
                     ifelse(is.na(newd$KT750)&!is.na(newd$KT750N), 
                            newd$KT750N, 
                            ifelse(!is.na(newd$KT750)&is.na(newd$KT750N),
                                   newd$KT750, NA)))

newd = subset(newd, select = -c(20,21,22,23,24,25,26,27,28,29))




#####################################
# boxplot
boxSum = function(data, var){
  ggplot(d, aes(x = 1, y = {{var}})) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 1) +
    coord_flip()
}
var.list = syms(names(d)[7:10])
plot.list = lapply(var.list, boxSum, data = d)
grid.arrange(grobs = plot.list, nrow =2)



## 추가 공부필요 ##############################################################################
# 이상치 탐방 -> 수정
out = boxplot.stats(d[["LV"]])[["out"]]
out_index = which(d$LV %in% c(out))
out_index
d[out_index,c("Lens", "LV")]

detectOutlier = function(data = data, var = var){
  var2 = enquos(var)
  t = boxplot.stats(data[[var2]])[["out"]]
  out_index = which(data[[var2]] %in% c(t))
  out_index
}
detectOutlier(d, KT500)


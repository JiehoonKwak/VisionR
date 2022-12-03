# Table3 - recal : 뭔가이상하다다

d.2 = newd %>% filter(Lens ==0)
d.2 = d.2 %>% group_by(id) %>% # visit : 방문차수
  mutate(visit = order(order(date.oct))) %>%
  as.data.frame()

d.2 = d.2 %>% group_by(id) %>%
  mutate(visitrank = ifelse((visit == max(visit)) & (visit != 1), 2, 
                            ifelse(visit ==1, 1, 0))) %>%
  as.data.frame()

d.2 = d.2 %>%
  mutate(pibar2w = ifelse(is.na(time) | time <=0, "pre",
                        ifelse(time <30, "2wk", "4yr"))) # 숫자를 몇으로 할 것인가
d.2$pibar2w = factor(d.2$pibar2w, levels = c("pre", "2wk", "4yr"))

table(d.2$pibar2w)

# mean, sd, p-value

library(sasLM)
temp = round(tsum(ACD~pibar, d.2[d.2$pibar %in% c("pre", "2wk"),])[1:2,1:2], digits = 3)
pval = round(t.test(ACD~pibar, d.2[d.2$pibar %in% c("pre", "2wk"),])$p.value, digits = 3)
cbind(pre = paste(temp[1,1], temp[2,1], sep = " ± "), pval)


# baseline vs 2wk
table3 = data.frame()
for (var in varlist) {
  formula = as.formula(paste0(var,"~ pibar2w"))
  temp = round(tsum(formula, d.2[d.2$pibar2w %in% c("pre", "2wk"),])[1:2,1:2], digits = 2)
  pval = round(t.test(formula, d.2[d.2$pibar2w %in% c("pre", "2wk"),])$p.value, digits = 2)
  table3 = rbind(table3,cbind(pre = paste(temp[1,1], temp[2,1], sep = " ± "), pval))
}
rownames(table3) = varlist

# baseline vs 4yr
table3.1 = data.frame()
for (var in varlist) {
  formula = as.formula(paste0(var,"~ pibar2w"))
  temp = round(tsum(formula, d.2[d.2$pibar2w %in% c("pre", "4yr"),])[1:2,1:2], digits = 2)
  pval = round(t.test(formula, d.2[d.2$pibar2w %in% c("pre", "4yr"),])$p.value, digits = 2)
  table3.1 = rbind(table3.1,cbind(yr4 = paste(temp[1,2], temp[2,2], sep = " ± "), pval))
}
rownames(table3.1) = varlist


# 2wk vs 4yr
table3.2 = data.frame()
for (var in varlist) {
  formula = as.formula(paste0(var,"~ pibar2w"))
  temp = round(tsum(formula, d.2[d.2$pibar2w %in% c("2wk", "4yr"),])[1:2,1:2], digits = 2)
  pval = round(t.test(formula, d.2[d.2$pibar2w %in% c("2wk", "4yr"),])$p.value, digits = 2)
  table3.2 = rbind(table3.2,cbind(wk2 = paste(temp[1,2], temp[2,2], sep = " ± "), pval))
}
rownames(table3.2) = varlist

table3fin = cbind(table3, table3.1, table3.2)
table3fin = table3fin[-c(5,7,9,13),] # 걍 다 두자리로 맞추기

write.csv(table3fin, "Newtable03.csv")
# pre, 2wk, 4yr : 2wk~4yr 사이는 NA로 표현??
## 그럼 bar graph도 수정해야되는지








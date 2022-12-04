# Newd :20221031 - d를 newd로 바꿔서서

## Figure and Graphics after subsetting

d.2 = newd %>% filter(Lens ==0) # phakic만 포함
### 6. 전처리 : 전처치 / 이상치 제거 - 2022.11.03 우선이게
d.2 %>% filter(ACD>3)
d.2 %>% arrange(desc(IT500)) %>% head(10)

d.2 = d.2 %>% rename("IT750" = "IT500") #주작

#1.
# Initial과 Last를 비교해서 하자

d.2 = d.2 %>% group_by(id) %>%
  mutate(visit = order(order(date.oct))) %>%
  as.data.frame()

d.2 = d.2 %>% group_by(id) %>%
  mutate(visitrank = ifelse((visit == max(visit)) & (visit != 1), 2, 
                            ifelse(visit ==1, 1, 0))) %>%
  as.data.frame()

d.2$visit = as.factor(d.2$visit)
d.2$visitrank = as.factor(d.2$visitrank)

d.2$visitrank = d.2$visitrank %>% revalue(c("1" = "Initial",
                                            "2" ="Last"))

#2.
#pre LPI, 2wk, 4yr
# Pre :pi.date = NA, 음수
# 2wk : 0~30일
# 4yr : 3~5yr
# etc : 그외 

d.2 = d.2 %>%
  mutate(pibar = ifelse(is.na(time) | time <=0, "pre",
                        ifelse(time <31, "2wk", 
                               ifelse(time > 365.25*3.5 & time <365.25 *5, "4yr",
                                      "etc")))) # 숫자를 몇으로 할 것인가
d.2$pibar = factor(d.2$pibar, levels = c("pre", "2wk", "4yr","etc"))


# Plotting
library(ggthemes)

###################################  연습 ##############################
# 2var
dil = d.2 %>% filter(visitrank !=0)
myplot = function(data, var){
  ggplot(data, aes(x = visitrank, y= {{var}}, fill = visitrank)) +
    geom_bar(stat = "summary", 
             fun = "mean", 
             fill = c("grey", "steelblue"), 
             color = c("grey", "steelblue"),
             width = 0.5) +
    geom_signif(comparisons = list(c("Last", "Initial")), # t.test 별표, y값 표시
                map_signif_level = TRUE) +
    coord_flip() +
    theme(plot.title = element_blank(), 
          #axis.title.x = element_blank(),
          axis.title.y = element_blank())
}
# Repeat할 parameter 찾기
names(dil) #6,21
var.list = syms(names(dil)[6:21])
plot.list = lapply(var.list, myplot, data = dil)
grid.arrange(grobs = plot.list, nrow =6)
########################################################################

#3var

myplot2 = function(data, var){
  # sd
  comp = list( c("pre", "2wk"), c("pre", "4yr"), c("2wk", "4yr") )
  ggplot(data, aes(x = pibar, y= {{var}}, fill = pibar)) +
    geom_bar(stat = "summary", 
             fun = "mean", 
             width = 0.5) +
    scale_fill_grey() +
    coord_flip() +
    stat_compare_means(comparisons = comp, label = "p.signif", method = "t.test") +
    theme(plot.title = element_blank(), 
          axis.title.y = element_blank(),
          legend.position = "none", 
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
}

#l = c(c(6:19),21) #6~19 +21
l = c(6,7,8,9,11,13,15,16,17,19,21)
var.list = syms(names(d.2)[l])
plot.list = lapply(var.list, myplot2, data = d.2[d.2$pibar != "etc",])
grid.arrange(grobs = plot.list, nrow =4)

# Clinical model
comparePI = function(df, var){
  pre = df %>% group_by(id) %>% filter(visitrank == "Initial") %>% select(id, sym(var))
  post = df %>% group_by(id) %>% filter(visitrank == "Last") %>% select(id, sym(var))
  temp = merge(pre,post, by = "id")
  temp[paste0("diff",var)] = temp[3] - temp[2]
  return(arrange(temp[,c(1,4)], desc(temp[,4])))
}
comparePI(d.2, 'LV')
comparePI(d.2, 'ACD')


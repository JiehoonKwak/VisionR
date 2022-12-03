# Newd :20221031 - d를 newd로 바꿔서서
## LMEM graphic
library(lme4)
d.3 = copy(d.2)

# time  = visit - pi
d.3$time = as.numeric(d.3$time)/365.25 # unit :1yr
d.3$pi = as.factor(d.3$pi)

d.3 = d.3 %>% filter(time>=0)

# # m1 = lmer(ACD ~ time + (1|id), data = d.3)
# # m2 = lmer(ACD ~ time + (1 + time|id), data = d.3) #random slope
# summary(m1)
# plot(m1)
# qqnorm(resid(m1))
# qqline(resid(m1))


### 2. Outlier 수정하기
d.2[resid(m1) > 0.7,] # 75, 510, 511
d.2[resid(m1) < - 0.7,]


library(ggeffects)
lmeSum2 = function(var){
  model = lmer(as.formula(paste0(var, "~ time + (1|id)")), data = d.3)
  pred.mm = ggpredict(model, terms = c("time"))
  ggplot(pred.mm) +
    geom_line(aes(x = x, y = predicted)) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), fill = "skyblue", alpha = 0.5) +
    geom_point(data = d.3, aes(x = time, y = !! rlang::sym(var), colour = id), alpha = 0.15) +
    labs(x = "Time", y = var)+
    theme(plot.title = element_blank(), 
          #axis.title.y = element_blank(),
          legend.position = "none", 
          panel.background = element_blank(),
          axis.line = element_line(arrow = arrow(type='closed', length = unit(4,'pt')))
          )
}


#l = c(c(6:19),21)
l = c(6,7,8,9,11,13,15,16,17,19,21)
var.list = as.list(names(d.3)[l])
plot.list = lapply(var.list, lmeSum2)
grid.arrange(grobs = plot.list, nrow =4)

ggsave("Fig05.tiff")


### 3. Multivariate analysis
# Tidy 다시 합치기 : 필요한 var - age, sex, AXL, Kreading
# AOD500~ time +PI +time*PI+PD +IT +LV
# IT500 ~ time + PI + time*PI + PD + LV + ARA500 (1 | id)

m3 = lmer(AOD500 ~ time + pi + PD + LV + (1|id), data = d.3)
summary(m3)

### 4. 
library(stargazer)
stargazer(m1, type = "text", digits = 3, star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


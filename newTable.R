# Newd :20221031 - d를 newd로 바꿔서서
# Table
## Tidy - d까지 로딩한 후








d.2 = newd %>% filter(Lens ==0) # phakic만 포함
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

dil = d.2 %>% filter(visitrank !=0)
dil$visitrank = factor(dil$visitrank, levels = c("Initial", "Last"))
names(dil)


# moonTable
library(moonBook)
mytable(visitrank~.-id-pi-pi.date-Lens-date.oct-time-visit,
        data = dil, digits = 2, method =3)

out = mytable(visitrank~.-id-pi-pi.date-Lens-date.oct-time-visit,
              data = dil, digits = 2)
mycsv(out, file = "table2.csv")

library(tidyverse)
g.loads= read_csv("~/R-projects/reheat2_pilot/output/mofa/gene_loadings.csv")
g.loads<- 
  g.loads %>% filter(Factor =="Factor1")%>%
  mutate(group = factor(sign(value)))%>%
  group_by(ctype, group)%>%
    mutate(rank.feature = rank(-abs(value) ),
           numb= length(feature),
           rank.feature.prop = rank.feature/numb)%>% 
    arrange((rank.feature))%>%
  mutate(ctype = factor(ctype, levels = c("CM", "Fib", "Endo","vSMCs", "PC", "Myeloid", "Lymphoid")))%>%
  mutate(qs= as.numeric(scale(value)))%>%
  ungroup()

quantile(g.loads$value)
saveRDS(g.loads, "data/mcp_processed.rds")

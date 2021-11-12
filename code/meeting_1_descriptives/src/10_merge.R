## Updated
# 2021/10/25
# 2021/10/27
# 2021/10/29

# data is from a google form
d <- read.csv("data/Formal demographers' group.csv")
d2 <- read.csv("data/Formal demographers' group 2.csv")

D <- d %>% 
  bind_rows(d2)

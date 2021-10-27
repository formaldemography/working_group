d_cle <- D %>% 
  separate(Research.interests, sep = ",", into = c("key1", "key2", "key3", "key4", "key5")) %>% 
  select("key1", "key2", "key3", "key4", "key5") %>% 
  gather(key = key, value = keyword) %>% 
  mutate(keyword2 = tolower(keyword)) %>% 
  separate(keyword2, sep = " and ", into = c("keyword3", "keyword4")) %>% 
  select(keyword3, keyword4) %>% 
  gather(key = key, value = keyword) %>% 
  separate(keyword, sep = ";", into = c("keyword2", "keyword3", "keyword4")) %>% 
  select(-key) %>% 
  gather(key = key, value = keyword) %>% 
  drop_na() %>% 
  mutate(keyword = as.character(keyword),
         keyword = str_squish(keyword), 
         # combine several keywords (some sort of arbitrary manner)
         keyword2 = case_when(keyword %in% c("ageing", "aging", "pop aging", "population ageing", 
                                             "population ageing", "population aging",
                                             "healthy ageing") ~ "ageing",
                              keyword %in% c("bayesian", "bayesian models", "bayesian demography", "bayesian methods") ~ "bayesian",
                              keyword %in% c("biodemography", "biodemography of young adulthood") ~ "biodemography",
                              keyword %in% c("cause of death", "causes of death", "causes-of-death") ~ "causes of death",
                              keyword %in% c("data vizualizaton", "dataviz", "data visualization") ~ "dataviz",
                              keyword %in% c("health disparities", "differential vulnerability",
                                             "early life determinants of health", "health inequality", "health inequalities") ~ "health disparities",
                              keyword %in% c("indirect methods", "indirect techniques", "indirect estimation") ~ "indirect methods",
                              keyword %in% c("labor", "labor market", "labour economics") ~ "labor",
                              keyword %in% c("life expectancy inequality", "lifespan inequality",
                                             "lifespan variation", "mortality inequalities", 
                                             "inequality in lifespan") ~ "lifespan inequality",
                              keyword %in% c("lifetables", "life tables", "lifetable methods", "multistate demography") ~ "life table",
                              keyword %in% c("mathematical demography", "mathematical modelling", "math") ~ "mathematical demography",
                              keyword %in% c("methods of demography", "formal demography ", "demographic methods",
                                             "formal demog", "formal methods", "methods", "demographic techniques") ~ "formal demography",
                              keyword %in% c("migration", "migration ", "mobility", "internal migration",
                                             "population displacement/mobility", "human mobility", "migrations",
                                             "currently working on migration statistics in south africa",
                                             "mexican migration", "international migration dynamics", "migration estimation") ~ "migration",
                              keyword %in% c("mortality", "mortality ", "mortality modeling", "mortality estimates",
                                             "mortality estimation", "mortality forecasting", "mortality models",
                                             "mortality selection", "sub-national mortality", "death", "demographic analysis of mortality",
                                             "i am experienced in mortality estimation", "death distribution methods") ~ "mortality",
                              keyword %in% c("premature mortality", "infant mortality") ~ "infant mortality",
                              keyword %in% c("pop health", "health", "population health", "fetal/perinatal health") ~ "health",
                              keyword %in% c("population projections", "population estimates", "population models", 
                                              "population trends", "modeling", "population estimation", "simulation models",
                                             "demographic estimation.", "demographic microsimulations",
                                             "forecasting", "forecasts", "population forecast", "population projection") ~ "projection/simulation",
                              keyword %in% c("reproductive", "infertility", "fertility", "fertility transition",
                                              "men's fertility", "birth", "childlessness", "late fertility") ~ "fertility",
                              keyword %in% c("small area", "small-area estimation", "subnational estimates",
                                              "subpopulations") ~ "subnational",
                              keyword %in% c("social demography.", "family demography", "family formation", "social demography",
                                             "kinship", "family", "family planning", "families", "family dynamics", "marriage",
                                             "nupciality", "partner markets") ~ "family demography",
                              keyword %in% c("spatial demography", "spatial inequality", "regional analysis") ~ "spatial demography",
                              keyword %in% c("social statistics", "statistical demography", "statistics",
                                             "stochastic processes", "survival analysis", "time series", "competing risks",
                                             "nonlinear models", "survey research methods") ~ "statistics",
                              keyword %in% c("demography", "population studies", "population") ~ "demography",
                              keyword %in% c("length of life", "life expectancy", "longevity") ~ "longevity",
                              keyword %in% c("social determinants of inequality", "inequality", "inequality measures", "inequalities") ~ "inequality",
                              keyword %in% c("social epidemiology", "epidemiological modelling") ~ "epidemiology",
                              keyword %in% c("data", "defective data", "missing data", "bias", "new forms of data",
                                             "survey data", "vital registration", "sampling", "vital statistics", "big data",
                                             "data collection.", "demographic data bases", "surveys") ~ "data",
                              keyword %in% c("development", "development studies", "human development") ~ "development",
                              keyword %in% c("lifecourse", "life course") ~ "life course",
                              keyword %in% c("lmics", "africa") ~ "lmics",
                              keyword %in% c("race", "ethnicity", "structural racism measures") ~ "race/ethnicity",
                              keyword %in% c("reproductive health", "pediatric", "sexual reproductive health rights of migrants",
                                             "maternal mortality") ~ "reproductive health",
                              keyword %in% c("social policies", "social policy", "policy") ~ "social policy",
                              keyword %in% c("climate", "climate change") ~ "climate change",
                              keyword %in% c("causal estimates", "causal inference") ~ "causal inference",
                              keyword %in% c("decomposition methods", "decomposition") ~ "decomposition",
                              keyword %in% c("environmental demography", "environmental") ~ "environmental demography",
                              keyword %in% c("household structure", "household dynamics") ~ "household",
                              keyword %in% c("computational methods", "computational statistics", "computational demography") ~ "computational demography",
                              T ~ keyword)) %>% 
  filter(keyword2 %out% c("geri’s ord", "hiv", "mapu")) %>% 
  count(keyword2)


pdf("out/Keyword_wordcloud.pdf")
set.seed(1)
wordcloud(words = d_cle$keyword2, freq = d_cle$n, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

#pdf("working_group/out/Keyword_wordcloud.pdf")
#set.seed(1)
#wordcloud(words = d_cle$keyword2, freq = d_cle$n, min.freq = 1,
#          max.words = 200, random.order = FALSE, rot.per = 0.35,
#          colors = brewer.pal(8, "Dark2"))
#dev.off()
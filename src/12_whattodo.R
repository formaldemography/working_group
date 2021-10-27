D %>% 
  separate(What.would.you.find.most.useful., sep = ";", into = c("key1", "key2", "key3", "key4", "key5")) %>%
  select("key1", "key2", "key3", "key4", "key5") %>% 
  gather(key = key , value = what) %>% 
  drop_na() %>% 
  count(what) %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = n, y = what)) +
  geom_bar(stat = "identity", fill = Mycol[4]) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 11))
ggsave("out/whattodo.pdf", width = 8, height = 5)
ggsave("working_group/out/whattodo.pdf", width = 8, height = 5)

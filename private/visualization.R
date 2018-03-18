df = brack$bracket.storage$data.matrix

# Help frame with unique configurations:
help.df = brack$bracket.storage$data.matrix %>%
  select(1:3) %>% # betrachte spalten 1 bis 3
  unique() %>% # davon die unique configs
  mutate(configuration = 1:nrow(.)) # mach für diese unique configs ne neue variable mit id

# interessanten parameter namen für späteren join:
nms = names(df)[1:3]


df.gg = df %>% left_join(help.df, by = nms) %>% # Join configuration id 
  group_by_at(vars(one_of(nms))) %>% # Group by parameter configs
  mutate(count = n()) # Zähle wie oft jede configuration vor kommt

# factor für ggplot
df.gg$configuration = as.factor(df.gg$configuration)


ggplot(df.gg, aes(x = current_budget, y = y, colour = configuration)) +
  geom_point(show.legend = FALSE) +
  geom_line(data = df.gg[df.gg$count > 1, ], aes(x = current_budget, y = y, colour = configuration), show.legend = FALSE)











df1 = data.frame(key = c("A", "B", "C", "A"))

df2 = data.frame(key = c("A", "B", "C"), value = 1:3)
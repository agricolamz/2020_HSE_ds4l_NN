library(tidyverse)
library(gutenbergr)
text <- gutenberg_download(34635)
text <- str_c(text$text, collapse = " ")
nchar(text)

library(udpipe)
library(tidytext)
texts_parsed <- udpipe(x = text, 
                       object = udpipe_load_model("data/polish-pdb-ud-2.4-190531.udpipe"))

texts_parsed %>% 
  count(token, lemma, upos, xpos, sort = TRUE) %>% 
  mutate(praet = str_detect(xpos, "praet")) %>% 
  filter(praet, upos == "VERB") %>%
  mutate(number = ifelse(str_detect(xpos, ":sg:"), "sg", "pl"),
         number = factor(number, levels = c("sg", "pl")),
         gender = str_extract(xpos, ":[mfn][123]?:"),
         gender = str_remove_all(gender, ":")) %>% 
  count(upos, number, gender) %>% 
  ggplot(aes(reorder_within(gender, n, number), n, fill = gender))+
  geom_col()+
  scale_x_reordered()+
  facet_wrap(~number, scales = "free")+
  labs(title = "Frequency of gender verb forms in past tense in Polish",
       subtitle = 'Based on Gabriela Zapolska "Menażerya ludzka"',
       caption = "data obtained from gutenberg.org",
       y = "number of verbs",
       x = "")

library(dplyr)
library(plyr)
library(ggplot2)

adopt_107 <- fileset$`107.csv`[4][[1]]
adopt_106 <- fileset$`106.csv`[3][[1]]
adopt_105 <- fileset$`105.csv`[3][[1]]
adopt_104 <- fileset$`104.csv`[3][[1]]
adopt_103 <- fileset$`103.csv`[3][[1]]
death_107 <- fileset$`107.csv`[6][[1]]
death_106 <- fileset$`106.csv`[6][[1]]
death_105 <- fileset$`105.csv`[6][[1]]
death_104 <- fileset$`104.csv`[6][[1]]
death_103 <- fileset$`103.csv`[6][[1]]
euthanasia_106 <- fileset$`106.csv`[4][[1]]
euthanasia_105 <- fileset$`105.csv`[4][[1]]
euthanasia_104 <- fileset$`104.csv`[4][[1]]
euthanasia_103 <- fileset$`103.csv`[4][[1]]

adopt_total <- c(adopt_103[23], adopt_104[23], adopt_105[23], adopt_106[23], adopt_107[23])
death_total <-c(death_103[23], death_104[23], death_105[23], death_106[23], death_107[23])
euthanasia_total <- c(euthanasia_103[23], euthanasia_104[23], euthanasia_105[23], euthanasia_106[23], 0)

df <- data.frame(year = c(103, 104, 105, 106, 107),
                 adopt_total, death_total)

data <- fileset$`107.csv`[-23,]

p1 <- ggplot(data = df) + 
  geom_line(aes(year, adopt_total, group = 1, colour = "認領養率")) +
  geom_line(aes(year, death_total, group = 1, colour = '所內死亡率')) +
  labs(x = "年", y = "percentage")

p2 <- ggplot(data) + 
  geom_bar(mapping = aes(x = reorder(縣市別, -animals_per_person), y = animals_per_person),
           stat = "identity") +
  labs(x = "縣市別", y = "照顧動物數/人")

p3 <- ggplot(data) + 
  geom_bar(mapping = aes(x = reorder(縣市別, -在養佔可留容比例), y = 在養佔可留容比例),
           stat = "identity") +
  labs(x = "縣市別", y = "在養佔可留容比例")

p4 <- ggplot(data) + 
  geom_bar(mapping = aes(x = reorder(縣市別, -total), y = total),
           stat = "identity") +
  labs(x = "縣市別", y = "在養數")

p5 <- ggplot(data) + 
  geom_bar(mapping = aes(x = reorder(縣市別, -`reliability(%)`), y = `reliability(%)`),
           stat = "identity") +
  labs(x = "縣市別", y = "可靠度")

p6 <- ggplot(data) + 
  geom_line(aes(縣市別, adopt, group = 1, colour = "認領養率")) +
  geom_line(aes(縣市別, `death_threat(%)`, group = 1, colour = '所內死亡率')) +
  labs(x = "縣市別", y = "percentage")

p7 <- ggplot(data = df) + 
  geom_text(aes(x = year, y = (euthanasia_total + 1), label = euthanasia_total)) +
  geom_line(aes(year, euthanasia_total, group = 1)) +
  labs(x = "年", y = "percentage")
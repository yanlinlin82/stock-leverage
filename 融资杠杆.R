library(tidyverse)

a = expand.grid(杠杆倍数 = seq(from=1, to=2, by=0.05),
                下跌 = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2)) %>%
  as_tibble() %>%
  mutate(新杠杆倍数 = 1 / (1 - (杠杆倍数 - 1) / (杠杆倍数 * (1 - 下跌)))) %>%
  mutate(下跌 = sprintf("%d%%",下跌*100),
         下跌 = factor(下跌, levels=rev(unique(下跌))))

g1 = a %>%
  ggplot(aes(x = 杠杆倍数, y = 新杠杆倍数, group = 下跌, color = 下跌)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=1.7, color="red", linetype=2) +
  scale_x_continuous(breaks = a$杠杆倍数, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from=1, to=3, by=0.1), minor_breaks = NULL) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color="lightgray"))

ggsave(g1, file="杠杆增加速度.png", width=12, height=8)

#---------------------------#

平仓线 = 1.3
警戒线 = 1.5

b = tibble(杠杆倍数 = seq(from=1, to=2, by=0.05)) %>%
  mutate(下跌至警戒 = (杠杆倍数 - 1) * 警戒线 / 杠杆倍数,
         下跌至平仓 = (杠杆倍数 - 1) * 平仓线 / 杠杆倍数) %>%
  pivot_longer(cols = -杠杆倍数, names_to="担保比例", values_to="容许下跌至") %>%
  mutate(担保比例 = sprintf("%s (%d%%)", 担保比例,
                            ifelse(担保比例=="下跌至警戒", 警戒线, 平仓线) * 100))

g2 = b %>%
  ggplot(aes(x = 杠杆倍数, y = 容许下跌至 * 100, group = 担保比例, color = 担保比例)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = a$杠杆倍数, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from=0, to=100, by=5)) +
  labs(y = "容许下跌至(%)") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color="lightgray"))

ggsave(g2, file="杠杆容许下跌程度.png", width=12, height=8)

st_summary <- mm %>% group_by(leg, corspcs) %>% summarize(n())

st_summary <- table(mm$corspcs, mm$leg)

write.table(st_summary, "mm_summary_mm_up4.csv", sep = ",")

mm4_bthy <- read_delim("mm4_bthytest.txt", delim = ",")

mm4_bthy <- mm4_bthy %>%
  mutate(bthy_diff = bthy_pro_old - bthy_pro)
 
ggplot(mm4_bthy, (aes(bthy_diff))) + 
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(-1000, 1000), ylim = c(0, 50)) #chopping off top of plot to see x spread better

pts_over100 <- mm4_bthy %>%
  filter(abs(bthy_diff) > 50) %>%
  group_by(strata1) %>%
  summarize(xbar = mean(abs(bthy_diff)))

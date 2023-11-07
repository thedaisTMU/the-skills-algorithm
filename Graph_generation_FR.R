library(DaisTheme)
library(ggplot2)
library(data.table)
library(ggpp)
library(scales)
library(ggrepel)
library(stringr)
library(tidyverse)
library(utils)

#########################################################
graph.data <-  fread("Graphs_spreadsheet.csv")
translation <- fread("Graphs_data/graph_translation_fr.csv")

figure_3_data_FR <- fread("Graphs_data/Figure_3.csv")
figure.3 <- ggplot(data=figure_3_data_FR,aes(x=1,y=round(-score, 0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(digital_assign),colour=as.character(digital_assign)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 3",Legend_label_1_FR],graph.data[graph.data$Figure_number=="Figure 3",Legend_label_2_FR]), name=graph.data[graph.data$Figure_number=="Figure 3",Legend_name_FR]) +
  scale_colour_manual(values =  c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 3",Legend_label_1_FR],graph.data[graph.data$Figure_number=="Figure 3",Legend_label_2_FR]),name=graph.data[graph.data$Figure_number=="Figure 3",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 3",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 3",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_4_data_FR <- fread("Graphs_data/Figure_4.csv")
translation_f4 <- translation %>%
  filter(str_detect(Figure, "Figure 4"))
figure_4_data_FR <- figure_4_data_FR%>%
  mutate(mod =  case_when(
    str_detect(mod, translation_f4$English[3]) ~ translation_f4$French[3],
    str_detect(mod, translation_f4$English[4]) ~ translation_f4$French[4],
    str_detect(mod, translation_f4$English[5]) ~ translation_f4$French[5],
    str_detect(mod, translation_f4$English[6]) ~ translation_f4$French[6],
    str_detect(mod, translation_f4$English[7]) ~ translation_f4$French[7],
    str_detect(mod, translation_f4$English[8]) ~ translation_f4$French[8]))

figure.4 <- ggplot(data=figure_4_data_FR,aes(x=1,y=round(-score, 0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=mod,colour=mod),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ff7200","#ffdc00","#eb0072","#7474c1","#6bbfae","#004c9b"),name=graph.data[graph.data$Figure_number=="Figure 4",Legend_name_FR]) +
  scale_colour_manual(values =  c("#ff7200","#ffdc00","#eb0072","#7474c1","#6bbfae","#004c9b"),name=graph.data[graph.data$Figure_number=="Figure 4",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 4",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 4",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())+
  guides(fill = guide_legend(nrow = 3))

figure_5_data_FR <- fread("Graphs_data/Figure_5.csv")
translation_f5 <- translation %>%
  filter(str_detect(Figure, "Figure 5"))
figure_5_data_FR <- figure_5_data_FR%>%
  mutate(lv.mod =  case_when(
    str_detect(lv.mod, translation_f5$English[3]) ~ translation_f5$French[3],
    str_detect(lv.mod, translation_f5$English[4]) ~ translation_f5$French[4],
    str_detect(lv.mod, translation_f5$English[5]) ~ translation_f5$French[5],
    str_detect(lv.mod, translation_f5$English[6]) ~ translation_f5$French[6],
    str_detect(lv.mod, translation_f5$English[7]) ~ translation_f5$French[7],
    str_detect(lv.mod, translation_f5$English[8]) ~ translation_f5$French[8]))

figure.5 <- ggplot(data=figure_5_data_FR,aes(x=1,y=round(-score, 0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels=unique(figure_5_data_FR$lv.mod),name=graph.data[graph.data$Figure_number=="Figure 5",Legend_name_FR]) +
  scale_colour_manual(values =  c("#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels=unique(figure_5_data_FR$lv.mod),name=graph.data[graph.data$Figure_number=="Figure 5",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 5",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 5",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = 10), legend.title = element_text(size = 10))+
  guides(fill = guide_legend(nrow = 3))

figure_6_data_FR <- fread("Graphs_data/Figure_6.csv")
translation_f6 <- translation %>%
  filter(str_detect(Figure, "Figure 6"))
figure_6_data_FR <- figure_6_data_FR%>%
  mutate(ht_df =  case_when(
    str_detect(ht_df, translation_f6$English[3]) ~ translation_f6$French[3],
    str_detect(ht_df, translation_f6$English[4]) ~ translation_f6$French[4],
    str_detect(ht_df, translation_f6$English[5]) ~ translation_f6$French[5],
    str_detect(ht_df, translation_f6$English[6]) ~ translation_f6$French[6],
    str_detect(ht_df, translation_f6$English[7]) ~ translation_f6$French[7],
    str_detect(ht_df, translation_f6$English[9]) ~ translation_f6$French[9],
    str_detect(ht_df, translation_f6$English[10]) ~ translation_f6$French[10]))

figure.6 <- ggplot(data=figure_6_data_FR,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_6_data_FR$ht_df),name=graph.data[graph.data$Figure_number=="Figure 6",Legend_name_FR]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_6_data_FR$ht_df), name=graph.data[graph.data$Figure_number=="Figure 6",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 6",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 6",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())+
  guides(fill = guide_legend(nrow = 3))

figure_7_data_FR <- fread("Graphs_data/Figure_7.csv")
translation_f7 <- translation %>%
  filter(str_detect(Figure, "Figure 7"))
figure_7_data_FR <- figure_7_data_FR%>%
  mutate(bm_df =  case_when(
    str_detect(bm_df, translation_f7$English[3]) ~ translation_f7$French[3],
    str_detect(bm_df, translation_f7$English[4]) ~ translation_f7$French[4],
    str_detect(bm_df, translation_f7$English[5]) ~ translation_f7$French[5],
    str_detect(bm_df, translation_f7$English[6]) ~ translation_f7$French[6],
    str_detect(bm_df, translation_f7$English[7]) ~ translation_f7$French[7],
    str_detect(bm_df, translation_f7$English[9]) ~ translation_f7$French[9],
    str_detect(bm_df, translation_f7$English[10]) ~ translation_f7$French[10]))

figure.7 <- ggplot(data=figure_7_data_FR,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_7_data_FR$bm_df),name=graph.data[graph.data$Figure_number=="Figure 7",Legend_name_FR]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_7_data_FR$bm_df), name=graph.data[graph.data$Figure_number=="Figure 7",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5))+
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 7",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 7",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())+
  guides(fill = guide_legend(nrow = 3))

figure_8_data_FR <- fread("Graphs_data/Figure_8.csv")
translation_f8 <- translation %>%
  filter(str_detect(Figure, "Figure 8"))
figure_8_data_FR <- figure_8_data_FR%>%
  mutate(go_df =  case_when(
    str_detect(go_df, translation_f8$English[3]) ~ translation_f8$French[3],
    str_detect(go_df, translation_f8$English[4]) ~ translation_f8$French[4],
    str_detect(go_df, translation_f8$English[5]) ~ translation_f8$French[5],
    str_detect(go_df, translation_f8$English[6]) ~ translation_f8$French[6],
    str_detect(go_df, translation_f8$English[7]) ~ translation_f8$French[7],
    str_detect(go_df, translation_f8$English[9]) ~ translation_f8$French[9],
    str_detect(go_df, translation_f8$English[10]) ~ translation_f8$French[10]))

figure.8 <- ggplot(data=figure_8_data_FR,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#6bbfae"),labels = unique(figure_8_data_FR$go_df),name=graph.data[graph.data$Figure_number=="Figure 8",Legend_name_FR]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#6bbfae"),labels = unique(figure_8_data_FR$go_df), name=graph.data[graph.data$Figure_number=="Figure 8",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 8",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 8",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_9_data_FR <- fread("Graphs_data/Figure_9.csv")
translation_f9 <- translation %>%
  filter(str_detect(Figure, "Figure 9"))
figure_9_data_FR <- figure_9_data_FR%>%
  mutate(ci_df =  case_when(
    str_detect(ci_df, translation_f9$English[3]) ~ translation_f9$French[3],
    str_detect(ci_df, translation_f9$English[4]) ~ translation_f9$French[4],
    str_detect(ci_df, translation_f9$English[5]) ~ translation_f9$French[5],
    str_detect(ci_df, translation_f9$English[6]) ~ translation_f9$French[6],
    str_detect(ci_df, translation_f9$English[7]) ~ translation_f9$French[7],
    str_detect(ci_df, translation_f9$English[9]) ~ translation_f9$French[9],
    str_detect(ci_df, translation_f9$English[10]) ~ translation_f9$French[10]))

figure.9 <- ggplot(data=figure_9_data_FR,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_9_data_FR$ci_df),name=graph.data[graph.data$Figure_number=="Figure 9",Legend_name_FR]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_9_data_FR$ci_df), name=graph.data[graph.data$Figure_number=="Figure 9",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 9",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 9",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())+
  guides(fill = guide_legend(nrow = 3))

figure_10_data_FR <- fread("Graphs_data/Figure_10.csv")
translation_f10 <- translation %>%
  filter(str_detect(Figure, "Figure 10"))
figure_10_data_FR <- figure_10_data_FR%>%
  mutate(trades_df =  case_when(
    str_detect(trades_df, translation_f10$English[3]) ~ translation_f10$French[3],
    str_detect(trades_df, translation_f10$English[4]) ~ translation_f10$French[4],
    str_detect(trades_df, translation_f10$English[5]) ~ translation_f10$French[5],
    str_detect(trades_df, translation_f10$English[6]) ~ translation_f10$French[6],
    str_detect(trades_df, translation_f10$English[7]) ~ translation_f10$French[7],
    str_detect(trades_df, translation_f10$English[9]) ~ translation_f10$French[9],
    str_detect(trades_df, translation_f10$English[10]) ~ translation_f10$French[10]))

figure.10 <- ggplot(data=figure_10_data_FR,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values =c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_10_data_FR$trades_df),name="Digital sub-cluster") +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_10_data_FR$trades_df),name="Digital sub-cluster") +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 10",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 10",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())+
  guides(fill = guide_legend(nrow = 3))

figure_11_data_FR <- fread("Graphs_data/Figure_11.csv")
translation_f11 <- translation %>%
  filter(str_detect(Figure, "Figure 11"))
figure_11_data_FR <- figure_11_data_FR%>%
  mutate(sm_df =  case_when(
    str_detect(sm_df, translation_f11$English[3]) ~ translation_f11$French[3],
    str_detect(sm_df, translation_f11$English[4]) ~ translation_f11$French[4],
    str_detect(sm_df, translation_f11$English[5]) ~ translation_f11$French[5],
    str_detect(sm_df, translation_f11$English[6]) ~ translation_f11$French[6],
    str_detect(sm_df, translation_f11$English[7]) ~ translation_f11$French[7],
    str_detect(sm_df, translation_f11$English[9]) ~ translation_f11$French[9],
    str_detect(sm_df, translation_f11$English[10]) ~ translation_f11$French[10]))

figure.11 <- ggplot(data=figure_11_data_FR,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_11_data_FR$sm_df),name = graph.data[graph.data$Figure_number=="Figure 11",Legend_name_FR]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_11_data_FR$sm_df), name = graph.data[graph.data$Figure_number=="Figure 11",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_Ticks_2_FR])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 11",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 11",Figure_title_FR],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())+
  guides(fill = guide_legend(nrow = 3))

figure_12_data_FR <- fread("Graphs_data/Figure_12.csv")
translation_f12 <- translation %>%
  filter(str_detect(Figure, "Figure 12"))
graph.data$X_Axis_FR <- gsub("’","'", graph.data$X_Axis_FR)
graph.data$Y_Axis_FR <- gsub("’","'", graph.data$Y_Axis_FR)
figure_12_data_FR <- figure_12_data_FR%>%
  mutate(g_s =  case_when(
    str_detect(g_s, translation_f12$English[5]) ~ translation_f12$French[5],
    str_detect(g_s, translation_f12$English[4]) ~ translation_f12$French[4]))

figure.12 <- ggplot(data=figure_12_data_FR,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=g_s)) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure 12",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 12",Figure_title_FR],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure 12",Legend_name_FR])+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  guides(size = 'none', color = guide_legend(nrow = 2))+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure 12", X_Axis_FR], size = 5)+
  annotate("text", x = 432, y = 200, label =  graph.data[graph.data$Figure_number=="Figure 12", Y_Axis_FR], angle = 90, size = 5) 

figure_13_data_FR <- fread("Graphs_data/Figure_13.csv")
translation_f13 <- translation %>%
  filter(str_detect(Figure, "Figure 13"))
# graph.data$X_Axis_FR <- gsub("’","'", graph.data$X_Axis_FR)
# graph.data$Y_Axis_FR <- gsub("’","'", graph.data$Y_Axis_FR)
figure_13_data_FR <- figure_13_data_FR%>%
  mutate(ht_bm =  case_when(
    str_detect(ht_bm, translation_f13$English[5]) ~ translation_f13$French[5],
    str_detect(ht_bm, translation_f13$English[4]) ~ translation_f13$French[4]))

figure.13 <- ggplot(data=figure_13_data_FR,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=ht_bm)) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure 13",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 13",Figure_title_FR],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure 13",Legend_name_FR])+
  guides(size = 'none', color = guide_legend(nrow = 2))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure 13", X_Axis_FR], size = 5)+
  annotate("text", x = 432, y = 200, label =  graph.data[graph.data$Figure_number=="Figure 13", Y_Axis_FR], angle = 90, size = 5) 

figure_14_data_FR <- fread("Graphs_data/Figure_14.csv")

figure.14 <- ggplot(data=figure_14_data_FR,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(SPD_D.dum),colour=as.character(SPD_D.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"), labels = c(graph.data[graph.data$Figure_number=="Figure 14",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 14",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 14",Legend_name_FR]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"), labels = c(graph.data[graph.data$Figure_number=="Figure 14",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 14",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 14",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-400,-60),labels = c(graph.data[graph.data$Figure_number=="Figure 14",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 14",Y_Axis_Ticks_2_FR])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 14",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 14",Figure_title_FR],
       y="",
       x="")

figure_15_data_FR <- fread("Graphs_data/Figure_15.csv")
figure.15 <- ggplot(data=figure_15_data_FR,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(C_SI.dum),colour=as.character(C_SI.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 15",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 15",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 15",Legend_name_FR]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"), labels = c(graph.data[graph.data$Figure_number=="Figure 15",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 15",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 15",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-400,-60),labels = c(graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_Ticks_2_FR])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 15",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 15",Figure_title_FR],
       y="",
       x="")

figure_16_data_FR <- fread("Graphs_data/Figure_16.csv")
figure.16 <- ggplot(data=figure_16_data_FR,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(IM_geo.dum),colour=as.character(IM_geo.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 16",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 16",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 16",Legend_name_FR]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"),labels =c(graph.data[graph.data$Figure_number=="Figure 16",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 16",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 16",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-400,-60),labels = c(graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_Ticks_2_FR])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 16",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 16",Figure_title_FR],
       y="",
       x="")

figure_17_data_FR <- fread("Graphs_data/Figure_17.csv")
figure.17 <- ggplot(data=figure_17_data_FR,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(d_m.dum),colour=as.character(d_m.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 17",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 17",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 17",Legend_name_FR]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"),labels =c(graph.data[graph.data$Figure_number=="Figure 17",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 17",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 17",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-400,-60),labels = c(graph.data[graph.data$Figure_number=="Figure 17",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 17",Y_Axis_Ticks_2_FR])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 17",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 17",Figure_title_FR],
       y="",
       x="")

figure_18_data_FR <- fread("Graphs_data/Figure_18.csv")
figure.18 <- ggplot(data=figure_18_data_FR,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(workforce.dum),colour=as.character(workforce.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 18",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 18",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 18",Legend_name_FR]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"),labels =c(graph.data[graph.data$Figure_number=="Figure 18",Legend_label_1_FR], graph.data[graph.data$Figure_number=="Figure 18",Legend_label_2_FR]), name = graph.data[graph.data$Figure_number=="Figure 18",Legend_name_FR]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-400,-60),labels = c(graph.data[graph.data$Figure_number=="Figure 18",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 18",Y_Axis_Ticks_2_FR])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 18",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 18",Figure_title_FR],
       y="",
       x="")

figure_20_data_FR <- fread("Graphs_data/Figure_20.csv")
translation_f20 <- translation %>%
  filter(str_detect(Figure, "Figure 20"))
figure_20_data_FR <- figure_20_data_FR%>%
  mutate(name_FR =  case_when(
    str_detect(name, translation_f20$English[9]) ~ translation_f20$French[9],
    str_detect(name, translation_f20$English[11]) ~ translation_f20$French[11],
    str_detect(name, translation_f20$English[12]) ~ translation_f20$French[12],
    str_detect(name, translation_f20$English[16]) ~ translation_f20$French[16],
    str_detect(name, translation_f20$English[17]) ~ translation_f20$French[17],
    str_detect(name, translation_f20$English[18]) ~ translation_f20$French[18],
    TRUE ~ as.character(name)))%>%
  mutate(lv.mod =  case_when(
    str_detect(lv.mod, translation_f20$English[1]) ~ translation_f20$French[1],
    str_detect(lv.mod, translation_f20$English[2]) ~ translation_f20$French[2],
    str_detect(lv.mod, translation_f20$English[3]) ~ translation_f20$French[3],
    str_detect(lv.mod, translation_f20$English[4]) ~ translation_f20$French[4],
    str_detect(lv.mod, translation_f20$English[5]) ~ translation_f20$French[5]))
size = 5

figure.20 <- ggplot(data=figure_20_data_FR,aes(x=-score,y=growth))+
  dais.base.theme() +
  geom_point(aes(size = (count_2021)^0.5*100, colour=lv.mod)) +
  scale_colour_manual(values = c("#6bbfae","#eb0072","#004c9b","#ffdc00","#7474c1"))+
  scale_x_continuous(limits = c(-199,0),breaks = c(-180,-15),labels = c(graph.data[graph.data$Figure_number=="Figure 20",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 20",Y_Axis_Ticks_2_FR]))+
  scale_y_continuous(limits = c(-.8, 1.5), labels = percent_format(accuracy = 1))+
  geom_hline(yintercept=0, linetype="dotted", color = "black")+
  labs(title = graph.data[graph.data$Figure_number=="Figure 20",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 20",Figure_title_FR],
       y=graph.data[graph.data$Figure_number=="Figure 20",Y_Axis_FR],
       x=graph.data[graph.data$Figure_number=="Figure 20",X_Axis_FR],
       color=graph.data[graph.data$Figure_number=="Figure 20",Legend_name_FR])+
  guides(size = 'none', color = guide_legend(nrow = 3))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 20, color="#004c9b"), plot.subtitle = element_text(size = 18))+
  geom_text_repel(data=subset(figure_20_data_FR,
                              str_detect(name,'^C\\++')),
                  aes(-score,growth,label=figure_20_data_FR[str_detect(figure_20_data_FR$name,'^C\\++'),name_FR]),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Artificial Intelligence (AI)'),
                  aes(-score,growth,label='IA'),
                  nudge_x = 2, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Computer terminals'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = 10, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Cisco Systems WebEx'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Google Sheets'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              str_detect(name,'^C\\#')),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Microsoft Power BI'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Desktop computers'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = 2, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name=='Digital Marketing'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.75, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              str_detect(name,'^Salesforce CRM')),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -2, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              str_detect (name_FR,'Microsoft Azure')),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              str_detect (name,'Oracle Learning Management')),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              str_detect (name,'Microsoft Active Directory')),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Microsoft Office'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = 1, nudge_y = 0.14,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Microsoft Excel'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = 8, nudge_y = -0.07,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Microsoft Word'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = -8, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'JavaScript'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = 0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Amazon Web Services (AWS)'),
                  aes(-score,growth,label='AWS'),
                  nudge_x = 2, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'SQL (Structured query language)'),
                  aes(-score,growth,label='SQL'),
                  nudge_x = 1, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data_FR,
                              name == 'Tableau'),
                  aes(-score,growth,label=name_FR),
                  nudge_x = 1, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')

wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}

figure_21_data_FR <- fread("Graphs_data/Figure_21.csv")
figure.21 <- plot.line.dais(data=figure_21_data_FR,x=Month,y=AI_pct, show.points = TRUE,
                            plot.fig.num = graph.data[graph.data$Figure_number=="Figure 21",Figure_number],
                            unit.y = "%",
                            x.axis = graph.data[graph.data$Figure_number=="Figure 21",X_Axis_FR],
                            y.axis = graph.data[graph.data$Figure_number=="Figure 21",Y_Axis_FR],
                            plot.title = wrapper(graph.data[graph.data$Figure_number=="Figure 21",Figure_title_FR],70))+
             scale_x_date(breaks = figure_21_data_FR$Month, date_labels = "%m/%y")+
             scale_y_continuous(limits = c(0,0.02), labels = percent)+
             theme(plot.title = element_text(color="#004c9b"))

figure_22_data_FR <- fread("Graphs_data/Figure_22.csv")
translation_f22 <- translation %>%
  filter(str_detect(Figure, "Figure 22"))
figure_22_data_FR <- figure_22_data_FR%>%
  mutate(groupName =  case_when(
    str_detect(groupName, translation_f22$English[1]) ~ translation_f22$French[1],
    str_detect(groupName, translation_f22$English[2]) ~ translation_f22$French[2],
    str_detect(groupName, translation_f22$English[3]) ~ translation_f22$French[3],
    str_detect(groupName, translation_f22$English[4]) ~ translation_f22$French[4]))%>%
  mutate(subGroupName_FR =  case_when(
    str_detect(subGroupName, translation_f22$English[6]) ~ translation_f22$French[6],
    str_detect(subGroupName, translation_f22$English[7]) ~ translation_f22$French[7],
    str_detect(subGroupName, translation_f22$English[8]) ~ translation_f22$French[8],
    str_detect(subGroupName, translation_f22$English[9]) ~ translation_f22$French[9],
    str_detect(subGroupName, translation_f22$English[10]) ~ translation_f22$French[10],
    str_detect(subGroupName, translation_f22$English[11]) ~ translation_f22$French[11],
    str_detect(subGroupName, translation_f22$English[12]) ~ translation_f22$French[12],
    str_detect(subGroupName, translation_f22$English[13]) ~ translation_f22$French[13],
    str_detect(subGroupName, translation_f22$English[14]) ~ translation_f22$French[14],
    str_detect(subGroupName, translation_f22$English[15]) ~ translation_f22$French[15],
    str_detect(subGroupName, translation_f22$English[16]) ~ translation_f22$French[16],
    TRUE ~ as.character(subGroupName)))
size = 5

figure.22 <- ggplot(data=figure_22_data_FR,aes(x=-wm,y=growth))+
  dais.base.theme() +
  geom_point(aes(size = count_2021^0.4, colour=groupName)) +
  scale_colour_manual(values = c("#eb0072","#7474c1","#6bbfae","#ff7200"))+
  scale_x_continuous(limits = c(-400,0),breaks = c(-385,-15),labels = c(graph.data[graph.data$Figure_number=="Figure 22",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure 22",Y_Axis_Ticks_2_FR]))+
  scale_y_continuous(limits = c(min(figure_22_data_FR$growth), 1.8), labels = percent_format(accuracy = 1))+
  geom_hline(yintercept=0, linetype="dotted", color = "black")+
  labs(title = graph.data[graph.data$Figure_number=="Figure 22",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 22",Figure_title_FR],
       y=graph.data[graph.data$Figure_number=="Figure 22",Y_Axis_FR],
       x=graph.data[graph.data$Figure_number=="Figure 22",X_Axis_FR],
       color=graph.data[graph.data$Figure_number=="Figure 22",Legend_name_FR])+
  guides(size = 'none', color = guide_legend(nrow = 2))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        plot.title = element_text(size = 20, color="#004c9b"), plot.subtitle = element_text(size = 18))+
  geom_text_repel(data=subset(figure_22_data_FR,
                              growth >0.75),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = 0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              str_detect (subGroupName,'Artificial Intelligence')),
                  aes(-wm,growth,label="Intelligence artificielle"),
                  nudge_x = 0.5, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              subGroupName == "Personal qualities"),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = 0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              subGroupName == "Interpersonal"),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = 1, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              subGroupName == "Languages"),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = 0.2, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              subGroupName == "Helpdesk or call center software"),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = 0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              subGroupName == "Business intelligence and data analysis software"),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = -0.2, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              subGroupName == "Information Technology and telecommunications"),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = -0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data_FR,
                              subGroupName == "Health and safety"),
                  aes(-wm,growth,label=subGroupName_FR),
                  nudge_x = -0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_smooth(method = "lm", mapping = aes(weight = count_2021), 
              color = "black", show.legend = FALSE)

figure_a1_data_FR <- fread("Graphs_data/Figure_A1.csv")
translation_fa1 <- translation %>%
  filter(str_detect(Figure, "Figure A.1"))
figure_a1_data_FR <- figure_a1_data_FR%>%
  mutate(lv.mod =  case_when(
    str_detect(lv.mod, translation_fa1$English[1]) ~ translation_fa1$French[1],
    str_detect(lv.mod, translation_fa1$English[2]) ~ translation_fa1$French[2],
    str_detect(lv.mod, translation_fa1$English[3]) ~ translation_fa1$French[3],
    str_detect(lv.mod, translation_fa1$English[4]) ~ translation_fa1$French[4],
    str_detect(lv.mod, translation_fa1$English[5]) ~ translation_fa1$French[5]))

figure.a1 <- ggplot(data=figure_a1_data_FR,aes(x=-score,y=growth))+
  dais.base.theme() +
  geom_point(aes(size = (count_2021)^0.5*100, colour=lv.mod)) +
  scale_colour_manual(values = c("#eb0072","#7474c1","#ff7200","#6bbfae","#ffdc00","#004c9b"))+
  scale_x_continuous(limits = c(-400,0),breaks = c(-385,-15),labels = c(graph.data[graph.data$Figure_number=="Figure A.1",Y_Axis_Ticks_1_FR],graph.data[graph.data$Figure_number=="Figure A.1",Y_Axis_Ticks_2_FR]))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  geom_hline(yintercept=0, linetype="dotted", color = "black")+
  labs(title = graph.data[graph.data$Figure_number=="Figure A.1",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure A.1",Figure_title_FR],
       y=graph.data[graph.data$Figure_number=="Figure A.1",Y_Axis_FR],
       x=graph.data[graph.data$Figure_number=="Figure A.1",X_Axis_FR],
       color=graph.data[graph.data$Figure_number=="Figure A.1",Legend_name_FR])+
  guides(size = 'none', color = guide_legend(nrow = 3))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        plot.title = element_text(size = 20, color="#004c9b"), plot.subtitle = element_text(size = 18))+
  annotate("rect", xmin=-200, xmax=0, ymin=-0.8, ymax=1.5, alpha=0.1, fill='orange')

figure_a2_data_FR <- fread("Graphs_data/Figure_A2.csv")
translation_fa2 <- translation %>%
  filter(str_detect(Figure, "Figure A.2"))
# graph.data$X_Axis_FR <- gsub("’","'", graph.data$X_Axis_FR)
# graph.data$Y_Axis_FR <- gsub("’","'", graph.data$Y_Axis_FR)
figure_a2_data_FR <- figure_a2_data_FR%>%
  mutate(gobm =  case_when(
    str_detect(gobm, translation_fa2$English[5]) ~ translation_fa2$French[5],
    str_detect(gobm, translation_fa2$English[4]) ~ translation_fa2$French[4]))
figure.a2 <- ggplot(data=figure_a2_data_FR,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=gobm)) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure A.2",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure A.2",Figure_title_FR],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure A.2",Legend_name_FR])+
  guides(size = 'none')+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  guides(size = 'none', color = guide_legend(nrow =2))+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure A.2", X_Axis_FR],size = 5)+
  annotate("text", x = 432, y = 200, label = graph.data[graph.data$Figure_number=="Figure A.2", Y_Axis_FR], angle = 90, size = 5)

figure_a3_data_FR <- fread("Graphs_data/Figure_A3.csv")
translation_fa3 <- translation %>%
  filter(str_detect(Figure, "Figure A.3"))
# graph.data$X_Axis_FR <- gsub("’","'", graph.data$X_Axis_FR)
# graph.data$Y_Axis_FR <- gsub("’","'", graph.data$Y_Axis_FR)
figure_a3_data_FR <- figure_a3_data_FR%>%
  mutate(ht_sm =  case_when(
    str_detect(ht_sm, translation_fa3$English[5]) ~ translation_fa3$French[5],
    str_detect(ht_sm, translation_fa3$English[4]) ~ translation_fa3$French[4]))
figure.a3 <- ggplot(data=figure_a3_data_FR,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=ht_sm)) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure A.3",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure A.3",Figure_title_FR],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure A.3",Legend_name_FR])+
  guides(size = 'none', color = guide_legend(nrow = 2))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  guides(size = 'none' )+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure A.3", X_Axis_FR], size = 5)+
  annotate("text", x = 432, y = 200, label = graph.data[graph.data$Figure_number=="Figure A.3", Y_Axis_FR], angle = 90, size = 5)

export.dais.plot("Graph_exports_FR/f3_FR.pdf",figure.3, p.height = 5, p.width = 8)
export.dais.plot("Graph_exports_FR/f4_FR.pdf",figure.4, p.height = 5, p.width = 8)
export.dais.plot("Graph_exports_FR/f5_FR.pdf",figure.5, p.height = 6.25, p.width = 10)
export.dais.plot("Graph_exports_FR/f6_FR.pdf",figure.6, p.height = 7, p.width = 11)
export.dais.plot("Graph_exports_FR/f7_FR.pdf",figure.7, p.height = 7, p.width = 11)
export.dais.plot("Graph_exports_FR/f8_FR.pdf",figure.8, p.height = 7, p.width = 11)
export.dais.plot("Graph_exports_FR/f9_FR.pdf",figure.9, p.height = 7, p.width = 12)
export.dais.plot("Graph_exports_FR/f10_FR.pdf",figure.10, p.height = 7, p.width = 11)
export.dais.plot("Graph_exports_FR/f11_FR.pdf",figure.11, p.height = 7, p.width = 12)
export.dais.plot("Graph_exports_FR/f12_FR.pdf",figure.12, p.height = 11, p.width = 18)
export.dais.plot("Graph_exports_FR/f13_FR.pdf",figure.13, p.height = 11, p.width = 18)
export.dais.plot("Graph_exports_FR/f14_FR.pdf",figure.14, p.height = 5, p.width = 8)
export.dais.plot("Graph_exports_FR/f15_FR.pdf",figure.15, p.height = 5, p.width = 8)
export.dais.plot("Graph_exports_FR/f16_FR.pdf",figure.16, p.height = 5, p.width = 8)
export.dais.plot("Graph_exports_FR/f17_FR.pdf",figure.17, p.height = 5, p.width = 8)
export.dais.plot("Graph_exports_FR/f18_FR.pdf",figure.18, p.height = 5, p.width = 8)
export.dais.plot("Graph_exports_FR/f20_FR.pdf",figure.20, p.height = 12, p.width = 16)
export.dais.plot("Graph_exports_FR/f21_FR.pdf", figure.21)
export.dais.plot("Graph_exports_FR/f22_FR.pdf",figure.22, p.height = 12, p.width = 16)
export.dais.plot("Graph_exports_FR/fa1_FR.pdf",figure.a1, p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_FR/fa2_FR.pdf",figure.a2, p.height = 11, p.width = 18)
export.dais.plot("Graph_exports_FR/fa3_FR.pdf",figure.a3, p.height = 11, p.width = 18)

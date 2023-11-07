library(DaisTheme)
library(ggplot2)
library(data.table)
library(ggpp)
library(scales)
library(ggrepel)
library(stringr)

#########################################################
#Load in graph spread sheets
graph.data <-  fread("Graphs_spreadsheet.csv")

figure_3_data <- fread("Graphs_data/Figure_3.csv")
figure.3 <- ggplot(data=figure_3_data,aes(x=1,y=round(-score, 0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(digital_assign),colour=as.character(digital_assign)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 3",Legend_label_1],graph.data[graph.data$Figure_number=="Figure 3",Legend_label_2]), name=graph.data[graph.data$Figure_number=="Figure 3",Legend_name]) +
  scale_colour_manual(values =  c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 3",Legend_label_1],graph.data[graph.data$Figure_number=="Figure 3",Legend_label_2]),name=graph.data[graph.data$Figure_number=="Figure 3",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 3",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 3",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_4_data <- fread("Graphs_data/Figure_4.csv")
figure.4 <- ggplot(data=figure_4_data,aes(x=1,y=round(-score, 0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=mod,colour=mod),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#eb0072","#7474c1","#ff7200","#6bbfae","#ffdc00","#004c9b"),name=graph.data[graph.data$Figure_number=="Figure 4",Legend_name]) +
  scale_colour_manual(values =  c("#eb0072","#7474c1","#ff7200","#6bbfae","#ffdc00","#004c9b"),name=graph.data[graph.data$Figure_number=="Figure 4",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 4",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 4",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_5_data <- fread("Graphs_data/Figure_5.csv")
figure.5 <- ggplot(data=figure_5_data,aes(x=1,y=round(-score, 0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels=unique(figure_5_data$lv.mod),name=graph.data[graph.data$Figure_number=="Figure 5",Legend_name]) +
  scale_colour_manual(values =  c("#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels=unique(figure_5_data$lv.mod),name=graph.data[graph.data$Figure_number=="Figure 5",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 5",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 5",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = 10), legend.title = element_text(size = 10))

figure_6_data <- fread("Graphs_data/Figure_6.csv")
figure.6 <- ggplot(data=figure_6_data,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_6_data$ht_df),name=graph.data[graph.data$Figure_number=="Figure 6",Legend_name]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_6_data$ht_df), name=graph.data[graph.data$Figure_number=="Figure 6",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 6",Figure_number],
       subtitle=graph.data[graph.data$Figure_number=="Figure 6",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_7_data <- fread("Graphs_data/Figure_7.csv")
figure.7 <- ggplot(data=figure_7_data,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_7_data$bm_df),name=graph.data[graph.data$Figure_number=="Figure 7",Legend_name]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_7_data$bm_df), name=graph.data[graph.data$Figure_number=="Figure 7",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5))+
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 7",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 7",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_8_data <- fread("Graphs_data/Figure_8.csv")
figure.8 <- ggplot(data=figure_8_data,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#6bbfae"),labels = unique(figure_8_data$go_df),name=graph.data[graph.data$Figure_number=="Figure 8",Legend_name]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#6bbfae"),labels = unique(figure_8_data$go_df), name=graph.data[graph.data$Figure_number=="Figure 8",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 8",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 8",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_9_data <- fread("Graphs_data/Figure_9.csv")
figure.9 <- ggplot(data=figure_9_data,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_9_data$ci_df),name=graph.data[graph.data$Figure_number=="Figure 9",Legend_name]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_9_data$ci_df), name=graph.data[graph.data$Figure_number=="Figure 9",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 9",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 9",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_10_data <- fread("Graphs_data/Figure_10.csv")
figure.10 <- ggplot(data=figure_10_data,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values =c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_10_data$trades_df),name="Digital sub-cluster") +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_10_data$trades_df),name="Digital sub-cluster") +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 10",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 10",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_11_data <- fread("Graphs_data/Figure_11.csv")
figure.11 <- ggplot(data=figure_11_data,aes(x=1,y=round(-score,0))) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(order),colour=as.character(order)),
               binaxis='y',
               binwidth=0.4,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_11_data$sm_df),name = graph.data[graph.data$Figure_number=="Figure 11",Legend_name]) +
  scale_colour_manual(values = c("#C0C0C0","#808080","#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"),labels = unique(figure_11_data$sm_df), name = graph.data[graph.data$Figure_number=="Figure 11",Legend_name]) +
  scale_x_continuous(limits = c(0.5,1.5)) +
  scale_y_continuous(breaks = c(-450,-50),labels = c(graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_Ticks_2])) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 11",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 11",Figure_title],
       y="",
       x="")+
  theme(axis.text.x = element_text(color="#004c9b"), plot.title = element_text(color="#004c9b"), axis.ticks = element_blank(),axis.text.y = element_blank())

figure_12_data <- fread("Graphs_data/Figure_12.csv")
figure.12 <- ggplot(data=figure_12_data,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=g_s)) +
  scale_colour_manual(values = c("#004c9b","#ffdc00"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure 12",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 12",Figure_title],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure 12",Legend_name])+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  guides(size = 'none')+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure 12", X_Axis], size = 5)+
  annotate("text", x = 432, y = 200, label =  graph.data[graph.data$Figure_number=="Figure 12", Y_Axis], angle = 90, size = 5) 

figure_13_data <- fread("Graphs_data/Figure_13.csv")
figure.13 <- ggplot(data=figure_13_data,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=ht_bm)) +
  scale_colour_manual(values = c("#004c9b","#ffdc00"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure 13",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 13",Figure_title],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure 13",Legend_name])+
  guides(size = 'none')+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  guides(size = 'none')+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure 13", X_Axis], size = 5)+
  annotate("text", x = 432, y = 200, label =  graph.data[graph.data$Figure_number=="Figure 13", Y_Axis], angle = 90, size = 5) 

figure_14_data <- fread("Graphs_data/Figure_14.csv")
figure.14 <- ggplot(data=figure_14_data,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(SPD_D.dum),colour=as.character(SPD_D.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"), labels = c(graph.data[graph.data$Figure_number=="Figure 14",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 14",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 14",Legend_name]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"), labels = c(graph.data[graph.data$Figure_number=="Figure 14",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 14",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 14",Legend_name]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-430,-30),labels = c(graph.data[graph.data$Figure_number=="Figure 14",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 14",Y_Axis_Ticks_2])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 14",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 14",Figure_title],
       y="",
       x="")

figure_15_data <- fread("Graphs_data/Figure_15.csv")
figure.15 <- ggplot(data=figure_15_data,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(C_SI.dum),colour=as.character(C_SI.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 15",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 15",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 15",Legend_name]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"), labels = c(graph.data[graph.data$Figure_number=="Figure 15",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 15",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 15",Legend_name]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-430,-30),labels = c(graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 15",Y_Axis_Ticks_2])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 15",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 15",Figure_title],
       y="",
       x="")

figure_16_data <- fread("Graphs_data/Figure_16.csv")
figure.16 <- ggplot(data=figure_16_data,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(IM_geo.dum),colour=as.character(IM_geo.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 16",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 16",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 16",Legend_name]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"),labels =c(graph.data[graph.data$Figure_number=="Figure 16",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 16",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 16",Legend_name]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-430,-30),labels = c(graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 16",Y_Axis_Ticks_2])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 16",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 16",Figure_title],
       y="",
       x="")

figure_17_data <- fread("Graphs_data/Figure_17.csv")
figure.17 <- ggplot(data=figure_17_data,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(d_m.dum),colour=as.character(d_m.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 17",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 17",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 17",Legend_name]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"),labels =c(graph.data[graph.data$Figure_number=="Figure 17",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 17",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 17",Legend_name]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-430,-30),labels = c(graph.data[graph.data$Figure_number=="Figure 17",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 17",Y_Axis_Ticks_2])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 17",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 17",Figure_title],
       y="",
       x="")

figure_18_data <- fread("Graphs_data/Figure_18.csv")
figure.18 <- ggplot(data=figure_18_data,aes(x=1,y=-m)) +
  dais.base.theme() +
  geom_dotplot(aes(fill=as.character(workforce.dum),colour=as.character(workforce.dum)),
               binaxis='y',
               binwidth=0.5,
               stackdir='centerwhole',
               stackgroups = TRUE,
               binpositions="all",
               dotsize=8) +
  scale_fill_manual(values = c("#ffdc00","#004c9b"),labels = c(graph.data[graph.data$Figure_number=="Figure 18",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 18",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 18",Legend_name]) +
  scale_colour_manual(values = c("#ffdc00","#004c9b"),labels =c(graph.data[graph.data$Figure_number=="Figure 18",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 18",Legend_label_2]), name = graph.data[graph.data$Figure_number=="Figure 18",Legend_name]) +
  scale_x_continuous(limits = c(0.75,1.25)) +
  scale_y_continuous(breaks = c(-430,-30),labels = c(graph.data[graph.data$Figure_number=="Figure 18",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 18",Y_Axis_Ticks_2])) +
  theme(axis.text.x = element_text(color="#004c9b"),axis.ticks = element_blank(),axis.text.y = element_blank(),plot.title = element_text(color="#004c9b")) +
  coord_flip() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 18",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 18",Figure_title],
       y="",
       x="")

figure_20_data <- fread("Graphs_data/Figure_20.csv")
size = 5
figure.20 <- ggplot(data=figure_20_data,aes(x=-score,y=growth))+
  dais.base.theme() +
  geom_point(aes(size = (count_2021)^0.5*100, colour=lv.mod)) +
  scale_colour_manual(values = c("#004c9b","#eb0072","#7474c1","#ffdc00","#6bbfae"))+
  scale_x_continuous(limits = c(-199,0),breaks = c(-180,-15),labels = c(graph.data[graph.data$Figure_number=="Figure 20",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 20",Y_Axis_Ticks_2]))+
  scale_y_continuous(limits = c(-.8, 1.5), labels = percent_format(accuracy = 1))+
  geom_hline(yintercept=0, linetype="dotted", color = "black")+
  labs(title = graph.data[graph.data$Figure_number=="Figure 20",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 20",Figure_title],
       y=graph.data[graph.data$Figure_number=="Figure 20",Y_Axis],
       x=graph.data[graph.data$Figure_number=="Figure 20",X_Axis],
       color=graph.data[graph.data$Figure_number=="Figure 20",Legend_name])+
  guides(size = 'none', color = guide_legend(nrow = 2))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 20, color="#004c9b"), plot.subtitle = element_text(size = 18))+
  geom_text_repel(data=subset(figure_20_data,
                              str_detect(name,'^C\\++')),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Artificial Intelligence (AI)'),
                  aes(-score,growth,label='AI'),
                  nudge_x = 4, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Computer terminals'),
                  aes(-score,growth,label=name),
                  nudge_x = 10, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Cisco Systems WebEx'),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Google Sheets'),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              str_detect(name,'^C\\#')),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Microsoft Power BI'),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Desktop computers'),
                  aes(-score,growth,label=name),
                  nudge_x = -2, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Digital Marketing'),
                  aes(-score,growth,label=name),
                  nudge_x = -0.75, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              str_detect (name,'^Salesforce CRM')),
                  aes(-score,growth,label=name),
                  nudge_x = -2, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              str_detect (name,'Microsoft Azure')),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              str_detect (name,'Oracle Learning Management')),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              str_detect (name,'Microsoft Active Directory')),
                  aes(-score,growth,label=name),
                  nudge_x = -0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Microsoft Office'),
                  aes(-score,growth,label=name),
                  nudge_x = 0.3, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Microsoft Excel'),
                  aes(-score,growth,label=name),
                  nudge_x = 8, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Microsoft Word'),
                  aes(-score,growth,label=name),
                  nudge_x = -8, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'JavaScript'),
                  aes(-score,growth,label=name),
                  nudge_x = 0.5, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Amazon Web Services (AWS)'),
                  aes(-score,growth,label='AWS'),
                  nudge_x = 2, nudge_y = 0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'SQL (Structured query language)'),
                  aes(-score,growth,label='SQL'),
                  nudge_x = 1, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_20_data,
                              name == 'Tableau'),
                  aes(-score,growth,label=name),
                  nudge_x = 1, nudge_y = -0.06,
                  size=size, family = 'Replica-Regular')

figure_21_data <- fread("Graphs_data/Figure_21.csv")
figure.21 <- plot.line.dais(data=figure_21_data,x=Month,y=AI_pct, show.points = TRUE,
                            plot.fig.num = graph.data[graph.data$Figure_number=="Figure 21",Figure_number],
                            unit.y = "%",
                            x.axis = graph.data[graph.data$Figure_number=="Figure 21",X_Axis],
                            y.axis = graph.data[graph.data$Figure_number=="Figure 21",Y_Axis],
                            plot.title = graph.data[graph.data$Figure_number=="Figure 21",Figure_title])+
             scale_x_date(breaks = figure_21_data$Month, date_labels = "%m/%y")+
             scale_y_continuous(limits = c(0,0.02), labels = percent)+
             theme(plot.title = element_text(color="#004c9b"))

figure_22_data <- fread("Graphs_data/Figure_22.csv")
size = 5
figure.22 <- ggplot(data=figure_22_data,aes(x=-wm,y=growth))+
  dais.base.theme() +
  geom_point(aes(size = count_2021^0.4, colour=groupName)) +
  scale_colour_manual(values = c("#eb0072","#7474c1","#ff7200","#6bbfae"))+
  scale_x_continuous(limits = c(-400,0),breaks = c(-385,-15),labels = c(graph.data[graph.data$Figure_number=="Figure 22",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure 22",Y_Axis_Ticks_2]))+
  scale_y_continuous(limits = c(min(figure_22_data$growth), 1.8), labels = percent_format(accuracy = 1))+
  geom_hline(yintercept=0, linetype="dotted", color = "black")+
  labs(title = graph.data[graph.data$Figure_number=="Figure 22",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 22",Figure_title],
       y=graph.data[graph.data$Figure_number=="Figure 22",Y_Axis],
       x=graph.data[graph.data$Figure_number=="Figure 22",X_Axis],
       color=graph.data[graph.data$Figure_number=="Figure 22",Legend_name])+
  guides(size = 'none')+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        plot.title = element_text(size = 20, color="#004c9b"), plot.subtitle = element_text(size = 18))+
  geom_text_repel(data=subset(figure_22_data,
                              growth >0.75),
                  aes(-wm,growth,label=subGroupName),
                  nudge_x = 0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              str_detect (subGroupName,'^Artificial Intelligence')),
                  aes(-wm,growth,label="Artificial Intelligence (AI)"),
                  nudge_x = 0.5, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              subGroupName == "Personal qualities"),
                  aes(-wm,growth,label=subGroupName),
                  nudge_x = 0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              subGroupName == "Interpersonal"),
                  aes(-wm,growth,label="Interpersonal skills"),
                  nudge_x = 1, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              subGroupName == "Languages"),
                  aes(-wm,growth,label="Language skills"),
                  nudge_x = 0.2, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              subGroupName == "Helpdesk or call center software"),
                  aes(-wm,growth,label=subGroupName),
                  nudge_x = 0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              subGroupName == "Business intelligence and data analysis software"),
                  aes(-wm,growth,label=subGroupName),
                  nudge_x = -0.2, nudge_y = -0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              subGroupName == "Information Technology and telecommunications"),
                  aes(-wm,growth,label="Information Technology and telecommunications software"),
                  nudge_x = -0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_text_repel(data=subset(figure_22_data,
                              subGroupName == "Health and safety"),
                  aes(-wm,growth,label="Health and safety skills"),
                  nudge_x = -0.2, nudge_y = 0.08,
                  size=size, family = 'Replica-Regular')+
  geom_smooth(method = "lm", mapping = aes(weight = count_2021), 
              color = "black", show.legend = FALSE)

figure_a1_data <- fread("Graphs_data/Figure_A1.csv")
figure.a1 <- ggplot(data=figure_a1_data,aes(x=-score,y=growth))+
  dais.base.theme() +
  geom_point(aes(size = (count_2021)^0.5*100, colour=lv.mod)) +
  scale_colour_manual(values = c("#eb0072","#7474c1","#ff7200","#6bbfae","#ffdc00","#004c9b"))+
  scale_x_continuous(limits = c(-400,0),breaks = c(-385,-15),labels = c(graph.data[graph.data$Figure_number=="Figure A.1",Y_Axis_Ticks_1],graph.data[graph.data$Figure_number=="Figure A.1",Y_Axis_Ticks_2]))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  geom_hline(yintercept=0, linetype="dotted", color = "black")+
  labs(title = graph.data[graph.data$Figure_number=="Figure A.1",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure A.1",Figure_title],
       y=graph.data[graph.data$Figure_number=="Figure A.1",Y_Axis],
       x=graph.data[graph.data$Figure_number=="Figure A.1",X_Axis],
       color=graph.data[graph.data$Figure_number=="Figure A.1",Legend_name])+
  guides(size = 'none', color = guide_legend(nrow = 2))+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size=16), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18),
        plot.title = element_text(size = 20, color="#004c9b"), plot.subtitle = element_text(size = 18))+
  annotate("rect", xmin=-200, xmax=0, ymin=-0.8, ymax=1.5, alpha=0.1, fill='orange')

figure_a2_data <- fread("Graphs_data/Figure_A2.csv")
figure.a2 <- ggplot(data=figure_a2_data,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=gobm)) +
  scale_colour_manual(values = c("#004c9b","#ffdc00"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure A.2",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure A.2",Figure_title],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure A.2",Legend_name])+
  guides(size = 'none')+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  guides(size = 'none')+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure A.2", X_Axis],size = 5)+
  annotate("text", x = 432, y = 200, label = graph.data[graph.data$Figure_number=="Figure A.2", Y_Axis], angle = 90, size = 5)

figure_a3_data <- fread("Graphs_data/Figure_A3.csv")
figure.a3 <- ggplot(data=figure_a3_data,aes(x=m.x,y=m.y))+
  dais.base.theme() +
  geom_point(aes(size = (weight)^2*5, colour=ht_sm)) +
  scale_colour_manual(values = c("#004c9b","#ffdc00"))+
  scale_x_reverse()+
  scale_y_reverse()+
  labs(title = graph.data[graph.data$Figure_number=="Figure A.3",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure A.3",Figure_title],
       y="",
       x="",
       color=graph.data[graph.data$Figure_number=="Figure A.3",Legend_name])+
  guides(size = 'none')+
  theme(plot.margin = margin(10, 10, 25, 30, "pt"),
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.text = element_text(size = 16),legend.title = element_text(size = 16),
        plot.title = element_text(size = 18, color="#004c9b"), plot.subtitle = element_text(size = 16))+
  guides(size = 'none' )+
  coord_cartesian(xlim = c(400, 0), ylim = c(400, 0), clip="off")+
  annotate("segment", x=350, y=430, xend=80, yend=430,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("segment", x=425, y=350, xend=425, yend=50,
           col="black", arrow=arrow(length=unit(5, "mm")))+
  annotate("text", x = 200, y = 442, label = graph.data[graph.data$Figure_number=="Figure A.3", X_Axis], size = 5)+
  annotate("text", x = 432, y = 200, label = graph.data[graph.data$Figure_number=="Figure A.3", Y_Axis], angle = 90, size = 5)

export.dais.plot("Dais_graphs/f3.pdf",figure.3, p.height = 5, p.width = 8)
export.dais.plot("Dais_graphs/f4.pdf",figure.4, p.height = 5, p.width = 8)
export.dais.plot("Dais_graphs/f5.pdf",figure.5, p.height = 6.25, p.width = 10)
export.dais.plot("Dais_graphs/f6.pdf",figure.6, p.height = 7, p.width = 11)
export.dais.plot("Dais_graphs/f7.pdf",figure.7, p.height = 7, p.width = 11)
export.dais.plot("Dais_graphs/f8.pdf",figure.8, p.height = 7, p.width = 11)
export.dais.plot("Dais_graphs/f9.pdf",figure.9, p.height = 7, p.width = 11)
export.dais.plot("Dais_graphs/f10.pdf",figure.10, p.height = 7, p.width = 11)
export.dais.plot("Dais_graphs/f11.pdf",figure.11, p.height = 7, p.width = 12)
export.dais.plot("Dais_graphs/f12.pdf",figure.12, p.height = 9, p.width = 15)
export.dais.plot("Dais_graphs/f13.pdf",figure.13, p.height = 9, p.width = 15)
export.dais.plot("Dais_graphs/f14.pdf",figure.14, p.height = 5, p.width = 8)
export.dais.plot("Dais_graphs/f15.pdf",figure.15, p.height = 5, p.width = 8)
export.dais.plot("Dais_graphs/f16.pdf",figure.16, p.height = 5, p.width = 8)
export.dais.plot("Dais_graphs/f17.pdf",figure.17, p.height = 5, p.width = 8)
export.dais.plot("Dais_graphs/f18.pdf",figure.18, p.height = 5, p.width = 8)
export.dais.plot("Dais_graphs/f20.pdf",figure.20, p.height = 12, p.width = 16)
export.dais.plot("Dais_graphs/f21.pdf", figure.21)
export.dais.plot("Dais_graphs/f22.pdf",figure.22, p.height = 12, p.width = 16)
export.dais.plot("Dais_graphs/fa1.pdf",figure.a1, p.height = 12, p.width = 18)
export.dais.plot("Dais_graphs/fa2.pdf",figure.a2, p.height = 9, p.width = 15)
export.dais.plot("Dais_graphs/fa3.pdf",figure.a3, p.height = 9, p.width = 15)

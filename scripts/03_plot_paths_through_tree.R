#plot illustrative paths through the tree

#select a particular set of parameters
path_data_frame<-estimate_performance_df %>% 
  filter(round(ppv,1)==0.4 | round(ppv,1)==0.6,
         round(cso_one,1) %in% c(0.7,0.9), 
         cso_two==0.5) %>% 
  mutate(Starting_PPV=sprintf("%s",round(ppv*100)),
         CSO_One_Accuracy=sprintf("%s",round(cso_one*100)),
         base_rate=case_when(ppv<0.5 ~ 0.012, TRUE ~ 0.024)) %>%
  select(CSO_One_Accuracy,Starting_PPV,base_rate,ppv,residual_risk_one,residual_risk_two,sens) %>%
  mutate(row=1:length(ppv),
         confirm_sens=factor(100*sens)) %>%
  pivot_longer(cols=c("base_rate","ppv","residual_risk_one","residual_risk_two")) %>% 
  mutate(Starting_PPV=factor(Starting_PPV,levels=c("40","60"),
                             labels=c("PPV 40%","PPV 60%")),
         CSO_One_Accuracy=factor(CSO_One_Accuracy,levels=c("90","70"),
                                 labels=c("CSO One Accuracy 90%","CSO One Accuracy 70%")),
         value=100*value) %>%
  mutate(name=case_when(name=="base_rate" ~ "Base Rate",
                        name=="ppv" ~"PPV", 
                        name=="residual_risk_one"~"Residual Risk 1",
                        name=="residual_risk_two" ~ "Residual Risk 2")) 

#add some annotation
event_desc_df<-path_data_frame %>% 
  group_by(CSO_One_Accuracy,Starting_PPV,name) %>%
  summarize(mval=median(value)) %>%
  ungroup() %>%
  group_by(CSO_One_Accuracy,Starting_PPV) %>%
  mutate(xpos=0.5+1:4,ypos=0.5*(mval+lead(mval,default=0))) %>%
  mutate(val=c("Screen","Confirm\nTest1","Confirm\nTest2","Dummy")) %>%
  ungroup() %>%
  filter(val!="Dummy")

#make the plot with annotation
path_tree_plot<-path_data_frame %>%
  ggplot(aes(x=name,y=value))+
  geom_line(aes(color=confirm_sens,group=row),arrow=arrow(),lwd=2)+
  geom_point()+
  geom_hline(yintercept=3,color="red",lty="dashed",lwd=1)+
  annotate("text",x=2.0,y=5,label="3% NICE",color="red")+
  geom_hline(yintercept=7,color="black",lty="dashed",lwd=1)+
  annotate("text",x=2.0,y=9,label="7% Li-Fraumeni",color="black")+
  geom_label(aes(x=xpos,y=ypos,label=val),data=event_desc_df)+
  facet_grid(rows=vars(Starting_PPV),cols=vars(CSO_One_Accuracy))+
  theme_bw()+
  theme(
    strip.text= element_text(size=16),
    axis.text = element_text(size=12),
    legend.text = element_text(size=12),
    title=element_text(size=18),
    #panel.grid=element_blank(),
    panel.grid=element_line(linetype="dashed",color="grey50"),
    panel.grid.major.y=element_line(linetype="dotted",color="grey30"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    legend.position="bottom")+
  coord_cartesian(y=c(0,60))+
  labs(x="Level of Investigation",y="Residual Current Cancer Risk (%)",color="Confirmatory Test Sensitivity")+
  ggtitle("Illustrative Paths Through the Decision Tree")

ggsave(sprintf("figs/%s_illustrate_tree.pdf",date_code),
       plot=path_tree_plot,
       width=14,height=12)

ggsave(sprintf("figs/%s_illustrate_tree.eps",date_code),
       plot=path_tree_plot,
       width=14,height=12)

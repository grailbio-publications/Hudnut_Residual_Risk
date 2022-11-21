#suppplemental: show how PPV varies with relative rate of true positives
#https://seer.cancer.gov/explorer/application.html?site=1&data_type=1&graph_type=3&compareBy=sex&chk_sex_1=1&rate_type=2&race=1&advopt_precision=1&advopt_show_ci=on&advopt_display=2#tableWrap

seer_all_sites<-read_csv("data/20211111_seer_by_age_retrieved.csv",skip=2,n_max=798) 

names(seer_all_sites)<-c("Sex","Race_Ethnicity","RateType","Age","RatePer100K","RateSE","RateL95","RateU95")

seer_reference<-seer_all_sites %>%
  filter(Sex=="Both Sexes",
         grepl("All Races",Race_Ethnicity),
         grepl("Observed",RateType)) %>%
  type_convert()

seer_by_age<-seer_reference %>% 
  mutate(Relative_Incidence_Rate=100*RatePer100K/RatePer100K[Age=="55-59"]) %>%
  select(Age,RatePer100K,Relative_Incidence_Rate)

basic_ppv_example_df<-tibble(Relative_Pos_Rate=100*seq(0.1,1,by=0.1)/0.4) %>%
  mutate(False_Pos_Relative_Rate=150) %>%
  mutate(ppv=Relative_Pos_Rate/(Relative_Pos_Rate+False_Pos_Relative_Rate)) %>%
  mutate(PPV=round(ppv*100))  %>%
  select(Relative_Pos_Rate,False_Pos_Relative_Rate,PPV)


ppv_example_plot<-basic_ppv_example_df %>%
  ggplot(aes(x=Relative_Pos_Rate,y=PPV))+
  geom_line()+
  geom_vline(xintercept=100,lty="dashed")+
  geom_label(aes(label=PPV),size=10,
             data=basic_ppv_example_df %>% filter(PPV %in% c(25.0,40.0,50.0,60.0)))+
  #  geom_label(aes(label=Age,color="Age"),data=seer_by_age %>%
  #               filter(Age %in% c("50-54","55-59","60-64","65-69","70-74")))+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
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
  coord_cartesian(y=c(0,70))+
  labs(x="Positive Rate Relative to Baseline Case (Pct)",y="PPV (Pct)")+
  ggtitle("Positive Predictive Value (PPV) is a function of population risk")

ggsave(sprintf("figs/%s_ppv_example_plot.eps",date_code),
       ppv_example_plot,
       width=10,height=10)

#supplemental tables
write_tsv(basic_ppv_example_df,sprintf("reports/%s_basic_ppv_example.tsv",date_code))

write_tsv(seer_by_age,sprintf("reports/%s_seer_relative_incidence_by_age.tsv",date_code))

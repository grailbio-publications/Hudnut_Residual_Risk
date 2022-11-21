#interested in ppv range 30-70%
#interested in first cso range 60-90%
#interested in second cso conditional range 30-70
#interested in confirmatory test 100% sens, 100% spec
#interested in confirmatory test 90% sens, 90% spec
#interested in confirmatory test 70% sens, 90% spec


base_ppv<-tibble(ppv=seq(0.3,0.7,by=0.1))
first_cso<-tibble(cso_one=seq(0.6,0.9,by=0.1))
second_cso<-tibble(cso_two=seq(0.4,0.6,by=0.1))
confirm_test<-tibble(sens=c(1.0,0.9,0.7),spec=c(1.0,0.9,0.9))

#make a table
table_one<-base_ppv %>%
  bind_rows(first_cso) %>%
  bind_rows(second_cso) %>%
  bind_rows(confirm_test) %>%
  pivot_longer(everything()) %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarize(values=paste(value,collapse=",")) %>%
  ungroup()

#write out human readable version of the table
write_tsv(table_one,sprintf("reports/%s_table_one.tsv",date_code))

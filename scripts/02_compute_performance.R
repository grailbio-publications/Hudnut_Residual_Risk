#make the actual performance
cover_df<-base_ppv %>% 
  full_join(first_cso,by=character()) %>%
  full_join(second_cso,by=character()) %>%
  full_join(confirm_test,by=character())

#procedure PPV->first CSO-> confirmatory test
# after first confirmatory test what to do?
# conditional second CSO

estimate_performance_df<-cover_df %>% 
  mutate(tp_at_zero=ppv, #start with this many tp
         fp_at_zero=1-ppv, #start with this many fp
         tp_at_one=tp_at_zero*cso_one*sens, #confirm this many
         ghost_at_one=fp_at_zero*(1-spec), #false positive from confirmation
         fp_at_one=fp_at_zero, #fp not eliminated
         tp_after_one=tp_at_zero-tp_at_one, #tp not eliminated
         tp_after_one_with_correct_cso=tp_at_zero*cso_one*(1-sens), #tp with correct label not eliminated
         tp_after_one_with_incorrect_cso=tp_after_one-tp_after_one_with_correct_cso,
         tp_at_two=tp_after_one_with_incorrect_cso*cso_two*sens, #if first label was correct, second has to be wrong
         fp_at_two=fp_at_one,
         ghost_at_two=fp_at_one*(1-spec),
         tp_after_two=tp_after_one-tp_at_two, #no worries about label here
         fp_after_two=fp_at_two) %>%
  mutate(residual_risk_one=tp_after_one/(tp_after_one+fp_at_one),
         cso_likely_elsewhere=tp_after_one_with_incorrect_cso/tp_after_one,
         total_ghosts_one=ghost_at_one,
         residual_risk_two=tp_after_two/(tp_after_two+fp_after_two),
         total_ghosts_two=ghost_at_one+ghost_at_two)

#write the large data table covering every contingency
write_tsv(estimate_performance_df,sprintf("reports/%s_estimated_performance_residual_risk.tsv",date_code))


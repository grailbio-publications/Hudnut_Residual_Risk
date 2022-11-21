#tidy decision tree diagram
library(tidyverse)


plot_decision_tree<-function(){
  xstart<-0.1
  lrad<-0.04
  yzero<-0.3
  delta_x<-0.11
  delta_y<-0.11
  
  
  rr_node_df<-tibble(start=rep("RR",3), child=c("CSO","XCT","STOP"),dy=c(delta_y,0,-delta_y)) %>% mutate(dx=delta_x)
  ct_node_df<-tibble(start=rep("CT",2),child=c("RR","FIND"),dy=c(delta_y,-delta_y)) %>% mutate(dx=delta_x)
  test_node_df<-tibble(start="CSO",child="CT",dy=0,dx=delta_x)
  
  base_node_df<-tibble(start="CSD",child=c("RR"),x=xstart,y=yzero,layer=0)
  
  layer_one<-base_node_df %>% 
    select(start=child,x,y,layer) %>%
    left_join(rr_node_df) %>%
    mutate(x=x+dx,y=y+dy,layer=layer+1) %>%
    select(start,child,x,y,layer)
  
  rr_one<-layer_one %>%
    select(start=child,x,y,layer) %>%
    inner_join(test_node_df) %>%
    mutate(x=x+dx,y=y+dy,layer=layer+1) %>%
    select(start,child,x,y,layer)
  
  ct_one<-rr_one %>%
    select(start=child,x,y,layer) %>%
    inner_join(ct_node_df) %>%
    mutate(x=x+dx,y=y+dy,layer=layer+1) %>%
    select(start,child,x,y,layer)
  
  layer_two<-ct_one %>%
    select(start=child,x,y,layer) %>%
    inner_join(rr_node_df) %>%
    mutate(x=x+dx,y=y+dy,layer=layer+1) %>%
    select(start,child,x,y,layer)
  
  rr_two<-layer_two %>%
    select(start=child,x,y,layer) %>%
    inner_join(test_node_df) %>%
    mutate(x=x+dx,y=y+dy,layer=layer+1) %>%
    select(start,child,x,y,layer)
  
  ct_two<-rr_two %>%
    select(start=child,x,y,layer) %>%
    inner_join(ct_node_df) %>%
    mutate(x=x+dx,y=y+dy,layer=layer+1) %>%
    select(start,child,x,y,layer)
  
  layer_three<-ct_two %>%
    select(start=child,x,y,layer) %>%
    inner_join(rr_node_df) %>%
    mutate(x=x+dx,y=y+dy,layer=layer+1) %>%
    select(start,child,x,y,layer)
  
  diagram_map_box<-bind_rows(base_node_df,layer_one,rr_one,ct_one,
                             layer_two,rr_two,ct_two,
                             layer_three) %>%
    mutate(box_size=lrad,status=child) %>%
    left_join(tribble(~status,~box,~box_col,~text_status,
                      "RR","hexa","white","RR",
                      "CT","rect","white","Spec\nTest",
                      "STOP","multi","red","STOP",
                      "CSO","ellipse","white","CSO",
                      "XCT","rect","grey","NSpec\nTest",
                      "FIND","rect","green","Find")) %>%
    filter(!(layer==max(layer) & status=="CSO")) %>%
    arrange(layer) %>%
    mutate(test_history=cumsum(status=="CT")) %>%
    mutate(text_status=case_when(status=="RR" & test_history==0 ~ "PPV",
                                 status=="RR" & test_history>0 ~ sprintf("Res%s\nRisk",test_history),
                                 status=="CSO" ~ paste(status,test_history+1,sep=""),
                                 status=="CT" ~ sprintf("Spec%s\nTest",test_history),
                                 TRUE ~ text_status))
  
  diagram_map_arrow<-diagram_map_box %>%
    select(start,child,x,y,layer) %>%
    left_join(diagram_map_box %>% 
                mutate(layer=layer-1) %>%
                select(child=start,final=child,x_end=x,y_end=y,layer)) %>%
    filter(!is.na(x_end))
  
  
  
  openplotmat(main = "Example: three step decision tree: two CSO estimates and final decison")
  
  
  for (ii in 1:length(diagram_map_arrow$x)){
    
    straightarrow(from=c(diagram_map_arrow$x[ii],diagram_map_arrow$y[ii]),
                  to=c(diagram_map_arrow$x_end[ii],diagram_map_arrow$y_end[ii]),
                  lty=1)
  }
  
  
  for (ii in 1:length(diagram_map_box$x)){
    
    langle=0
    if (diagram_map_box$box[ii]=="multi")
      langle=360/16
    shadowbox(box.type=diagram_map_box$box[ii],
              mid=c(diagram_map_box$x[ii],diagram_map_box$y[ii]),
              radx=diagram_map_box$box_size[ii],
              rady=diagram_map_box$box_size[ii],
              shadow.size=0.0,
              box.col=diagram_map_box$box_col[ii],
              nr=8,angle=langle,
              lwd=2)
  }
  
  for (ii in 1:length(diagram_map_box$x)){
    textplain(
      mid=c(diagram_map_box$x[ii],diagram_map_box$y[ii]),
      height=diagram_map_box$box_size[ii],
      lab=diagram_map_box$text_status[ii],
      cex=1.5)
  }
  
  box()
  
  #cases TP at CSO1 - will be confirmed by CT1 (depending on sensitivity) or XCT
  #      TP at CSO2 - will be confirmed by CT2 (depending on sensitivity) or XCT
  #      TP other location - will be confirmed by XCT
  #      FP (may be falsely confirmed depending on specificity, but has a CSO1/CSO2)
  rect(0.04,0.73,0.65,0.98)
  text(0.05,0.95, "Cases in starting PPV pool:",adj=0,cex=1.5)
  text(0.05,0.9, "1. TP at CSO1 - Specific Test 1 finds with sensitivity",adj=0,cex=1.5)
  text(0.05,0.85, "2. TP at CSO2 - Specific Test 2 finds with sensitivity",adj=0,cex=1.5)
  text(0.05,0.8, "3. TP at other - only found by Non Specific Test",adj=0,cex=1.5)
  text(0.05,0.75, "4. FP: never confirmed",adj=0,cex=1.5)
  
  #text(0.3,0.8,expression(RRnew == RR %*% (1 - sensCT) / (1 - RR %*% sensCT)))
}

pdf(sprintf("figs/%s_decision_tree.pdf",date_code),
    width=12,height=12)

plot_decision_tree()
dev.off()

setEPS()
postscript(sprintf("figs/%s_decision_tree.eps",date_code),
           width=12,height=12)
plot_decision_tree()
dev.off()






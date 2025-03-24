
christmasTree = function(){
  set.seed(0)
  
  require(tidyverse, quietly = T)
  require(ggplot2)
  
  cone = data.frame(x = 1:9,
                    y = c(1:5,4:1)) %>% 
    na.omit() %>% 
    arrange(x)
  
  cone %>% 
    ggplot(aes(x=x, y=y)) +
    geom_polygon(fill="#315c18")
  
  fancy = cone %>% 
    mutate(xoff = ifelse(x<5, x+.4, ifelse(x>5, x-.4, NA))) %>% 
    gather(del, x, contains("x")) %>% 
    mutate(y = ifelse(del=="xoff", y-.1,y)) %>% 
    filter(y>=1) %>% 
    na.omit() %>% 
    select(-del) %>% 
    arrange(y)
  
  cone %>% 
    ggplot(aes(x=x, y=y)) +
    geom_polygon(fill="#315c18") +
    geom_polygon(data=fancy, fill = "#668c6f")
  
  # Define bauble colours
  bauble_colours = c(rep("gold",10),rep("#00ff66",7), rep("#fe29fe",7),
                     rep("white",8),rep("red",10),rep("#8d3414",7))
  
  baubles = cone %>% 
    
    # Group by y, nest and make up some random values for x.
    group_by(y) %>% 
    nest() %>% 
    mutate(data =  map(data, ~data.frame(x=seq(min(.$x), max(.$x), by=.1)))) %>% 
    unnest() %>% 
    
    # Group by x, nest and make up some random values for y.
    group_by(x) %>% 
    nest() %>% 
    mutate(data =  map(data, ~data.frame(y=seq(min(.$y), max(.$y), by=.1)))) %>% 
    unnest() %>% 
    ungroup() %>% 
    
    # Give baubles random shapes, sizes and two different colours.
    mutate(col1 = sample(bauble_colours, nrow(.), replace = T),
           col2 = sample(bauble_colours, nrow(.), replace = T),
           shp = sample(1:7, nrow(.), replace = T),
           sz = sample(seq(4,24,by=1), nrow(.), replace = T),
           time = sample(seq(.5,1,by=.01), nrow(.), replace = T)
    ) %>%
    rownames_to_column() %>% 
    
    # Grab only x baubles
    sample_n(120) %>% 
    
    # Gather the colours into a single column
    gather(dd, cols, contains("col")) %>% 
    mutate(alph = ifelse(dd == "col1", .8, 1))
  ## Warning: `cols` is now required.
  ## Please use `cols = c(data)`
  
  baubles = data.frame(baubles)
  baubles$sz = baubles$sz*3
  
  N.sf = 600
  snowflakes = data.frame(x=runif(N.sf,-0.5,10.5),y=runif(N.sf,0.5,6.5),size=rgamma(N.sf,.5)*10)
  grad1 = data.frame(x=rep(seq(-1,11,length.out=100),times=100),y=rep(seq(1,6,length.out=100),each=100),
                     z=NA)
  grad1$z = (grad1$x+4)*(grad1$y)
  
  # DRAW PLOT
  ts = 19
  cone %>% 
    ggplot(aes(x=x, y=y)) +
    coord_cartesian(xlim=c(0,10),ylim=c(1,5.2))+
    geom_tile(data=grad1,aes(x=x,y=y,fill=z))+
    scale_fill_gradient(low="#000011",high="#4444DD")+
    #geom_rect(aes(xmin=-2,ymin=-2,xmax=12,ymax=4.15),fill="#110044",alpha=.05)+
    #geom_rect(aes(xmin=-2,ymin=-2,xmax=12,ymax=2.65),fill="#110033",alpha=.05)+
    geom_rect(aes(xmin=-2,ymin=-2,xmax=12,ymax=1.25),fill="#CFCFFF")+
    geom_point(data=snowflakes,aes(x=x,y=y,size=size),color="white",shape="R")+
    geom_polygon(fill="#213c18") +
    geom_polygon(data=fancy, fill = "#668c6f")+
    geom_point(data = baubles, aes(colour=I(cols), 
                                   shape = factor(shp),size=sz,stroke=1.2), show.legend = T) + 
    scale_shape_manual(values = c(15:20,8))+
    scale_y_continuous(breaks = seq(0,10,.5))+
    theme(axis.text=element_text(size=ts),axis.title=element_text(size=ts),
          panel.background = element_rect(fill="#221155"),
          panel.grid = element_blank(),plot.title=element_text(size=ts,hjust=0.5),
          #legend.title=element_text(size=ts*.6),
          legend.position="none")+
    geom_point(aes(x=5,y=5),size=ts*1,color="gold",shape=8,stroke=3.5)+
    xlab("Holidays")+ylab("Happy")#+ggtitle("Buone Feste!")+
}



happyBirthday = function(){
  set.seed(0)
  # devtools::install_github("dill/emoGG")
  require(emoGG)
  require(ggplot2)
  emoji_search("birthday")
  
  heart = data.frame(t = seq(0, 2*pi, by = 0.1))
  x =  function(t) 16 * sin(t)^3
  y = function(t) 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)
  heart$y = y(heart$t)
  heart$x = x(heart$t)
  
  heart2 = rbind(heart,heart)
  heart2$y = heart2$y + runif(nrow(heart2),-2,2)
  heart2$x = heart2$x + runif(nrow(heart2),-2,2)
  heart2$size = runif(nrow(heart2),.05,.08)
  heart3 = rbind(heart,heart,heart,heart)
  heart3$y = heart3$y + runif(nrow(heart3),-3,3)
  heart3$x = heart3$x + runif(nrow(heart3),-3,3)
  heart3$size = runif(nrow(heart2),.02,.04)
  Heart = rbind(heart2,heart3)
  
  ncirc = 200
  circles = data.frame(
    y = runif(ncirc,-25,25),
    x = runif(ncirc,-25,25),
    size = rgamma(ncirc,2,.1),
    alpha = runif(ncirc,.1,.3),
    shape = 16,
    id = sample(1:20,ncirc,replace=T)
  )
  ncirc = 100
  circles2 = data.frame(
    y = runif(ncirc,-25,25),
    x = runif(ncirc,-25,25),
    size = rgamma(ncirc,.8,.1),
    alpha = runif(ncirc,.2,.4),
    shape = 1,
    id = sample(1:20,ncirc,replace=T)
  )
  circles = rbind(circles,circles2)
  circles$id = as.factor(circles$id)
  
  em = c("1f382","1f370","2728","1f388","1f38a","1f389","1f388","1f38a","1f389")
  
  textsize = 45
  age = 33; breaksn = 11 + 1
  ggplot(heart,aes(x=x,y=y,group=1))+theme_bw()+
    geom_point(data=circles,aes(color=id),stroke=2,alpha=circles$alpha,size=circles$size,shape=circles$shape)+
    geom_emoji(data=Heart,emoji=sample(em,nrow(Heart),replace=T),size=Heart$size)+
    geom_emoji(emoji=sample(em,nrow(heart),replace=T))+
    geom_emoji(aes(x=0,y=-1.5),emoji="1f382",size=.15)+
    coord_cartesian(ylim=c(-20,15),xlim=c(-20,20))+
    scale_y_continuous(breaks=seq(-20,16,length.out=breaksn),labels=seq(0,age,age/(breaksn-1)))+
    scale_x_continuous(breaks=seq(-20,20,length.out=breaksn),labels=seq(0,age,age/(breaksn-1)))+
    theme(text=element_text(size=textsize),axis.text=element_text(size=textsize/2.2),legend.position="none")+
    ylab("happy")+xlab("birthday")
}

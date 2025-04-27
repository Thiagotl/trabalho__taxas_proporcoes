#---------------------------------------
resid.analisys<-function(resid=NULL,data=NULL,fitted=NULL,plot.type=c("plot","FAC","FACP","densidade",
"envelope","residxindex","resxfit","realxfit"),lag.max=NULL)
{
  #---------------------------------------
  #resid o teu resíduo do modelo
  if(!is.null(resid)){resid<-ts(as.vector(resid))}
  if (is.null(lag.max)){lag.max<-length(resid)/4}
  #---------------------------------------
  #Grafico dos Dados
  if(plot.type == "plot"){
    lastplot<-forecast::autoplot(resid)+ggplot2::ggtitle(" ") + 
      ggplot2::xlab("Time")+ggplot2::ylab("Values")+ggplot2::theme_minimal(base_size=12)
  }
  #---------------------------------------
  #FAC e FACP
  if(plot.type == "FAC"){
    lastplot<-forecast::ggAcf(as.numeric(resid),lag.max=lag.max,type = c("correlation"))+
      ggplot2::labs(y = "Sample ACF",title="")+ggplot2::theme_minimal()
  }
  if(plot.type == "FACP"){
    lastplot<-forecast::ggAcf(resid, lag.max=lag.max,type = c("partial"))+
      ggplot2::labs(y = "Sample PACF",title="")+ggplot2::theme_minimal(base_size=12)
  }
  #---------------------------------------
  #Envelope
  if(plot.type == "envelope"){
    dados<-as.vector(resid)
    lastplot<-ggpubr::ggqqplot(resid)+ggplot2::labs(y = "Sample Quantiles",x="Theoretical Quantiles",title="")+
      ggplot2::theme_minimal(base_size=12)
  }
  #-------------------------------------
  #Residuo Versus indice
  if(plot.type == "residuo"){
    LI1<-ts(rep(-2,length(resid)))
    LS1<-ts(rep(2,length(resid)))
    LI2<-ts(rep(-3,length(resid)))
    LS2<-ts(rep(3,length(resid)))
    index<-1:length(resid)
    df<-data.frame(index,resid)
    lastplot<-ggplot2::ggplot(df,ggplot2::aes(x=index,y=resid))+ggplot2::geom_point()+
      ggplot2::autolayer(LI1,colour = FALSE,linetype=3)+
      ggplot2::autolayer(LI2,colour = FALSE,linetype=2)+
      ggplot2::autolayer(LS1,colour = FALSE,linetype=3)+
      ggplot2::autolayer(LS2,colour = FALSE,linetype=2)+
      ggplot2::xlab("Index")+ggplot2::ylab("Residuals")+ggplot2::theme_minimal(base_size=12)+
      ggplot2::guides(fill=FALSE, color=FALSE)
  }
  #-------------------------------------
  #Densidade
  if(plot.type == "densidade"){
    df<-data.frame(resid)
    lastplot<-ggplot2::ggplot(df,ggplot2::aes(x=resid)) +
      ggplot2::geom_density(alpha=0.4,ggplot2::aes(color="black"))+
      ggplot2::stat_function(fun =dnorm, 
                    args=list(mean =0,
                                sd = 1), 
                    ggplot2::aes(color="blue"))+
      ggplot2::geom_hline(yintercept=0)+
      ggplot2::xlab(" ")+ggplot2::ylab("Density")+
      ggplot2::scale_color_identity(name="Legenda",
                           breaks=c("black","blue"),
                           labels=c("Sample","Theoretical"),
                           guide="legend")+
      ggplot2::theme_minimal(base_size=12)
  }
  #-------------------------------------
  #Residuo Versus Ajustado
  if(plot.type == "resxfit"){
    df<-data.frame(resid,fitted)
    lastplot<-ggplot2::ggplot(df,ggplot2::aes(x=resid,y=fitted))+
      ggplot2::geom_point()+
      ggplot2::xlab("Real Data")+ggplot2::ylab("Fitted")+
      ggplot2::theme_minimal(base_size=12)
  }
  #-------------------------------------
  #Data Versus Ajustado
  if(plot.type == "realxfit"){
    df<-data.frame(data,fitted)
    lastplot<-ggplot2::ggplot(df,ggplot2::aes(x=data,y=fitted))+
      ggplot2::geom_point()+
      ggplot2::xlab("Real Data")+ggplot2::ylab("Fitted")+
      ggplot2::theme_minimal(base_size=12)
  }
  #-------------------------------------
  print(lastplot)
  #ggplot2::ggsave("names.pdf",plot=last_plot,width=8,height=10,units="cm")
}
#Comandos----
# resid.analisys(resid=out$residuals,plot.type=c("plot"))
# resid.analisys(resid=out$residuals,plot.type=c("FAC"),lag.max=30)
# resid.analisys(resid=out$residuals,plot.type=c("FACP"),lag.max=30)
# resid.analisys(resid=out$residuals,plot.type=c("densidade"))
# resid.analisys(resid=out$residuals,plot.type=c("envelope"))
# resid.analisys(resid=out$residuals,plot.type=c("residuo"))
# resid.analisys(resid=out$residuals,fitted=out$fitted,plot.type=c("resxfit"))
# resid.analisys(data=y,fitted=out$fitted,plot.type=c("realxfit"))

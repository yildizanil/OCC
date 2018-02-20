library(shiny)
library(plotly)
library(rootSolve)# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output,session) {
  output$p0 <- renderUI({
    sliderInput("p0", "Initial confining pressure [kPa]", min=1, max=input$cp, value=100)
  })
  
  output$inputgraph <- renderPlotly({
    
    pc <- input$cp
    
    p_ini_yield1 <- seq(0,pc,1)
    q_ini_yield1 <- input$M*p_ini_yield1
    qy1 <- input$M*p_ini_yield1*log(pc/p_ini_yield1)
    
    vl <- input$l-input$k+input$N
    v <- vl - input$l*log(pc)
    vo <- v + input$k*log(pc/input$p0)
    
    
    plot_ly(x=p_ini_yield1,y=qy1,type="scatter",mode="none",name="Yield surface",fill="tozeroy",fillcolor="rgba(0,0,0,0.2)") %>%
      add_trace(x=p_ini_yield1,y=q_ini_yield1,mode="lines",line=list(color="red"),fill=NULL,name="CSL") %>%
      layout(xaxis=list(title="p, p' [kPa]"),yaxis=list(title="q [kPa]"),autosize=T,
             annotations=list(x=c(input$p0,pc,pc+5),y=c(0,0,pc*input$M),text=c("Confining pressure","Consolidation pressure","M"),
                              showarrow=F,xanchor="centre",yanchor="top"),
             shapes=list(list(type="circle",xref="x",x0=input$p0-(input$p0/20),x1=input$p0+(input$p0/20),yref="circle",y0=-(input$p0/20),y1=(input$p0/20),fillcolor="black",opacity=0.6),
                         list(type="circle",xref="x",x0=pc-(input$p0/20),x1=pc+(input$p0/20),yref="circle",y0=-(input$p0/20),y1=(input$p0/20),fillcolor="black",opacity=0.6),
                         list(type="line",xref="x",yref="y",x0=pc,x1=pc,y0=pc*input$M-(input$p0/5),y1=pc*input$M,line=list(color="red")),
                         list(type="line",xref="x",yref="y",x0=(pc*input$M-(input$p0/5))/input$M,x1=pc,y0=pc*input$M-(input$p0/5),y1=pc*input$M-(input$p0/5),line=list(color="red"))))
    
  })
  
  output$qpPlot <- renderPlotly({
    pc <- input$cp
    
    p_ini_yield1 <- seq(0,pc,1)
    qy1 <- input$M*p_ini_yield1*log(pc/p_ini_yield1)
    
    eq1 <- function(x){
      (((3*x)/(input$M*(input$p0+x)))-log(input$cp/(input$p0+x)))
    }
    pyp <- input$p0+(uniroot(eq1,c(-1,10000),extendInt="yes")$root)
    qyp <- 3*(uniroot(eq1,c(-1,10000),extendInt="yes")$root)
    
    eq2 <- function(y){
      3*y-(input$M*(input$p0+y))
    }
    ptsp <- input$p0+(uniroot(eq2,c(-1,10000),extendInt="yes")$root)
    qtsp <- 3*(uniroot(eq2,c(-1,10000),extendInt="yes")$root)
    
    vl <- input$l-input$k+input$N
    v <- vl - input$l*log(pc)
    vo <- v + input$k*log(pc/input$p0)
    pfu <- exp((input$N-vo)/input$l)
    qfu <- pfu*input$M
    qey <- input$M*input$p0*log(pc/input$p0)
    
    if(ptsp > pc)
    {
      p_ini_yield <- seq(0,ptsp,1)
      q_ini_yield <- input$M*p_ini_yield
      
    } else {
      p_ini_yield <- seq(0,pc,1)
      q_ini_yield <- input$M*p_ini_yield
    }
    
    pc_fail <- ptsp*exp(qtsp/(input$M*ptsp))
    p_fail <- seq(0,pc_fail,1)
    q_fail<- input$M*p_fail*log(pc_fail/p_fail)
    
    if(input$testtype=="Drained")
    {
      plot_ly(x=p_ini_yield1,y=qy1,type="scatter",mode="none",name="Yield surface",fill="tozeroy",fillcolor="rgba(0,0,0,0.2)") %>%
        add_trace(x=p_ini_yield,y=q_ini_yield,mode="lines",name="CSL",line=list(color="rgb(256,0,0)"),fill="") %>%
        add_trace(x=c(input$p0,pyp),y=c(0,qyp),mode="lines",fill="none",name="OAB: Total \nstress path",line=list(color="rgb(0,1,0)")) %>%
        add_trace(x=c(pyp,ptsp),y=c(qyp,qtsp),mode="lines",fill="none",name="",showlegend=F,line=list(color="rgb(0,1,0)")) %>%
        layout(xaxis=list(title="p, p' [kPa]"),yaxis=list(title="q [kPa]"),autosize=T,
               annotations=list(x=c(input$p0,pyp,ptsp),y=c(0,qyp,qtsp),text=c("O","A","B"),showarrow=F,xanchor="left"))
    } else{
      plot_ly(x=p_ini_yield1,y=qy1,type="scatter",mode="none",name="Yield surface",fill="tozeroy",fillcolor="rgba(0,0,0,0.2)") %>%
        add_trace(x=p_ini_yield,y=q_ini_yield,mode="lines",name="CSL",line=list(color="rgb(256,0,0)"),fill="") %>%
        add_trace(x=c(input$p0,pyp),y=c(0,qyp),mode="lines",fill="none",name="OAB: Total \nstress path",line=list(color="rgb(0,1,0)")) %>%
        add_trace(x=c(pyp,ptsp),y=c(qyp,qtsp),mode="lines",fill="none",name="",showlegend=F,line=list(color="rgb(0,1,0)")) %>%
        add_trace(x=c(input$p0,input$p0),y=c(0,qey),mode="lines",fill="none",name="OCD: Effective \nstress path",line=list(color="cyan")) %>%
        add_trace(x=c(input$p0,pfu),y=c(qey,qfu),mode="lines",fill="none",showlegend=F,name="Effective stress path",line=list(color="cyan")) %>%
        layout(xaxis=list(title="p, p' [kPa]"),yaxis=list(title="q [kPa]"),autosize=T,
               annotations=list(x=c(input$p0,pyp,ptsp,input$p0,pfu),y=c(0,qyp,qtsp,qey,qfu),text=c("O","A","B","C","D"),showarrow=F,xanchor="left"))
    } 
  })
  
  output$vlnpPlot <- renderPlotly({
    
    pc <- input$cp
    p_ini <- seq(0,pc,1)
    vl <- input$l-input$k+input$N
    v_ini_ncl <- vl - input$l*log(p_ini)
    v_ini_csl <- input$N-input$l*log(p_ini)
    
    v <- vl - input$l*log(pc)
    vo <- v + input$k*log(pc/input$p0)
    
    
    eq1 <- function(x){
      (((3*x)/(input$M*(input$p0+x)))-log(input$cp/(input$p0+x)))
    }
    eq2 <- function(y){
      3*y-(input$M*(input$p0+y))
    }
    pyp <- input$p0+(uniroot(eq1,c(0,input$cp))$root)
    qyp <- 3*(uniroot(eq1,c(0,input$cp))$root)
    ptsp <- input$p0+(uniroot(eq2,c(0,input$cp))$root)
    qtsp <- 3*(uniroot(eq2,c(0,input$cp))$root)
    pfu <- exp((input$N-vo)/input$l)
    
    va <- vo - input$k*(log(pyp/input$p0))
    vb <- input$N - input$l*log(ptsp)
    
    if (input$testtype=="Drained")
    {
      plot_ly(x=log(p_ini),y=v_ini_ncl,type="scatter",mode="lines",name="Iso NCL",line=list(color="blue")) %>%
        add_trace(y=v_ini_csl,mode="lines",name="CSL",line=list(color="red")) %>%
        add_trace(x=c(log(pc),log(input$p0)),y=c(v,vo),mode="lines",line=list(color="green"),name="URL") %>%
        add_trace(x=c(log(input$p0),log(pyp)),y=c(vo,va),mode="lines",line=list(color="black"),name="OAB: Total \nstress path") %>%
        add_trace(x=c(log(pyp),log(ptsp)),y=c(va,vb),mode="lines",line=list(color="black"),name="Total stress path",showlegend=F) %>%
        layout(xaxis=list(title="ln p' [kPa]"),yaxis=list(title="v [-]"),
               annotations=list(x=c(log(input$p0),log(pyp),log(ptsp)),y=c(vo,va,vb),text=c("O","A","B"),showarrow=F,xanchor="right"))
    } else{
      plot_ly(x=log(p_ini),y=v_ini_ncl,type="scatter",mode="lines",name="Iso NCL",line=list(color="blue")) %>%
        add_trace(y=v_ini_csl,mode="lines",name="CSL",line=list(color="red")) %>%
        add_trace(x=c(log(pc),log(input$p0)),y=c(v,vo),mode="lines",line=list(color="green"),name="URL") %>%
        add_trace(x=c(log(input$p0),log(pfu)),y=c(vo,vo),mode="lines",line=list(color="cyan"),name="OCD: Effective \nstress path") %>%
        add_trace(x=c(log(input$p0),log(pyp)),y=c(vo,va),mode="lines",line=list(color="black"),name="OAB: Total \nstress path") %>%
        add_trace(x=c(log(pyp),log(ptsp)),y=c(va,vb),mode="lines",line=list(color="black"),name="Total stress path",showlegend=F) %>%
        layout(xaxis=list(title="ln p' [kPa]"),yaxis=list(title="v [-]"),
               annotations=list(x=c(log(input$p0),log(pyp),log(ptsp),log(input$p0),log(pfu)),y=c(vo,va,vb,va,vo),text=c("O","A","B","C","D"),showarrow=F,xanchor="right"))
    }
    
  })
  output$ThreeDPlot <- renderPlotly({
    
    pc <- input$cp
    
    p_csl <- seq(1,pc,1)
    q_csl <- input$M*p_csl
    v_csl <- input$N-input$l*log(p_csl)
    
    vl <- input$l-input$k+input$N
    v <- vl - input$l*log(pc)
    vk <- v+input$k*log(pc)
    
    p_yield <- seq(1,pc,1)
    q_yield <- input$M*p_yield*log(pc/p_yield)
    v_yield <- vk-input$k*log(p_yield)
    
    p_ncl <- seq(1,pc,1)
    q_ncl <- rep(0,times=length(p_ncl))
    v_ncl <- vl-input$l*log(p_ncl)
    
    eq1 <- function(x){
      (((3*x)/(input$M*(input$p0+x)))-log(input$cp/(input$p0+x)))
    }
    pyp <- input$p0+(uniroot(eq1,c(-1,10000),extendInt="yes")$root)
    qyp <- 3*(uniroot(eq1,c(-1,10000),extendInt="yes")$root)
    
    eq2 <- function(y){
      3*y-(input$M*(input$p0+y))
    }
    ptsp <- input$p0+(uniroot(eq2,c(-1,10000),extendInt="yes")$root)
    qtsp <- 3*(uniroot(eq2,c(-1,10000),extendInt="yes")$root)
    
    vo <- v + input$k*log(pc/input$p0)
    va <- vo - input$k*(log(pyp/input$p0))
    vb <- input$N - input$l*log(ptsp)
    
    pfu <- exp((input$N-vo)/input$l)
    qfu <- pfu*input$M
    qey <- input$M*input$p0*log(pc/input$p0)
    
    if (input$testtype=="Drained")
    {
      plot_ly(x=p_csl,y=v_csl,z=q_csl,type="scatter3d",mode="lines",line=list(width=6,color="red"),name="CSL") %>%
        add_trace(x=p_yield,y=v_yield,z=q_yield,type="scatter3d",mode="lines",line=list(width=6,color="gray"),name="Yield surface") %>%    
        add_trace(x=p_ncl,y=v_ncl,z=q_ncl,type="scatter3d",mode="lines",line=list(width=6,color="blue"),name="Iso NCL") %>%
        add_trace(x=c(input$p0,pyp),y=c(vo,va),z=c(0,qyp),line=list(color="black")) %>%
        add_trace(x=c(pyp,ptsp),y=c(va,vb),z=c(qyp,qtsp),line=list(color="black")) %>%
        layout(autosize=F,width=1000,height=1000,scene=list(xaxis=list(title="p' [kPa]"),yaxis=list(title="v [-]"),zaxis=list(title="q [kPa]")))
    } else {
      plot_ly(x=p_csl,y=v_csl,z=q_csl,type="scatter3d",mode="lines",line=list(width=6,color="red"),name="CSL") %>%
        add_trace(x=p_yield,y=v_yield,z=q_yield,type="scatter3d",mode="lines",line=list(width=6,color="gray"),name="Yield surface") %>%    
        add_trace(x=p_ncl,y=v_ncl,z=q_ncl,type="scatter3d",mode="lines",line=list(width=6,color="blue"),name="Iso NCL") %>%
        add_trace(x=c(input$p0,pyp),y=c(vo,va),z=c(0,qyp),line=list(color="black")) %>%
        add_trace(x=c(pyp,ptsp),y=c(va,vb),z=c(qyp,qtsp),line=list(color="black")) %>%
        add_trace(x=c(input$p0,input$p0),y=c(vo,vo),z=c(0,qey),line=list(color="cyan")) %>%
        add_trace(x=c(input$p0,pfu),y=c(vo,vo),z=c(qey,qfu),line=list(color="cyan"),name="Effective \nstress path") %>%
        layout(autosize=F,width=1000,height=1000,scene=list(xaxis=list(title="p' [kPa]"),yaxis=list(title="v [-]"),zaxis=list(title="q [kPa]")))
    }
  })
  
})
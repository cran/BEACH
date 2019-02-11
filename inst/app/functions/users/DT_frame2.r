#Author: Danni Yu (danni.yu@gmail.com)
#define global variables
if(TRUE){
  fL<<-'https://www.fda.gov/drugs/informationondrugs/approveddrugs/ucm279174.htm'
  FDA.Link <<- fL
}


#Begin 1. ---------------------------------------------------------------------#
if(TRUE){
  #Optimal basket design phase 1a/b study
  #Clinical Benifit and Biological Response are {0, 1} for {no or yes}. 
  #Output@: ranked table {dose, Disease, baselineBiomarker, expectedUtility}
  #Output@: tree plot
  DT_frame <- function(
    blBMK=c('B0','B1'),          #Baseline Biomarkers, required input
    tumorType=c('T0','T1','T2'), #Types of tumors, required input
    dose=c(20, 100),             #numeric values of dose levels, required input
    #parameters
    prior_ti=c(0.1, 0.2,  0.2, 0.3, 0.05, 0.15),#PrioInfo Tumor Incidence: 
                                 #length is length(blBMK)*length(tumorType)
                                 #values obtained from prior knowledge
    prior_prop=c(0.1, 0.9),      #proportion of patients in the subgroup of dose 
                                 #and bmk length is either length(dose) or
                                 #length(dose)*length(blBMK)*length(tumorType)
                                 #values obtained from prior knowledge
    prob_stop0 = c(0.75, 0.05),  #Prob(not stop|dose) matching to the levels in 
                                 #dose length is length(dose), the 
                                 #proportions obtained from early phase trials
    prob_BR1   = c(0.1, 0.75),   #Prob(BioResp=1|stop=0, dose, tumor, bmk), 
                                 #length is either length(dose) or 
                                 #length(dose)*length(blBMK)*length(tumorType)
                                 #values obtained from early phase trials  
    prob_CB1_BR = c(0.8, 0.1),   #Prob(ClinBenefit=1|BioResp=1) and 
                                 #Prob(CB=1|BR=0). The lengh is either 2, or
                                 #2*length(dose)*length(blBMK)*length(tumorType)
    showTree   = TRUE,           #if FASLE only return the tree table
    showProb   = TRUE,           #if TRUE show the probablities on the tree and 
                                 #return the probability table
    showBar    = TRUE,           #show the barplot of expected U(dose|{T,B})
    #other args for plotting
    th.arrow        = 0.8,       #horizontal space between an arrow and target
    th.utDB    = 1,              #vertical space between dose sign and utility
    topRatio   = 0.2,            #the top ratio of joint p-values (or utilities) 
                                 #that need to be colored
    topCol     = 'red',          #the color for the top joint p-values  
    payoff     = c(100, -100)    #payoff value for CB=1 and CB=0
  ){
    #an internal function trunc values to 0, 1
    trunc01 <- function(val){
      val[val>1]<-1
      val[val<-0]<-0
      return(val)
    }
    
    #cleanup the payoff values
    if(is.character(payoff)){
      payoff <- as.numeric(strsplit(payoff, split=',', fixed=TRUE)[[1]])
      if(length(payoff)==1){
        payoff<-c(max(0, payoff), min(0, payoff))
      }else{payoff <- payoff[1:2]}
    }
    
    #cleanup the top ratio
    if(is.null(topRatio)) topRatio <- 0.2
    topRatio <- trunc01( as.numeric(topRatio) )
  
    #cleanup the input
    if(length(blBMK)==1){
      blBMK<-strsplit(as.character(blBMK), split=',', fixed=T)[[1]]
    }
    if(length(tumorType)==1){
      tumorType<-strsplit(as.character(tumorType), split=',', fixed=T)[[1]]
    }
    if(length(dose)==1){
      dose<-strsplit(as.character(dose), split=',', fixed=T)[[1]]
    }
    if(length(prior_ti)==1){
      prior_ti<-as.numeric(strsplit(as.character(prior_ti), split=',', fixed=T)[[1]])
    }
    if(length(prior_prop)==1){
      prior_prop<-as.numeric(strsplit(as.character(prior_prop), split=',', fixed=T)[[1]])
    }
    if(length(prob_stop0)==1){
      prob_stop0<-as.numeric(strsplit(as.character(prob_stop0), split=',', fixed=T)[[1]])
    }
    if(length(prob_BR1)==1){
      prob_BR1<-as.numeric(strsplit(as.character(prob_BR1), split=',', fixed=T)[[1]])
    }
    if(length(prob_CB1_BR)==1){
      prob_CB1_BR<-as.numeric(strsplit(as.character(prob_CB1_BR), split=',', fixed=T)[[1]])
    }
    
    #check wether the probablities are matching to the actions
    if(showProb){
      con1 <- length(prior_ti) == length(blBMK)*length(tumorType)
      con2 <- length(prior_prop) == length(dose) | 
        length(prior_prop)==length(dose)*length(blBMK)*length(tumorType)
      con3 <- length(prob_stop0)==length(dose) | 
        length(prob_stop0)==length(dose)*length(blBMK)*length(tumorType)
      con4 <- length(prob_BR1) == length(dose) | 
        length(prob_BR1)==length(dose)*length(blBMK)*length(tumorType)
      con5 <- (length(prob_CB1_BR)==2)| (
        length(prob_CB1_BR)==2*length(dose)*length(blBMK)*length(tumorType)
      )
      if(!all(c(con1, con2, con3, con4, con5))){showProb<-FALSE}
    }
    
    #construct the output matrix
    if(TRUE){
      numL.ClinBenif<- 2
      numL.BioResp  <- 2
      numL.stop      <- 2
      numL.dose     <- length(dose)
      numL.tumorType<- length(tumorType)
      numL.blBMK    <- length(blBMK)
      
      ClinBenif <- rep(c('yes', 'no'), 
                       times=numL.BioResp*numL.stop*numL.dose*numL.tumorType*numL.blBMK)  
      BioResp   <- rep(rep(c('yes', 'no'), each=numL.ClinBenif),
                       times=numL.stop*numL.dose*numL.tumorType*numL.blBMK)
      stop0       <- rep(rep(c('yes', 'no'), each=numL.ClinBenif*numL.BioResp),
                       times=numL.dose*numL.tumorType*numL.blBMK)
      dose.in   <- rep(rep(dose, each=numL.ClinBenif*numL.BioResp*numL.stop),
                       times=numL.tumorType*numL.blBMK)
      BMK.in    <- rep(rep(blBMK, each=numL.ClinBenif*numL.BioResp*numL.stop*numL.dose), 
                       times=numL.tumorType)  
      tumor.in  <- rep(rep(tumorType),
                       each=numL.ClinBenif*numL.BioResp*numL.stop*numL.dose*numL.blBMK)
      numRow <- length(BMK.in)
      if(any(c(length(ClinBenif), length(BioResp), length(stop0), length(dose.in),
               length(tumor.in))!=numRow)){
        stop('Error in level definition: check input value for blBMK, tumorType, dose')
      }
      mat <- cbind(tumorType=tumor.in, blBMK=BMK.in, 
                        dose=dose.in, stop0=stop0, 
                        BioResp=BioResp, ClinBenif=ClinBenif)
      fun1<-function(x, lastCol=NULL){
        if(length(x)<=1) return(x)
        sel <- c(FALSE, x[2:length(x)]==x[1:(length(x)-1)])
        if(!is.null(lastCol)) sel <- sel & (lastCol=="")
        x[sel] <- ""
        return(x)
      }
      
      mat.tab <- mat[mat[,"stop0"]=='no', ]
      m11 <- matrix(fun1(mat.tab[,1]), ncol=1)
      for(i in 2:ncol(mat.tab)){
        m11 <- cbind(m11, fun1(mat.tab[,i], m11[,ncol(m11)]))
      }
      dimnames(m11) <- dimnames(mat.tab)
      mat.tab <- m11
  
      if(is.matrix(mat.tab)) {
        mat.tab[which(mat.tab[,'dose']!=''), 'stop0'] <- 'no'
      }
    }
    
    
    #build arrows' coordinates
    if(showTree){
      
      tot.col <- ncol(mat.tab)
      tot.row <- nrow(mat.tab)
      tot.col2<- tot.col+(tot.col-3)+2 #to enable the fitting lines longer
      text.size1 <- 3/log(tot.row)
      
      th <- th.arrow/log(tot.row)
      
      y.tt <- which(mat.tab[,1]!='')
      pnt <- data.frame(x=c(0, rep(1, length(y.tt))), y=c(1, y.tt), 
                        pch=rep(22,length(y.tt)+1), cex=rep(3,length(y.tt)+1))
      arr <- data.frame(x0=rep(0, length(y.tt))+th*0.8, 
                        y0=rep(1, length(y.tt)), 
                        x1=rep(1, length(y.tt))-th*0.8, 
                        y1=y.tt-th, 
                        lty=rep(1, length(y.tt)),
                        lwd=rep(1, length(y.tt)), 
                        col=rep('gray80', length(y.tt)))  
      y.tt0 <-y.tt;
      dum1 <- 1
      for(i in 2:tot.col){
        y.tt <- which(mat.tab[,i]!='')
        lty1 <- 1;  lwd1 <- 1; col1='gray80';
        if (i>=4) {col1 <- 'black'}
        if (i==5) {lty1<-4; }
  
        if(i <= 3){
          pnt <- rbind(pnt, 
              data.frame(x=rep(i, length(y.tt)), 
                   y=y.tt, pch=22, cex=3) )
          
          arr <- rbind(arr, 
                       data.frame(x0=rep(i-1, length(y.tt))+th*0.8, 
                                  y0=rep(which(mat.tab[,i-1]!=''), each=length(y.tt)/length(y.tt0)),
                                  x1=rep(i, length(y.tt))-th*0.8,
                                  y1=y.tt, 
                                  lty=rep(lty1, length(y.tt)),
                                  lwd=rep(lwd1, length(y.tt)),
                                  col=rep(col1, length(y.tt))))
        }else{
          arr <- rbind(arr, 
                       data.frame(x0=rep(i+dum1-2, length(y.tt))+th , 
                                  y0=rep(which(mat.tab[,i-1]!=''), each=length(y.tt)/length(y.tt0)),
                                  x1=rep(i+dum1, length(y.tt))-th,
                                  y1=y.tt, 
                                  lty=rep(lty1, length(y.tt)),
                                  lwd=rep(lwd1, length(y.tt)),
                                  col=rep(col1, length(y.tt))))
          dum1 <- dum1+1
        }
        y.tt0 <-y.tt
      }
      
      
      par(mar=c(0.2, 0.2, 0.2, 0.2), mfrow=c(1,1))
      plot(0~0, col='white', ylim=c(0, tot.row+3), xlim=c(0, tot.col2+ 0.5),
           axes=F, ylab='', xlab='')
      dum2 <- 0;   pos.col<-NULL; 
      th.p <- 0.5; #threshold for probability X-axis position
      #joint probability for Pr(br=1,...), Pr(br=0, ...), Pr(br=1,...), Pr(br=0, ...), etc.
      num.mat.L <- nrow(mat.tab)/2
      print(num.mat.L)
      j.prob <- rep(1, num.mat.L) 
      for(i in 1:tot.col){
        if(i>3) { dum2 <- dum2+1; th.p <- 1;}
        text(x=i+dum2, y=1:tot.row, labels=mat.tab[,i], cex=text.size1)
        pos.col <- c(pos.col, i+dum2)
        
        if(showProb){
          # add probabilities to the tree and the the expected Utility for each action
          if(i==2){
            text(x=i+dum2-th.p, y=which(mat.tab[,i]!=''), labels=prior_ti, cex=text.size1, col='gray')
            print(prior_ti)
            j.prob <- j.prob * rep(prior_ti, each=num.mat.L/length(prior_ti))
             print(j.prob)
          }else if(i==3){
            if(length(prior_prop)==length(dose)) 
              prior_prop <- rep(prior_prop, numL.blBMK*numL.tumorType)
            text(x=i+dum2-th.p, y=which(mat.tab[,i]!=''), labels=prior_prop, cex=text.size1, col='gray')
            j.prob <- j.prob * rep(prior_prop, each=num.mat.L/length(prior_prop))
             print(j.prob)
          }else if (i==4){
            if(length(prob_stop0)==length(dose))
               prob_stop0 <- rep(prob_stop0, numL.blBMK*numL.tumorType) 
            text(x=i+dum2-th.p, y=which(mat.tab[,i]!=''), labels=prob_stop0, cex=text.size1, col='gray')
            j.prob <- j.prob * rep(prob_stop0, each=num.mat.L/length(prob_stop0))
             print(j.prob)
          }else if (i==5){
            if(length(prob_BR1)==numL.dose && numL.dose<numL.blBMK*numL.tumorType)
              prob_BR1 <- rep(prob_BR1, numL.blBMK*numL.tumorType)
            text(x=i+dum2-th.p, y=which(mat.tab[,i]=='yes'), labels=prob_BR1, cex=text.size1, col='gray') 
            #the prob are Pr(br=1|...), Pr(br=0|...), Pr(br=1|...), Pr(br=0|...), etc.
            prob_BR <- as.vector(rbind(prob_BR1, 1-prob_BR1))
            j.prob <- j.prob * prob_BR
             print(j.prob)
          }else if (i==6){
            if(length(prob_CB1_BR)==2)
              prob_CB1_BR <- rep(prob_CB1_BR, numL.blBMK*numL.tumorType)
            #Pr(CB=1|BR=1,...), Pr(CB=1|BR=0,...), Pr(CB=1|BR=1,...), Pr(CB=1|BR=0,...), etc.
            text(x=i+dum2-th.p, y=which(mat.tab[,i]=='yes'), labels=prob_CB1_BR, cex=text.size1, col='gray')          
            #for U(CB=0|....)
            #Pr(CB=0,BR=1|...), Pr(CB=0,BR=0|...),Pr(CB=0,BR=1|...), Pr(CB=0,BR=0|...), etc.
            j.prob0 <- round(payoff[2]*j.prob * trunc01(1-prob_CB1_BR), 3)          
            #for U(CB=1|....)
            #Pr(CB=1,BR=1|...), Pr(CB=1,BR=0|...),Pr(CB=1,BR=1|...), Pr(CB=1,BR=0|...), etc.
            j.prob <- round(payoff[1]*j.prob * prob_CB1_BR, 3)
            
  
            
            #color the top 10%
            num.col <- round(length(j.prob)*topRatio)
            sub.u <- j.prob+j.prob0
            top.p   <- sort(sub.u, decreasing=TRUE)[1:num.col]
            col.p   <- rep('gray', length(j.prob))
            col.p[sub.u%in%top.p] <- topCol
            print(j.prob)
            text(x=i+dum2+1, y=which(mat.tab[,i]=='yes'), labels=j.prob, 
                 cex=text.size1, col=col.p) 
            text(x=i+dum2+2.5, y=which(mat.tab[,i]=='yes'), labels=j.prob0, 
                 cex=text.size1, col='gray')           
            
            #add the utility into the treat leave
            mat.tab <- cbind(mat.tab, 
                             U=rep("", nrow(mat.tab)),
                             U0=rep("", nrow(mat.tab)),
                             topColor=rep("", nrow(mat.tab)))
            
            print(prob_CB1_BR)
            print(j.prob)
            print(mat.tab)
  
            mat.tab[mat.tab[, i]=='yes', 'U']<-j.prob
            mat.tab[mat.tab[, i]=='yes', 'U0']<-j.prob0
            mat.tab[mat.tab[, i]=='yes', 'topColor']<-col.p
            #get the index for top p values in output table
            wh.top.p<- which(mat.tab[,'topColor']==topCol)
            
          }
          
        }
      }
      points(x=pnt$x, y=pnt$y, pch=pnt$pch, cex=pnt$cex, col='gray80')
      
      arr$col <- as.character(arr$col)
      arrows(x0=arr$x0, y0=arr$y0, x1=arr$x1, y1=arr$y1, length=0.1, lty=arr$lty, lwd=arr$lwd, 
             col=arr$col)
      
      nms <- c('Tumor\nType (T)', 'Baseline\nBiomarker (B)', '\nDose', 
               'Stop due\nto toxicity', 'Biological\nResponse', 
               'Clinical\nBenefit')
      mtext(text=nms, side=3, at=pos.col, padj=1.1)
      
      #It is alway true: P(BR|stop=1)=0
      note1 <- c('P(stop=0|dose)', 'P(BR=1|stop=0, dose, TI)', 'P(CB=1|BR)') 
      text(x=pos.col[-(1:3)]-1, y=tot.row, labels=note1, col='darkgreen', cex=text.size1)
      
      #Assume {dose, stop} is independent from the tumor incident of a biomarker in the tumor type.
      #TI is independent from dose, so P({T,B}|dose)=P({T,B})
      note2 <- c('PriorInfo\nTumorIncidence(TI)', 'Proportion\nDoseLevel')
      text(x=c(pos.col[1]-0.5, pos.col[2]+0.5), y=tot.row, labels=note2, col='blue', cex=text.size1)
      
      if(showProb){
        note3 <- c(paste0('U=', payoff[1], '*Prob(CB=1, BR,\nstop=0, dose, {T,B})'),
                   paste0('U=', payoff[2], '*Prob(CB=0, BR,\nstop=0, dose, {T,B})'))
        pos.note3 <- tot.col2 + c(-1, 0.5)
        text(labels=note3, x=pos.note3, y=rep(tot.row+1, 2), 
             cex=text.size1*0.7, col=c('red', 'magenta'))
        
        #add the expected utility
        mat.tab <- cbind(mat.tab, 
                         U_dTB=rep("", nrow(mat.tab)),
                         U_dTB_color=rep("", nrow(mat.tab)),
                         U_dTB_topColor=rep("", nrow(mat.tab)))
        dum.u <- 0; dum.col <- ''
        for(r in nrow(mat.tab):1){
          if(mat.tab[r,'dose']=="" & mat.tab[r,'U']!=''){
            payoff.pos <-as.numeric(mat.tab[r, 'U'])
            payoff.neg <-as.numeric(mat.tab[r, 'U0'])
            dum.u <- dum.u+ payoff.pos + payoff.neg
            if(mat.tab[r,'topColor']!='') 
              dum.col<-mat.tab[r,'topColor']
          }else if (mat.tab[r,'U']!=''){
            payoff.pos <-as.numeric(mat.tab[r, 'U'])
            payoff.neg <-as.numeric(mat.tab[r, 'U0'])
            dum.u <- dum.u+ payoff.pos + payoff.neg
            mat.tab[r, 'U_dTB'] <- dum.u
            dum.u <- 0
            if(mat.tab[r,'topColor']!='') {
              dum.col<-mat.tab[r,'topColor']
              mat.tab[r, 'U_dTB_color']<-dum.col
            }
          }
        }      
        top.U_dTB <- sort(as.numeric(mat.tab[,"U_dTB"]), decreasing=T)
        top.U_dTB <- top.U_dTB[1:ceiling(length(top.U_dTB)*topRatio)]
        
        u.tDB <- mat.tab[,'U_dTB']; 
        wh.utDB <- which(u.tDB!='')
        mat.tab[wh.utDB, 'U_dTB_topColor']<-'black'
        mat.tab[wh.utDB&u.tDB%in%as.character(top.U_dTB), 'U_dTB_topColor']<-'red'
        u.tDB <- u.tDB[wh.utDB];
        #u.tDB.col<-mat.tab[wh.utDB, 'U_dTB_color']
        u.tDB.col<-mat.tab[wh.utDB, 'U_dTB_topColor']
        note4 <- paste0('U(d,T,B)= ', u.tDB)
        
        if(showBar){
          for(k in 1:length(u.tDB)){
            lines(x=0+c(0, as.numeric(u.tDB[k])), y=rep(wh.utDB[k]-th.utDB, 2), lwd=8, 
                  col=rgb(0, 0, 255, alpha=80, maxColorValue=255) )
          }
          lines(x=c(0,0), y=c(0,nrow(mat.tab)), 
                col=rgb(0, 0, 255, alpha=80, maxColorValue=255))
        }
        
        #u.tDB.col[u.tDB.col=='gray'] <- 'black'
        text(labels=note4, x=3, y=wh.utDB-th.utDB, 
             cex=text.size1, col=u.tDB.col)
        
        
      }
  
    }
    
    
    return(mat.tab)
    
  }
  
  #Obtain combination annotation
  anno_tt_bmk <- function(bmk, tt, other.note=''){
    bmk <- strsplit(as.character(bmk), split=',', fixed=TRUE)[[1]]
    tt <- strsplit(as.character(tt), split=',', fixed=TRUE)[[1]]
    ot <- paste(rep(tt, each=length(bmk)), rep(bmk, length(tt)), sep='_')
    ot <- paste(ot, collapse=',')
    ot <- paste(other.note, ot)
    return(ot)
  }
}
#End 1. -----------------------------------------------------------------------#



#Begin 2. ---------------------------------------------------------------------#
#1. Extension to have the CSF analysis
#2. Generalized to user-defined variables
#3. For discrete variables
if(TRUE){
  #expected loss function for discrete X variables
  #L(theta,a)=0 if x in the range else abs(lev-a)*abs(theta-th)
  #f(theta|data) is a binomial distriubtion
  #Users can define their own expected loss function however the input must
  #be [th, n, p_pos] and the output must be a vector of expected loss under
  #each Bayes decision levels
  my.eLoss <<- function(
    th,             #the vector of thresholds (delta) of decision rule
    n,              #the number of patients in a cohort
    p_pos,          #the posterior probability of responding to drug
                    #is the value update by data and affect decsion loss
    d.fun=pbinom    #probability function (lower.tail=T)
  ){
    #defin the distribution as binomial
    #d.fun=pbinom   #the density function of p_pos
    #user can re-define the loss function from here to the end....
    
    th <- sort(as.numeric(th))
    len_a<- length(th)
    
    #~~~data construction~~~#
    #the number of decision levels is the number thresholds plus 1
    xs <- 1:n              #get all numbers in the binomial distribution
    xs_lev <- rep(1, n)    #get decision levels for each number
    for(i in 1:len_a) xs_lev[xs>th[i]]<-i+1
    
    #~~~get the expected loss for each action~~~#
    #a is the action level from 0 to len_a according to theta
    e_loss <- rep(0, len_a+1)
    
    #construct loss elements based on the higher bound for the lowest level
    i<-1
    els.h<-abs(xs_lev - i)*abs(xs-th[i])*(1-d.fun(xs, size=n, prob=p_pos))
    e_loss[i]<-sum(els.h[xs_lev>i])
    
    #expected loss from level 2 to lev_a
    for(i in 2:len_a){#if taking the action as level i
      els.h<-abs(xs_lev - i)*abs(xs-th[i])*(1-d.fun(xs, size=n, prob=p_pos))
      els.l<-abs(xs_lev - i)*abs(xs-th[i-1])*(1-d.fun(xs, size=n, prob=p_pos))
      e_loss[i] <- sum(els.h[xs_lev>i]) + sum(els.l[xs_lev<i])
    }
    
    #for highest level
    #construct loss elements based on the lower bound
    i <- len_a
    els.l<-abs(xs_lev - i-1)*abs(xs-th[i])*(1-d.fun(xs, size=n, prob=p_pos))
    e_loss[i+1]<-sum(els.l[xs_lev<=i])
    
    return(e_loss)
  }
  
  
  #improved function for Baysian decision theory with Critical Success Factor
  #available to add or remove variables
  #available to specify the Bayse loss function
  #available to select utility bar and the loss bar
  #Clinical Benifit and Biological Response are {0, 1} for {no or yes}. 
  #Output@: ranked table {dose, Disease, baselineBiomarker, expectedUtility}
  #Output@: tree plot
  #function name: Bayesian Decision Theory Utility and Loss
  #fixing the expected utility and make loss function flexible
  BDT_UaL <- function(
    levVars="B1::T1::coh1::BR1,B1::T1::coh2::BR1,B1::T2::coh3::BR2",       
                                  #variables and levels separated by "::"
    dr_lev="nogo::go,nogo::go,nogo::go",  #order does matter.
                                  #decision rule labels
    incidence="0.3,0.3,0.1",      #Biomarker incidence in the tumor type 
                                  #values obtained from prior knowledge
    pBprior=NULL,                 #the hyper parameters "alph, beta"
                                  #if NULL, then alpha=1+incidence
                                  #beta=2-incidence
                                  #if not NULL, the value should be like
                                  #"1.3 1.7,1.3 1.7, 1.1 1.9," 
    n_ij="10, 10, 10",            #sample sizes for each cohort
                                  #values obtained from decision makers
    dr_th="0.1,0.5,0.6",          #decision rule threshold (delta)
                                  #if 3 levels of decision rule such as 
                                  #dr_lev="go::moreData::nogo," then
                                  #dr_th="0.9::0.3,"
    drFunc=my.eLoss,              #user-defined Bayes decision loss function
                                  #input: [th, n, p_pos]
                                  #output: a vector of Bayes decision loss
    showBar=TRUE,                 #show the barplot of utility & loss
    th.arrow= 0.8,                #horizontal space between an arrow and target
    payoff= c(10, -1)            #payoff value for utility, only two values
                                 #gain vs lost
  ){
    #an internal function truncates values to 0, 1
    trunc01 <- function(val){
      val[val>1]<-1
      val[val<-0]<-0
      return(val)
    }
    
    #an internal function gets the hierarchical variables
    my.split1 <- function(mylab="", s1=",", s2="::"){
      if(length(mylab)==1 & is.character(mylab)){
        L1 <- strsplit(mylab, split=s1)[[1]]
      }else{L1 <- mylab}
      if(length(L1)==0) return('L1 in my.split1 is missing.')
      if(is.null(s2)) return(L1)
      if(all(is.character(L1))){
        L2 <- strsplit(L1, split=s2)
      }else{
        L2 <- mylab
      }
      return(L2)
      #mat1<-t(matrix(unlist(L2), ncol=length(L2)))
    }
    
    
    #cleanup the input parameters
    if(TRUE){
      #sample size proportions
      if(is.null(n_ij) || all(n_ij=='')|all(incidence=='')|
         all(dr_th=='')) return(NULL)
      n_1 <- as.numeric(unlist(my.split1(n_ij)))
      n_rt<- n_1/sum(n_1)
      #incidences
      if(is.null(incidence)) return(NULL)
      incd<- as.numeric(unlist(my.split1(incidence)))
      
      p.0<-list()
      for(o in 1:length(incd)){
        p.0[[o]]<-c(1+incd[o], 2-incd[o])
      }
      
      if(!is.null(pBprior) && pBprior!="~" && 
         gsub(" ", "", pBprior)!=""){
        p.0p<-my.split1(pBprior, s2=" ")
        p.0p<-lapply(p.0p, function(x){x[x!=""]})
        for(o in 1:length(p.0)){
          p.0[[o]] <- p.0[[o]]+as.numeric(p.0p[[o]])
        }
        #note the length p.0 == the leve of plans
      }
      num.p0 <- length(p.0)
      
      #if using default threshold
      if(dr_th=="~"){
        th1 <- list()
        for(o in 1:length(p.0))
          th1[[o]] <- p.0[[o]][1]/sum(p.0[[o]])
      }else{
        th1 <- lapply(my.split1(dr_th),as.numeric)
      }
      print(th1)
      
      #cleanup the payoff values
      if(is.character(payoff)){
        payoff <- as.numeric(strsplit(payoff, split=',', fixed=TRUE)[[1]])
        if(length(payoff)==1){
          payoff<-c(payoff, 0)
        }else{
          payoff <- payoff[1:2]
        }
      }
    }
    
    #construct the decision tree with user-defined variables
    if(TRUE){
      
      if(is.null(levVars)) return(NULL)

      LV0 <- LV <- my.split1(levVars)
      num.var <- length(LV[[1]])
      drLV  <- my.split1(dr_lev)
      num.dr  <- length(drLV[[1]])
      
      if(length(LV)!=length(drLV) | length(LV)!=num.p0){
        #print('Error: lengths of decision rule and layers do not match!')
        return(NULL)
      }
      
      iLV <- E.L <- U <- p.1 <- list()
      for(o in 1:length(LV)){
        th2<-round(th1[[o]]*n_1[o])
        p.1[[o]] <- p.0[[o]][1]/sum(p.0[[o]])
        eL <- drFunc(th=th2,
                     n=n_1[o],
                     p_pos=p.1[[o]] )
        E.L[[o]]<-eL  #expected loss
        
        U[[o]] <- incd[o]*n_rt[o]*p.1[[o]]*payoff[1]+
          incd[o]*n_rt[o]*(1-p.1[[o]])*payoff[2]
        #expected utility
        
        if(o>1){
          o.wh <- which(LV0[[o]]!=LV0[[o-1]])[1]
          if(length(o.wh)==0) o.wh<-1
          no.wh <- which(LV0[[o]]==LV0[[o-1]])
          LV[[o]][ no.wh[no.wh<o.wh] ]<-''
        }
        
        iLV[[o]] <- c(LV[[o]][1:(num.var-1)],
                      paste0(LV[[o]][num.var], ", n=", n_1[o],
                             "\nI=", incd[o],
                             ", U=", round(U[[o]],3),
                             ", p=", round(p.1[[o]],3)  ), 
                      paste0(drLV[[o]][1],": go if r>", th2[1],
                             ", E(L)=", round(eL[1],3)))
        if(num.dr==1) next
        for(h in 2:num.dr){
          if(h==num.dr){
            iLV[[o]] <- c(iLV[[o]], rep('', num.var), 
                          paste0(drLV[[o]][h], ": stop if r<=",th2[h-1],
                                 ", E(L)=", round(eL[h],3))  )
          }else{
            iLV[[o]] <- c(iLV[[o]], rep('', num.var), 
                          paste0(drLV[[o]][h], ": ",th2[h-1],
                                 "<= r <",th2[h],
                                 ", E(L)=", round(eL[h],3))  )
          }
        }
      }
      varMat <- t(matrix(unlist(iLV), nrow=num.var+1))
      #E.L[[o]]: expected Bayes decision loss
      #U[[o]]: utility of plan
    }
    
    #build arrows and coordinates to show the decision tree
    if(TRUE){
      tot.col   <- ncol(varMat)
      max.nchar <- apply(varMat, 2, function(x){max(nchar(x))})
      cex.1char<- 0.1
      tot.row <- nrow(varMat)
      tot.col2<- tot.col+2 #to enable the fitting lines longer
      text.size1 <- 3/log(tot.row)
      th.a <- th.arrow/log(tot.row) #about arrow locaiton
      
      #for Layer 1
      lty1 <- 1;  lwd1 <- 1; col1='gray80';
      y.tt <- which(varMat[,1]!='')
      pnt <- data.frame(x=c(0, rep(1, length(y.tt))), y=c(1, y.tt), 
                        lab=c('', varMat[y.tt,1]),
                        pch=rep(22,length(y.tt)+1), cex=rep(3,length(y.tt)+1))
      arr <- data.frame(x0=rep(0, length(y.tt)), 
                        y0=rep(1, length(y.tt)), 
                        x1=rep(1, length(y.tt)), 
                        y1=y.tt, 
                        lty=rep(1, length(y.tt)),
                        lwd=rep(1, length(y.tt)), 
                        col=rep(col1, length(y.tt)))  
      shf <- sum(max.nchar[1])*cex.1char
      y.tt0 <- y.tt
      for(i in 2:tot.col){
        y.tt <- which(varMat[,i]!='')
        pnt <- rbind(pnt, data.frame(x=rep(i+shf, length(y.tt)), 
                                     y=y.tt, lab=varMat[y.tt,i],
                                     pch=22, cex=3) )
        wh.a1<-which(!y.tt%in%y.tt0)
        y.tt0a <- y.tt
        for(a in wh.a1){
          y.tt0a[a] <- y.tt0a[a-1]
        }
        arr <- rbind(arr,
                     data.frame(x0=rep(i-1+shf, length(y.tt)), 
                                y0=y.tt0a,
                                x1=rep(i+shf, length(y.tt)),
                                y1=y.tt, 
                                lty=rep(lty1, length(y.tt)),
                                lwd=rep(lwd1, length(y.tt)),
                                col=rep(col1, length(y.tt))))
        shf <- sum(max.nchar[1:i])*cex.1char
        y.tt0 <- y.tt
      }
      
      
      par(mar=c(0.2, 0.2, 0.2, 0.2), mfrow=c(1,1))
      plot(y~x, data=pnt, col='gray80', pch=pnt$pch,
           ylim=c(0, tot.row), xlim=c(0, tot.col2+shf+0.5),
           axes=F, ylab='', xlab='')
      text(x=pnt$x, y=pnt$y, labels=pnt$lab, adj=-0.07)
      arrows(x0=arr$x0, y0=arr$y0, x1=arr$x1, y1=arr$y1, 
             length=0.1, lty=arr$lty, lwd=arr$lwd, 
             col=arr$col)
    }
    
    #add barplot of utility and loss
    if(showBar){
      u.x0<-rep(0, length(LV))
      u.y0<-which(varMat[,ncol(varMat)-1]!='')
      u.x1<-unlist(U)
      col.bar1 <- rgb(0, 0, 255, alpha=80, maxColorValue=255)
      abline(v=0, col=col.bar1)
      for(i in 1:length(u.y0)){
        lines(x=c(u.x0[i], u.x1[i]), y=c(u.y0[i], u.y0[i]),
              lwd=8, 
              col=col.bar1)
      }

      l.x0<-rep(tot.col2+shf, nrow(varMat))
      l.y0<-1:nrow(varMat)
      l.x1<-l.x0-unlist(E.L)
      col.bar2 <- rgb(255, 0, 0, alpha=80, maxColorValue=255)
      abline(v=tot.col2+shf, col=col.bar2)
      for(i in 1:length(l.y0)){
        lines(x=c(l.x0[i], l.x1[i]), y=c(l.y0[i], l.y0[i]),
              lwd=8, 
              col=col.bar2)
      }
    }
    
    return(list(dat=varMat, BayesLoss=E.L, U=U, p=p.1))
    
  }
  
}

#define global variables for FDA_log analysis
if(TRUE){
  #get a subset
  key.words <<- c('all', 'NSCLC|lung', 
                  'urothelial', 
                  'gastrointestinal', 
                  'msi', 'breast', 'head', 'hcc',
                  'other')
  mySelect<-function(vec1, sp=NULL,
                     ctype='all',
                     fdaLink=FALSE, 
                     allkeys=key.words  ){
    
    if(!is.null(fdaLink)&&as.logical(fdaLink)){
      vec1 <- readLines(fL)
      #vec1 <- vec1[grepl('<li>', vec1)]
      vec1 <- vec1[grepl('approv', vec1)]
      if(length(vec1)==1){
        vec1<-vec1[grepl('href=\"#updates\"', vec1, fixed=T)]
        vec1<-strsplit(split='<li>', vec1, fixed=TRUE)[[1]]
        vec1<-gsub('\t|  ','', vec1)
        vec1 <- vec1[grepl('approv', vec1)]
      }
      if(length(vec1)==0){
        return(data.frame(FDA_log='Fail to reach the link.'))
      }
    }
    if(is.null(sp) || is.null(ctype)){
      return(data.frame(FDA_log='no data'))
    }
    if(!ctype%in%c('all')){
      if(ctype=='other'){
        ctype <- paste(allkeys[!allkeys%in%c('all','other')], collapse='|')
        vec1<-vec1[!grepl(ctype, vec1, ignore.case=T)]
      }else{
        vec1<-vec1[grepl(ctype, vec1, ignore.case=T)]
      }
      if(length(vec1)==0){
        return(data.frame(FDA_log='The disease is not found.'))
      }
    }
    if(sp==''|sp=='~'){
      if(is.data.frame(vec1)){
        dat1 <- data.frame(FDA_log=vec1[,1])
      }else if (is.vector(vec1)){
        dat1<- data.frame(FDA_log=vec1)
      }else{
        dat1<-data.frame(FDA_log='no data')
      }
      return(dat1)
    }
    vec2<- vec1[grepl(
      paste(paste0("(?=.*",strsplit(as.character(sp), split='&')[[1]], ")"), 
            collapse=""), vec1, perl=T, ignore.case=T)]
    dat1<-data.frame(FDA_log=vec2)
    return(dat1)
  }
}
#End 2. -----------------------------------------------------------------------#


#Begin 3. ---------------------------------------------------------------------#
#1.add contineus variables
#2.add benchmark reference
if(TRUE){
  
  #A function for P(y.trt-y.ref<x) given the equal length samples of y.trt, y.ref
  prob.diff <- function(x, y.diff=NULL, y.trt=NULL, y.ref=NULL){
    if(!is.null(y.diff)){
      dif1 <- y.diff[is.finite(y.diff)]
      return( mean(dif1<x, na.rm=T) )
    }else if(!is.null(y.trt) & !is.null(y.ref) & length(y.trt)==length(y.ref)){
      dif1 <- (y.trt-y.ref)
      dif1 <- dif1[is.finite(dif1)]
      return( mean(dif1<x, na.rm=T) )
    }else{return(0)}
  }
  #functions getting the random samples from given distribution
  #rbinom with n and pi
  #rlnorm with meanlog and sdlog
  
  
  #expected loss function for either discrete or continues X variables
  #L(theta,a)=0 if x in the range else abs(lev-a)*abs(theta-th)
  #f(theta|data) is a binomial distriubtion
  #Users can define their own expected loss function however the input must
  #be [th, n, p_pos] and the output must be a vector of expected loss under
  #each Bayes decision levels
  eLoss.diff <<- function(
    th=NULL,              #the vector of thresholds (delta) of decision rule
    sample1,             #a vector of samples under treatment assumption
    sample2,             #a vector of samples under control assumption
    sample1.prob,        #a vector of samples prob under treatment assumption
    sample2.prob,        #a vector of samples prob under control assumption
    len_cutoffs=NULL #the number of cutoffs or thresholds of a decision rule
  ){
    prob.1g2<-NULL
    #create the matrix of difference and the probablity
    if(!is.vector(sample1) | !is.vector(sample1.prob) |
       length(sample1)!=length(sample1.prob)){
      stop('Sample1 input is wrong for eLoss.diff')
    }else if(!is.vector(sample2) | !is.vector(sample2.prob) |
       length(sample2)!=length(sample2.prob)){
      stop('Sample2 input is wrong for eLoss.diff')
    }else{
      n.r <- length(sample1)
      n.c <- length(sample2)
      sam1.mat<-matrix(sample1, nrow=n.r, ncol=n.c)
      sam1prob.mat<-matrix(sample1.prob, nrow=n.r, ncol=n.c)
      sam2.mat<-t(matrix(sample2, nrow=n.c, ncol=n.r))
      sam2prob.mat<-t(matrix(sample2.prob, nrow=n.c, ncol=n.r))
      prob.mat <- sam1prob.mat*sam2prob.mat
      diff.mat <- sam1.mat - sam2.mat
      y2sampleD <- as.vector(diff.mat)
      ord <- order(y2sampleD)
      y2sampleD <- y2sampleD[ord]
      y2sampleD.prob <- as.vector(prob.mat)[ord]
      prob.1g2 <- sum(y2sampleD.prob[y2sampleD>0])
    }
    
    
    #Recomend threshold maximizing the loss difference
    if(is.null(th)||th%in%c("", "~", "-")){
      if(is.null(len_cutoffs)){len_cutoffs <- 1}
      th.prob <- seq(0,1, by=1/(len_cutoffs+1))
      th.prob <- th.prob[c(-1, -length(th.prob))]
      th <- quantile(y2sampleD, th.prob)
      recom.th <- TRUE
    }else{recom.th<-FALSE}
    
    
    #order the thereshold for a decision rule
    th <- sort(as.numeric(th))
    len_a<- length(th)
    n  <- length(y2sampleD)
    
    #~~~data construction~~~#
    #the number of decision levels is the number thresholds plus 1
    xs <- y2sampleD  #get the distribution consits of samples
    xs_lev <- rep(1, n)    #get decision levels for each sample
    for(i in 1:len_a) {
      xs_lev[xs>th[i]]<-i+1
    }
    
    #~~~get the expected loss for each action~~~#
    #a is the action level from 0 to len_a according to theta
    e_loss <- rep(0, len_a+1)
    
    #construct loss elements based on the higher bound for the lowest level
    #i<-1
    els.h<-abs(xs_lev - 1)*abs(xs-th[1])*y2sampleD.prob
    e_loss[1]<-sum(els.h[xs_lev>1])
    
    #expected loss from level 2 to lev_a
    for(i in 2:len_a){#if taking the action as level i
      els.h<-abs(xs_lev - i)*abs(xs-th[i])*y2sampleD.prob
      els.l<-abs(xs_lev - i)*abs(xs-th[i-1])*y2sampleD.prob
      e_loss[i] <- sum(els.h[xs_lev>i]) + sum(els.l[xs_lev<i])
    }
    
    #for highest level
    #construct loss elements based on the lower bound
    els.l<-abs(xs_lev - len_a-1)*abs(xs-th[len_a])*y2sampleD.prob
    e_loss[len_a+1]<-sum(els.l[xs_lev<=len_a])
    
    if(recom.th){
      return(list(e_loss=e_loss, th=th, p.1g2=prob.1g2))
    }else{
      return(list(e_loss=e_loss, p.1g2=prob.1g2))  
    }
    
  }
  
  #A revised function of generating random variable from a lognormal distb
  rlnorm2 <- function(B=1000000, m, s){
    location <- log(m^2 / sqrt(s^2 + m^2))
    shape <- sqrt(log(1 + (s^2 / m^2)))
    smp <- rlnorm(n=B, location, shape)
    return(smp)
  }
  
  #improved function for Baysian decision theory with Critical Success Factor
  #take the difference between treatment variable and the benchmark control
  #available to add or remove variables
  #available to specify the Bayse loss function
  #available to select utility bar and the loss bar
  #Clinical Benifit and Biological Response are {0, 1} for {no or yes}. 
  #Output@: ranked table {dose, Disease, baselineBiomarker, expectedUtility}
  #Output@: tree plot
  #function name: Bayesian Decision Theory Utility and Loss
  #fixing the expected utility and make loss function flexible
  BDT_UaL.diff <- function(
    levVars="B1::T1::coh1::BR1,B1::T1::coh2::BR1,B1::T2::coh3::BR2",       
    #variables and levels separated by "::"
    dr_lev="No Go::Go,No Go::Go,No Go::Go",  #order does matter.
    #decision rule labels
    incidence="0.3,0.3,0.1",      #Biomarker incidence in the tumor type 
    #values obtained from prior knowledge
    numRsp=NULL,                 #the hyper parameters "alph, 2-alpha"
    #if NULL, then alpha=1+incidence and beta=2-incidence, 
    #and only trt ORR is onsidered without the benchmark control ORR
    #if not NULL, the value should be like
    #"1.3vs1.3,1.3vs1.2, 1.1vs1" matches "TRT_ORRvsCTR_ORR"
    muTTE=NULL,
    sdTTE=NULL,           #the mean and sd of TTE variables
    #if not NULL, e.g. "5vs5, 9vs2, 10vs8" and "1vs1, 1vs1, 1vs1"
    #matches "TRTvsCTR"
    n_ij="10, 10, 10",            #sample sizes for each cohort
    #or "10vs20,10vs80,10vs10" for "TRTvsCTR"
    #values obtained from decision makers
    dr_th="0.1,0.5,0.6",          #decision rule threshold for ORR
    #if 3 levels of decision rule such as 
    #dr_lev="go::moreData::nogo," then
    #dr_th="0.9::0.3,"
    #input: [th, n, p_pos]
    #output: a vector of Bayes decision loss
    showBar=TRUE,                #show the barplot of utility & loss
    th.arrow= 0.8,               #horizontal space between an arrow and target
    payoff= c(10, -1),            #payoff value for utility, only two values
    #gain vs lost
    bar.wid=8,          #the width of the horizontal bar
    Bsample=5000        #number of bootstrapping samples
  ){
    #a list of difference in ORR between trt and ctr
    RT.diff <- list()
    #a list of difference in tte between trt and ctr
    tte.diff<- list()
    #by default, the difference variable between trt and ctr is not used.
    #it will be TRUE if 'vs' shows in the string of numRsp
    useRspDiff<-FALSE
    #it will be TRUE if 'vs's shows in the string muTTE
    useTteDiff<-FALSE
    
    #an internal function gets the hierarchical variables 
    #seperated by "::" for different levels of a variable
    #seperated by "," for different subgroups
    #seperated by "vs" for treatment vs control
    my.split1 <- function(mylab="", s1=",", s2="::"){
      if(length(mylab)==1 & is.character(mylab)){
        #the input is a string
        L1 <- strsplit(mylab, split=s1)[[1]]
      }else{ #when the input is already a vector
        L1 <- mylab
      }
      if(length(L1)==0) return('L1 in my.split1 is missing.')
      if(is.null(s2)) return(L1)
      if(all(is.character(L1))){
        L2 <- strsplit(L1, split=s2)
      }else{
        L2 <- mylab
      }
      return(L2)
    }
    
    
    #cleanup the input parameters
    if(TRUE){
      #Cleanup sample size parameters
      if(is.null(n_ij) || all(n_ij=='')|all(incidence=='')) return(NULL)
      if(grepl('vs',n_ij)){
        nL <- my.split1(n_ij, s2='vs')
        n_1.trt<- as.numeric(as.vector(sapply(nL,function(x){x[1]})))
        n_1.ctr<- as.numeric(as.vector(sapply(nL,function(x){x[2]})))
        n_1 <- n_1.trt #n_1 alwasy has values for treatment sizes
        if(length(n_1.trt)!=length(n_1.ctr)) return(NULL)
      } else {
        n_1 <- as.numeric(unlist(my.split1(n_ij)))
        n_1.trt <- n_1.ctr <- n_1
      }
      n_rt<- n_1/sum(n_1)
      
      #incidences
      if(is.null(incidence)) return(NULL)
      incd<- as.numeric(unlist(my.split1(incidence)))
      
      #for treatment prior parameters of Beta Variable pi based on incidences
      p.0<-list()
      for(o in 1:length(incd)){
        p.0[[o]]<-c(1+incd[o], 2-incd[o])
      }
      
      #posterior parameters of Beta Variable pi updated by trt ORR
      #and get RT.diff
      if(!is.null(numRsp) && numRsp!="~" && gsub(" ", "", numRsp)!=""){
        if(!(grepl("vs", numRsp)) && !is.null(n_1)){
          #when no sample sizes for control or benchmark
          #only the Beta posterrior parameters are updated
          p.0p<-as.numeric(my.split1(numRsp, s2=NULL))
          if(length(incd)!=length(p.0p)) return(NULL)
          if(length(p.0p)!=length(n_1)){
            #use the probability only without the sample sizes
            #if(any(p.0p>1|p.0p<0)) return(NULL)
            for(o in 1:length(p.0)){
              if(p.0p[o]>1){
                p.0[[o]]<-p.0[[o]]+c(p.0p[o], n_1[o]-p.0p[o])
              }else{
                p.0[[o]]<-p.0[[o]]+c(p.0p[o], 1-p.0p[o])
              }
            }
          } else {
            for(o in 1:length(p.0)){
              if(p.0p[o]>1){
                p.0[[o]]<-p.0[[o]]+c(p.0p[o], n_1[o]-p.0p[o])
              }else{
                p.0[[o]]<-p.0[[o]]+c(p.0p[o], 1-p.0p[o])
              }
            }
          }
          #note: p.0 is a listing object. Each element is a vector of
          #the two Beta parameters. The total number of elements matches
          #the number of incidences or cohorts/arms.
        }else{
          #when number of responders in control are inserted in numRsp
          #then the difference between trt and ref is given
          p.0p <- my.split1(numRsp, s2='vs')
          p.0p.trt<-as.numeric(sapply(p.0p, function(x){x[1]}))
          p.0p.ctr<-as.numeric(sapply(p.0p, function(x){x[2]}))
          if(is.null(n_1.ctr)) n_1.ctr <- n_1.trt
          if(!is.null(n_1.ctr) & length(p.0p)==length(n_1.ctr)){
            for(o in 1:length(p.0p)){
              if(p.0p.trt[o]>1){#if the new alpha>1
                p.0[[o]] <- p.0[[o]]+c(p.0p.trt[o], n_1[o]-p.0p.trt[o])
              }else{
                p.0[[o]] <- p.0[[o]]+n_1[o]*c(p.0p.trt[o], 1-p.0p.trt[o])
                #p.0p.trt[o] <- round(p.0p.trt[o], n_1[o])
                p.0p.trt[o] <- p.0p.trt[o]
              }
              if(p.0p.ctr[o]>1){
                prob.ctr<-p.0p.ctr[o]/n_1.ctr[o]
              }else{
                prob.ctr<-p.0p.ctr[o];
                p.0p.ctr[o]<-p.0p.ctr[o]*n_1.ctr[o]
              }
              if(F){
              sam.trt<-rbinom(Bsample, size=n_1.trt[o], 
                              prob=p.0[[o]][1]/sum(p.0[[o]]) )
              sam.ctr<-rbinom(Bsample, size=n_1.ctr[o], 
                              prob=prob.ctr )
              sam.trt<-sam.trt[!is.na(sam.trt) & is.finite(sam.trt)]
              sam.ctr<-sam.ctr[!is.na(sam.ctr) & is.finite(sam.ctr)]
              }
              sam.trt <- sam.ctr <- 0:n_1.trt[o]
              sam.trt.prob<-dbinom(sam.trt, size=n_1.trt[o], 
                              prob=p.0[[o]][1]/sum(p.0[[o]]) )
              sam.ctr.prob<-dbinom(sam.ctr, size=n_1.ctr[o], 
                              prob=prob.ctr )
              
              RT.diff[[o]] <- list()
              RT.diff[[o]]$sample1 <- sam.trt
              RT.diff[[o]]$sample2 <- sam.ctr
              RT.diff[[o]]$sample1.prob <- sam.trt.prob
              RT.diff[[o]]$sample2.prob <- sam.ctr.prob
              print(paste('sum(sam.trt.prob)', sum(sam.trt.prob))); 
              print(paste(p.0[[o]][1]/sum(p.0[[o]]), ',', prob.ctr))
              print(quantile(sam.trt.prob))
              print(quantile(sam.trt))
              print(quantile(sam.ctr))
              print(paste('sum(sam.ctr.prob)', sum(sam.ctr.prob)))
            }
            useRspDiff<-TRUE
          }else{return(NULL)}
        }
        #note: RT.diff[[o]] is a listing object for the o_th cohorts/arms. 
        #Four objects are saved in the listting RT.diff[[o]]: 
        #sample1, sample1.prob for treatment and  
        #sample2, sample2.prob for control
      }
      
      #Note:the length p.0 == the level of plans or #cohorts
      num.p0 <- length(p.0)
      
      #When using the default threshold
      if(!is.null(dr_th) && dr_th%in%c("~", "")){
        th1 <- list()
        #use prevalence for the thresholds
        for(o in 1:length(p.0))
          th1[[o]] <- p.0[[o]][1]/sum(p.0[[o]])
      }else{
        #when the thresholds are defined in a string such as "3::5,4::6"
        #then 3 and 4 are the lower cutoff for cohort 1 and 2, 
        #5 and 6 are the higher cutoff for cohort 1 and 2. 
        th1 <- lapply(my.split1(dr_th),as.numeric)
      }
      #note: th1 is a listing object. Each element is a vector of ORR 
      #cutoffs. The number of elements matches the number of incidences.
      
      #cleanup the two parameters for lognormal variables, such as TTE
      #Note: The benchmark control reference must be provided for the analysis
      tte.mu <- tte.sd <- list()
      if(!is.null(muTTE) & !is.null(sdTTE) & 
         grepl('vs', muTTE) & grepl('vs', sdTTE)){
        #a listing object of lognormal means. each element is the two means
        #of trt vs cntr
        tte.mu<-lapply(my.split1(muTTE, s2='vs'), as.numeric)
        tte.sd<-lapply(my.split1(sdTTE, s2='vs'), as.numeric)
        if(length(tte.mu)==length(tte.sd) & 
           all(sapply(tte.mu, length)==2) & 
           all(sapply(tte.sd, length)==2) &
           length(tte.mu)==length(incd)
           ){
          for(o in 1:length(incd)){
           cy.trt <- rlnorm2(B=Bsample, m=tte.mu[[o]][1], s=tte.sd[[o]][1])
           cy.ctr <- rlnorm2(B=Bsample, m=tte.mu[[o]][2], s=tte.sd[[o]][2])
           tte.diff[[o]] <- cy.trt-cy.ctr
           tte.diff[[o]] <- tte.diff[[o]][is.finite(tte.diff[[o]]) & 
                                            !is.na(tte.diff[[o]])]
          }
          useTteDiff<-TRUE
        }
      }
      
      #cleanup the payoff values
      if(is.character(payoff)){
        payoff <- as.numeric(strsplit(payoff, split=',', fixed=TRUE)[[1]])
        if(length(payoff)==1){
          payoff<-c(payoff, 0)
        }else{
          payoff <- payoff[1:2]
        }
      }
    }
    
    #construct the decision tree with user-defined variables
    #calculate Utility and Loss
    if(TRUE){
      
      #A listing object of plans. 
      #Each element is a vector of critical variables.
      if(is.null(levVars)) return(NULL)
      LV0 <- LV <- my.split1(levVars)
      num.var <- length(LV[[1]]) #number of variables
      
      #A listing object of decision rules. 
      #Each element is a vector of decision thresholds
      drLV  <- my.split1(dr_lev) 
      num.dr  <- length(drLV[[1]])
      
      if(length(LV)!=length(drLV) | length(LV)!=num.p0){
        #print('Error: lengths of decision rule and layers do not match!')
        return(NULL)
      }
      
      #lost, utility, response probability pi. 
      #p.2 is the posterior prob for benefit.
      iLV <- E.L <- U <- Utte <- p.1 <-p.2 <- list()
      for(o in 1:length(LV)){
        #conver the ORR_trt decision thresholds into numRsp_trt
        th2.lab<-th2<-round(th1[[o]]*n_1[o])
        
        #eLoss.diff(th, y2sampleD)
        if(useRspDiff){
          #if benchmark ref is available then the threshold th2 change to be
          #the difference. But th2.lab still keep as the original one
          th2 <- round((th1[[o]]-p.0p.ctr[o]/n_1.ctr[o])*n_1[o])
          print(th2); print(paste("length(RT.diff[[o]])", length(RT.diff[[o]])));
          eL <- eLoss.diff(th=th2, 
                           sample1=RT.diff[[o]]$sample1,
                           sample2=RT.diff[[o]]$sample2,
                           sample1.prob=RT.diff[[o]]$sample1.prob,
                           sample2.prob=RT.diff[[o]]$sample2.prob)
          #probability of the difference pi_trt-pi_ctr>0
          print(paste0('p.1[[',o,']]=', p.1[[o]] <- eL$p.1g2))
          eL <- eL$e_loss
        }else{
          #expected response probability pi_trt
          p.1[[o]] <- p.0[[o]][1]/sum(p.0[[o]])
          print(p.1[[o]])
          eL <- my.eLoss(th=th2, n=n_1[o], p_pos=p.1[[o]] )
        }
        E.L[[o]]<-eL  #expected loss
        
        #expected utility based on ORR
        U[[o]] <- incd[o]*n_rt[o]*p.1[[o]]*payoff[1]+
          incd[o]*n_rt[o]*(1-p.1[[o]])*payoff[2]
        
        #expected utility based on TTE
        if(useTteDiff){
          #P(mu_trt-mu_ctr>0|...)
          p.2[[o]] <- mean(tte.diff[[o]]>0)
          Utte[[o]] <- incd[o]*n_rt[o]*p.2[[o]]*payoff[1]+
            incd[o]*n_rt[o]*(1-p.2[[o]])*payoff[2]
        }
        
        if(o>1){
          o.wh <- which(LV0[[o]]!=LV0[[o-1]])[1]
          if(length(o.wh)==0) o.wh<-1
          no.wh <- which(LV0[[o]]==LV0[[o-1]])
          LV[[o]][ no.wh[no.wh<o.wh] ]<-''
        }
        
        iLV[[o]] <- c(LV[[o]][1:(num.var-1)],
                      paste0(LV[[o]][num.var], ", n=", n_1[o],
                             "\nI=", incd[o],
                             ", U_orr=", round(U[[o]],3),
                             ifelse(useRspDiff,
                                    ", p(trt>ref)=", 
                                    ", p="), round(p.1[[o]],3),
                             ifelse(useTteDiff, 
                                    paste0("\nU_tte=",round(Utte[[o]],3),
                                           ", m_trt=", tte.mu[[o]][1],
                                           ", m_ref=", tte.mu[[o]][2],
                                           ", s_trt=", tte.sd[[o]][1],
                                           ", s_ref=", tte.sd[[o]][2]), 
                                    "")), 
                      paste0(drLV[[o]][1],": go if r>", th2.lab[1],
                             ", E(L)=", round(eL[1],3)))
        if(num.dr==1) next
        for(h in 2:num.dr){
          if(h==num.dr){
            iLV[[o]] <- c(iLV[[o]], rep('', num.var), 
                          paste0(drLV[[o]][h], ": stop if r<=",th2.lab[h-1],
                                 ", E(L)=", round(eL[h],3))  )
          }else{
            iLV[[o]] <- c(iLV[[o]], rep('', num.var), 
                          paste0(drLV[[o]][h], ": ",th2.lab[h-1],
                                 "<= r <",th2.lab[h],
                                 ", E(L)=", round(eL[h],3))  )
          }
        }
      }
      varMat <- t(matrix(unlist(iLV), nrow=num.var+1))
      #E.L[[o]]: expected Bayes decision loss
      #U[[o]]: utility of plan based on ORR
      #Utte[[o]]: utility based on tte
    }
    
    #build arrows and coordinates to show the decision tree
    if(TRUE){
      tot.col   <- ncol(varMat)
      max.nchar <- apply(varMat, 2, function(x){max(nchar(x))})
      cex.1char<- 0.05 #affect distance between text and arrow
      tot.row <- nrow(varMat)
      tot.col2<- tot.col+2 #to enable the fitting lines longer
      text.size1 <- 3/log(tot.row)
      th.a <- th.arrow/log(tot.row) #about arrow locaiton
      
      #for Layer 1
      lty1 <- 1;  lwd1 <- 1; col1='gray80';
      y.tt <- which(varMat[,1]!='')
      pnt <- data.frame(x=c(0, rep(1, length(y.tt))), y=c(1, y.tt), 
                        lab=c('', varMat[y.tt,1]),
                        pch=rep(22,length(y.tt)+1), cex=rep(3,length(y.tt)+1))
      arr <- data.frame(x0=rep(0, length(y.tt)), 
                        y0=rep(1, length(y.tt)), 
                        x1=rep(1, length(y.tt)), 
                        y1=y.tt, 
                        lty=rep(1, length(y.tt)),
                        lwd=rep(1, length(y.tt)), 
                        col=rep(col1, length(y.tt)))  
      shf <- sum(max.nchar[1])*cex.1char
      y.tt0 <- y.tt
      for(i in 2:tot.col){
        y.tt <- which(varMat[,i]!='')
        pnt <- rbind(pnt, data.frame(x=rep(i+shf, length(y.tt)), 
                                     y=y.tt, lab=varMat[y.tt,i],
                                     pch=22, cex=3) )
        wh.a1<-which(!y.tt%in%y.tt0)
        y.tt0a <- y.tt
        for(a in wh.a1){
          y.tt0a[a] <- y.tt0a[a-1]
        }
        arr <- rbind(arr,
                     data.frame(x0=rep(i-0.5+shf, length(y.tt)), 
                                y0=y.tt0a,
                                x1=rep(i+shf, length(y.tt)),
                                y1=y.tt, 
                                lty=rep(lty1, length(y.tt)),
                                lwd=rep(lwd1, length(y.tt)),
                                col=rep(col1, length(y.tt))))
        shf <- sum(max.nchar[1:i])*cex.1char
        y.tt0 <- y.tt
      }
      
      
      par(mar=c(0.2, 0.2, 0.2, 0.2), mfrow=c(1,1))
      plot(y~x, data=pnt, col='gray80', pch=pnt$pch,
           ylim=c(0, tot.row), xlim=c(0, tot.col2+shf+0.5),
           axes=F, ylab='', xlab='')
      text(x=pnt$x, y=pnt$y, labels=pnt$lab, adj=-0.07)
      arrows(x0=arr$x0, y0=arr$y0, x1=arr$x1, y1=arr$y1, 
             length=0.1, lty=arr$lty, lwd=arr$lwd, 
             col=arr$col)
    }
    
    #add barplot of utility and loss
    if(showBar){
#      bar.wid <- 8
      u.x0<-rep(0, length(LV))
      u.y0<-which(varMat[,ncol(varMat)-1]!='')
      u.x1<-unlist(U)
      if(useTteDiff){utte.x1<-unlist(Utte)}else{utte.x1<-NULL}
      col.bar1 <- rgb(0, 0, 255, alpha=80, maxColorValue=255) #blue
      col.bar1tte <- rgb(0, 100, 0, alpha=80, maxColorValue=255)  #darkgreen
      abline(v=0, col=col.bar1)
      for(i in 1:length(u.y0)){
        lines(x=c(u.x0[i], u.x1[i]), y=c(u.y0[i], u.y0[i]),
              lwd=bar.wid, 
              col=col.bar1)
        if(useTteDiff){
          lines(x=c(u.x0[i], utte.x1[i]), y=c(u.y0[i], u.y0[i])+0.1,
                lwd=bar.wid, 
                col=col.bar1tte)
        }
      }
      
      l.x0<-rep(tot.col2+shf, nrow(varMat))
      l.y0<-1:nrow(varMat)
      l.x1<-l.x0-unlist(E.L)
      col.bar2 <- rgb(255, 0, 0, alpha=80, maxColorValue=255)
      abline(v=tot.col2+shf, col=col.bar2)
      for(i in 1:length(l.y0)){
        lines(x=c(l.x0[i], l.x1[i]), y=c(l.y0[i], l.y0[i]),
              lwd=bar.wid, 
              col=col.bar2)
      }
    }
    
    return(list(dat=varMat, BayesLoss=E.L, U=U, p=p.1))
    
  }
  
  
}

#End 3. -----------------------------------------------------------------------#


















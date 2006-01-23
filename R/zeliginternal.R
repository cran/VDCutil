# NOTE -- THESE ARE TO BE MOVED INTO ZELIG -- IF YOU MAKE CHANGES
# 	YOU MUST ALSO WORK WITH ZELIG TEAM TO GET THEM INTO THEIR DISTRIBUTION

# example : 
#
# print out all model descriptions for installed models
#
# ignored = 
#    sapply(zeligInstalledModels(),
#	function(x)cat(zmodel2string(zeligDescribeModel(x))),simplify=F)

# Functions to list Zelig Models


# The functions that VDC uses directly are

# zeligListModels
# zeligInstalledModels
# zeligDescribeModel
# zmodel2string 

# Please do not remove any of the arguments to these functions, change the default values
# or change the output format of zeligListModels, zeligInstalledModels, or zmodel2string
#
# Changing the output of ZeligDescribeModels is ok, provided that it returns NULL
# where the model is not described under the requested schema, and provides
# an output acceptable to zmodel2string



zeligListModels<-function(inZeligOnly=T) {
     if (inZeligOnly) {
    		tmp = ls(envir=asNamespace("Zelig"),pattern="^zelig2")
     } else { 
    		tmp = c( ls(envir=asNamespace("Zelig"),pattern="^zelig2"),
         		apropos("zelig2",mode="function"))
     }
     sub("zelig2","", tmp)
}

zeligInstalledModels<-function(inZeligOnly=T,schemaVersion="1.1") {
  chkpkgs<-function(name)  {
       zd=zeligDescribeModel(name,schemaVersion=schmemaVersion)
       if (is.null(zd)) {
                return (FALSE)
                # exclude models that are not documented explicitly
       }
       zdpd=zd$model[which(names(zd$model)=="packageDependency")]
       ow=options(warn=-1)
       ret = (class(try(sapply(zdpd,function(x)require(x$name,character.only=T)),silent=T))
		!="try-error")
	options(ow)
 	return (ret)
  }
  models<-zeligListModels(inZeligOnly=inZeligOnly)
     # Not being the trusting sort, lets check to see if we can run
     # a dummy formula. If not -- almost always means that something
     # required() is missing
     tmpModels<-sapply(models,chkpkgs)
  models[which(tmpModels)]
}

# this describes a model based on the model name
# it should return an object suitable for zmodel2string if a description
# exists _in that schema_. If the description does not exists, or is 
# not supported in the schema version, return NULL
# 
# zmodel2string()
# should return an XML document, in the appropriate schema
# e.g. zmodel2string(zeligDescribeModel("ls"))

zeligDescribeModel<-function(modelName,force=F,schemaVersion="1.1") {
    res=try(eval(call(paste("zcheck.",modelName,sep=""))),silent=T)
    if (inherits(res,"try-error")) {
        if (force) {
                res=zdescDefault(modelName)
        } else {
                res=NULL
        }
    }
    return(res)
}

zmodel2string<-function(modelDescription) {
     xmlList(modelDescription)
}

#################################################################
# INTERNAL METHODS
#################################################################

# default description object, used as a start for other methods
zdescDefault <-function(modelName,
        descriptionText=paste("A statistical model."),
        helpUrl="http://gking.harvard.edu/Zelig/docs/models/",
        labelText="other",
        packages=c("stats"),
        outcomeTypes=c("continuous","ordinal","nominal"),
        mulitiOutcomes=F,
        explanatoryTypes=c("continuous","ordinal","nominal")
        ) {
  # set elements before the package dependency
  res=list(
             model = list(
               name=modelName, 
               label=labelText,
               description = list(descriptionText),
               helpLink = list(url=helpUrl)
            )
          )
        
  # add the package-dependency tag if specified
  if (!is.null(packages)) {
         tmp = sapply(packages, function(x)list(name=x,version="0.1",relationship="required"),simplify=F)
         names(tmp)=replicate(length(packages),"packageDependency")
         res$model=c(res$model,tmp)
  }
  # set the formula and setx tags
  
  if (mulitiOutcomes){
      # explanatory and explanatoryTypes are added later
      res$model=c(res$model,list(
        formula = list(maxEquations="1", minEquations="1", simulEq="0", 
          equation = list(crossedAllowed="1", interceptAllowed="1",
            nestedAllowed="1", 
            outcome = list(minVar="1" , maxVar="1"),
          ),
        ), setx= list(maxSetx="2")
       )
      )
  } else {
      res$model=c(res$model,list(
        formula = list(maxEquations="1", minEquations="1", simulEq="0", 
          equation = list(crossedAllowed="1", interceptAllowed="1",
            nestedAllowed="1", 
            outcome = list(minVar="1" , maxVar="1"),
            explanatory = list(minVar="1"),
          ),
        ), setx= list(maxSetx="2")
       )
      )
      
     # set explanatoryTypes if specified
     if (!is.null(explanatoryTypes)) {
         tmp = sapply(explanatoryTypes, function(x)list(x),simplify=F)
         names(tmp)=replicate(length(explanatoryTypes),"modelingType")
         res$model$formula$equation$explanatory=c(res$model$formula$equation$explanatory,tmp)
      }
  }
  # set outcomeTypes if specified
  if (!is.null(outcomeTypes)) {
       tmp = sapply(outcomeTypes, function(x)list(x),simplify=F)
       names(tmp)=replicate(length(outcomeTypes),"modelingType")
       res$model$formula$equation$outcome=c(res$model$formula$equation$outcome,tmp)
  }
     
  class(res)="zelig.describe"
  return(res)
}

# helper function to convert lists to XML strings
xmlList<-function(x,name=NULL) {
   escape<-function(x) {
      x=gsub("&","&amp;",x)
      x=gsub("<","&lt;",x)
      x=gsub(">","&gt;",x)
      return(x)
   }

   res=""
   nestedElements = which(sapply(x,class)=="list")
   if (!is.null(name) && (name!="")) {
      res=paste(res,"<",escape(name),sep="")
      nonNested = x[which(sapply(x,class)!="list")]
      elemAtt=which(names(nonNested)!="")
      if (length(nonNested)==1 && is.null(names(nonNested))){
         elemText=1
      }else {
         elemText= which(names(nonNested)=="")
      }
      for (i in elemAtt) {
         res=paste(res," ",names(nonNested[i]),"=",'"',escape(nonNested[i])
              ,'"',sep="")
      }
      if ((name == 'helpLink') || (name == 'packageDependency') || (name == 'setx')){
        res=paste(res,"/>",sep="")
      } else {
        res=paste(res,">",sep="")
      }
      for (i in elemText) {
        res=paste(res,escape(nonNested[i]),sep="")
      }
   }
   for (i in nestedElements) {
           if (names(x[i])!="") {
              res=paste(res,"\n", sep="")
           }
           res=paste(res,  xmlList(x[[i]], name=names(x[i])),sep="")
   }
   if (length(nestedElements)>0 && !is.null(name) && (name!="")) {
      res=paste(res,"\n",sep="")
   }
   if (!is.null(name) &&  (name!="")) {
      if ((name == 'helpLink') || (name == 'packageDependency') || (name == 'setx')){
          
      } else {
          res=paste(res,"</",escape(name),">",sep="")
      
      }
      
   }
   return(res)
}

#
# Model Description Function for Built-In Zelig Models
#


zcheck.ls<-function(){
    res=zdescDefault("ls",
        descriptionText="Least Squares Regression for Continuous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_ls_TT__Least_Squar.html",
        outcomeTypes=c("continuous"),
        labelText="Multiple Regression"
        )
    res$model$helpLink$rhelp="lm"
    return(res)
}

zcheck.normal<-function(){
    res=zdescDefault("normal",
        descriptionText="Normal Regression for Continuous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_normal_TT__Normal.html",
        outcomeTypes=c("continuous"),
        labelText="Multiple Regression"
        )
    res$model$helpLink$rhelp="glm"
    return(res)
}

zcheck.normal.bayes<-function(){
    res=zdescDefault("normal.bayes",
        descriptionText="Bayesian Normal Linear Regression",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_normal_bayes_TT__B.html",
        labelText="Multiple Regression",
        outcomeTypes=c("continuous"),
        packages=c("MCMCpack")
        )
    return(res)
}

zcheck.tobit.bayes<-function(){
    res=zdescDefault("tobit.bayes",
        descriptionText="Bayesian Linear Regression for a Censored Dependent Variable",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_tobit_bayes_TT__Ba.html",
        labelText="Multiple Regression",
        outcomeTypes=c("continuous"),
        packages=c("MCMCpack")
        )
    return(res)
}

zcheck.logit<-function(){
    res=zdescDefault("logit",
        descriptionText="Logistic Regression for Dichotomous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_logit_TT__Logistic.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("binary")
        )
    res$model$helpLink$rhelp="glm"
    return(res)
}

zcheck.probit<-function(){
    res=zdescDefault("probit",
        descriptionText="Probit Regression for Dichotomous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_probit_TT__Probit.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("binary")
        )
    res$model$helpLink$rhelp="glm"
    return(res)
}

zcheck.bprobit<-function(){
    res=zdescDefault("bprobit",
        descriptionText="Bivariate Probit Regression for Dichotomous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_bprobit_TT__Bivari.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("binary"),
        packages=c("VGAM")
        )
    res$model$packageDependency$CRAN="http://www.stat.auckland.ac.nz/~yee"
    res$model$formula$equation$outcome$minVar="2"
    res$model$formula$equation$outcome$maxVar="2"
    res$model$specialFunction="cbind"
    return(res)
}

zcheck.blogit<-function(){
    res=zdescDefault("blogit",
        descriptionText="Bivariate Logistic Regression for Dichotomous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_blogit_TT__Bivaria.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("binary"),
        packages=c("VGAM")
        )
    res$model$packageDependency$CRAN="http://www.stat.auckland.ac.nz/~yee"
    res$model$formula$equation$outcome$minVar="2"
    res$model$formula$equation$outcome$maxVar="2"
    res$model$specialFunction="cbind"
    return(res)
}

zcheck.logit.bayes<-function(){
    res=zdescDefault("logit.bayes",
        descriptionText="Bayesian Logistic Regression for Dichotomous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_logit_bayes_TT__Ba.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("binary"),
        packages=c("MCMCpack")
        )
    return(res)
}

zcheck.probit.bayes<-function(){
    res=zdescDefault("probit.bayes",
        descriptionText="Bayesian Probit Regression for Dichotomous Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_probit_bayes_TT__B.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("binary"),
        packages=c("MCMCpack")
        )
    return(res)
}

zcheck.ologit<-function(){
    res=zdescDefault("ologit",
        descriptionText="Ordinal Logistic Regression for Ordered Categorical Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_ologit_TT__Ordinal.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("ordinal"),
        packages=c("MASS")
        )
    res$model$helpLink$rhelp="polr"
    res$model$specialFunction="as.factor"
    return(res)
}

zcheck.oprobit<-function(){
    res=zdescDefault("oprobit",
        descriptionText="Ordinal Probit Regression for Ordered Categorical Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_oprobit_TT__Ordina.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("ordinal"),
        packages=c("MASS")
        )
    res$model$helpLink$rhelp="glm"
    res$model$specialFunction="as.factor"
    return(res)
}

zcheck.oprobit.bayes<-function(){
    res=zdescDefault("oprobit.bayes",
        descriptionText="Bayesian Ordinal Probit Regression for Ordered Categorical Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_oprobit_bayes_TT.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("ordinal"),
        packages=c("MCMCpack")
        )
    res$model$specialFunction="as.factor"
    return(res)
}

zcheck.relogit<-function(){
    res=zdescDefault("relogit",
        descriptionText="Rare Events Logistic Regression for Dichotomous Dependent Variables ",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_relogit_TT__Rare_E.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("binary")
        )
    return(res)
}

zcheck.mlogit<-function(){
    res=zdescDefault("mlogit",
        descriptionText="Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_mlogit_TT__Multino.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("nominal"),
        packages=c("VGAM")
        )
    res$model$packageDependency$CRAN="http://www.stat.auckland.ac.nz/~yee"
    return(res)
}

zcheck.mlogit.bayes<-function(){
    res=zdescDefault("mlogit.bayes",
        descriptionText="Bayesian Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_mlogit_bayes_TT__B.html",
        labelText="Limited Dependent Variables Regression",
        outcomeTypes=c("nominal"),
        packages=c("MCMCpack")
        )
    return(res)
}

zcheck.factor.ord<-function(){
    res=zdescDefault("factor.ord",
        descriptionText="Ordinal Data Factor Analysis",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_factor_ord_TT__Ord.html",
        labelText="Factor Analysis",
        outcomeTypes=c("ordinal"),
        explanatoryTypes=NULL,
        packages=c("MCMCpack")
        )
    res$model$formula$equation$outcome$label<-"Observed";
    res$model$formula$equation$outcome$minVar="2"
    res$model$formula$equation$outcome$maxVar<-NULL
    res$model$formula$equation$explanatory$minVar="0"
    res$model$formula$equation$explanatory$maxVar="0"
    res$model$specialFunction="cbind"
    res$model$setx$maxSetx="0"
    return(res)
}

zcheck.factor.mix<-function(){
    res=zdescDefault("factor.mix",
        descriptionText="Mixed Data Factor Analysis",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_factor_mix_TT__Mix.html",
        labelText="Factor Analysis",
        outcomeTypes=c("ordinal","continuous"),
        explanatoryTypes=NULL,
        packages=c("MCMCpack")
        )
    res$model$formula$equation$outcome$label<-"Observed";
    res$model$formula$equation$outcome$minVar="2"
    res$model$formula$equation$outcome$maxVar<-NULL
    res$model$formula$equation$explanatory$minVar="0"
    res$model$formula$equation$explanatory$maxVar="0"
    res$model$specialFunction="cbind"
    res$model$setx$maxSetx="0"
    return(res)
}


zcheck.factor.bayes<-function(){
    res=zdescDefault("factor.bayes",
        descriptionText="Bayesian Factor Analysis",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_factor_bayes_TT__B.html",
        labelText="Factor Analysis",
        outcomeTypes=c("continuous"),
        explanatoryTypes=NULL,
        packages=c("MCMCpack")
        )
    res$model$formula$equation$outcome$label="Observed"
    res$model$formula$equation$outcome$minVar="3"
    res$model$formula$equation$outcome$maxVar<-NULL
    res$model$formula$equation$explanatory$minVar="0"
    res$model$formula$equation$explanatory$maxVar="0"
    res$model$specialFunction="cbind"
    res$model$setx$maxSetx="0"
    return(res)
}

zcheck.irt1d<-function(){
    res=zdescDefault("irt1d",
        descriptionText="One Dimensional Item Response Model",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_irt1d_TT__One_Dime.html",
        labelText="Item Response Theory",
        outcomeTypes=c("ordinal","continuous"),
        explanatoryTypes=NULL,
        packages=c("MCMCpack")
        )
    res$model$formula$equation$outcome$label<-"Observed"
    res$model$formula$equation$outcome$minVar="2"
    res$model$formula$equation$outcome$maxVar<-NULL
    res$model$formula$equation$explanatory$minVar="0"
    res$model$formula$equation$explanatory$maxVar="0"
    res$model$specialFunction="cbind"
    res$model$setx$maxSetx="0"
    return(res)
}

zcheck.irtkd<-function(){
    res=zdescDefault("irtkd",
        descriptionText="K-Dimensional Item Response Model",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_irtkd_TT___tex2htm.html",
        labelText="Item Response Theory",
        outcomeTypes=c("ordinal","continuous"),
        explanatoryTypes=NULL,
        packages=c("MCMCpack")
        )
    res$model$formula$equation$outcome$label<-"Observed"
    res$model$formula$equation$outcome$minVar="2"
    res$model$formula$equation$outcome$maxVar<-NULL
    res$model$formula$equation$explanatory$minVar="0"
    res$model$formula$equation$explanatory$maxVar="0"
    res$model$specialFunction="cbind"
    res$model$setx$maxSetx="0"
    return(res)
}

zcheck.poisson<-function(){
    res=zdescDefault("poisson",
        descriptionText="Poisson Regression for Event Count Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_poisson_TT__Poisso.html",
        labelText="Event Count Models",
        outcomeTypes=c("ordinal")
        )
    res$model$helpLink$rhelp="glm"
    return(res)
}


zcheck.negbin<-function(){
    res=zdescDefault("negbin",
        descriptionText="Negative Binomial Regression for Event Count Dependent Variables ",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_negbin_TT__Negativ.html",
        labelText="Event Count Models",
        outcomeTypes=c("ordinal"),
        packages=c("MASS")
        )
    res$model$helpLink$rhelp="glm.nb"
    return(res)
}

zcheck.poisson.bayes<-function(){
    res=zdescDefault("poisson.bayes",
        descriptionText="Bayesian Poisson Regression for Event Count Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_poisson_bayes_TT.html",
        labelText="Event Count Models",
        outcomeTypes=c("ordinal"),
        packages=c("MCMCpack")
        )
    return(res)
}

zcheck.gamma<-function(){
    res=zdescDefault("gamma",
        descriptionText="Gamma Regression for Continuous, Positive Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_gamma_TT__Gamma_Re.html",
        labelText="Duration Models"
        )
    res$model$helpLink$rhelp="glm"
    return(res)
}


zcheck.exp<-function(){
    res=zdescDefault("exp",
        descriptionText="Exponential Regression for Duration Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_exp_TT__Exponentia.html",
        labelText="Duration Models",
        packages=c("survival"),
        outcomeTypes=c("continuous"),
        mulitiOutcomes=T
        )
    res$model$helpLink$rhelp="survival"
    res$model$formula$equation$outcome$label="Duration"
    tmp=list(outcome=list(maxVar="1",minVar="0",label="Censored",modelingType=list("binary")))
    res$model$formula$equation=c(res$model$formula$equation,tmp)
    
    tmp=list(explanatory = list(minVar="1"))
    res$model$formula$equation=c(res$model$formula$equation,tmp)
    explanatoryTypes=c("continuous","ordinal","nominal")
    tmp = sapply(explanatoryTypes, function(x)list(x),simplify=F)
    names(tmp)=replicate(length(explanatoryTypes),"modelingType")
    res$model$formula$equation$explanatory=c(res$model$formula$equation$explanatory,tmp)
    res$model$specialFunction="Surv"
    
    
    return(res)
}

zcheck.weibull<-function(){
    res=zdescDefault("weibull",
        descriptionText="Weibull Regression for Duration Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_weibull_TT__Weibul.html",
        labelText="Duration Models",
        packages=c("survival"),
        outcomeTypes=c("continuous"),
        mulitiOutcomes=T
        )
    res$model$helpLink$rhelp="survival"
    res$model$formula$equation$outcome$label="Duration"
    tmp=list(outcome=list(maxVar="1",minVar="0",label="Censored",modelingType=list("binary")))
    res$model$formula$equation=c(res$model$formula$equation,tmp)
    
    tmp=list(explanatory = list(minVar="1"))
    res$model$formula$equation=c(res$model$formula$equation,tmp)
    explanatoryTypes=c("continuous","ordinal","nominal")
    tmp = sapply(explanatoryTypes, function(x)list(x),simplify=F)
    names(tmp)=replicate(length(explanatoryTypes),"modelingType")
    res$model$formula$equation$explanatory=c(res$model$formula$equation$explanatory,tmp)
    res$model$specialFunction="Surv"
    
    return(res)
}

zcheck.lognorm<-function(){
    res=zdescDefault("lognorm",
        descriptionText="Log-Normal Regression for Duration Dependent Variables",
        helpUrl="http://gking.harvard.edu/zelig/docs/_TT_lognorm_TT__Log_No.html",
        labelText="Duration Models",
        packages=c("survival"),
        outcomeTypes=c("continuous"),
        mulitiOutcomes=T
        )
    res$model$helpLink$rhelp="survival"
    res$model$formula$equation$outcome$label="Duration"
    tmp=list(outcome=list(maxVar="1",minVar="0",label="Censored",modelingType=list("binary")))
    res$model$formula$equation=c(res$model$formula$equation,tmp)
    
    tmp=list(explanatory = list(minVar="1"))
    res$model$formula$equation=c(res$model$formula$equation,tmp)
    explanatoryTypes=c("continuous","ordinal","nominal")
    tmp = sapply(explanatoryTypes, function(x)list(x),simplify=F)
    names(tmp)=replicate(length(explanatoryTypes),"modelingType")
    res$model$formula$equation$explanatory=c(res$model$formula$equation$explanatory,tmp)
    res$model$specialFunction="Surv"

    
    return(res)
}



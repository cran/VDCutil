VDCcrossTabulation<-function(data=parent.env(),classificationVars=NULL, freqVars=NULL,
  wantPercentages=T, wantTotals=T, wantStats=T, wantExtraTables=FALSE,
  HTMLfile="", ...
  ) {
  

   if (length(classificationVars)<2) {
      warning("VDCcrosstab at least two classifaction variables")
      return(NULL)
   }
   xt.formulaLft=NULL
   if (length(freqVars)>0) {
         xt.formulaLft = paste("cbind(",paste(freqVars,collapse=","),")")
   }
   if (wantExtraTables) {
      minsize = 2
   }  else  {
      minsize = length(classificationVars)
   }
   for (size in length(classificationVars):minsize) {
      xt.formula = paste(xt.formulaLft,"~",paste(classificationVars[1:size],collapse="+"),sep="")
      xt = VDCxtabs(as.formula(xt.formula),data, ...)
      HTML(xt,  wantPercentages=wantPercentages, wantTotals=wantTotals, wantStats=wantStats,
           file=HTMLfile)
      if (size>minsize) {HTML("<hr/>", file=HTMLfile)}
   }
 }


VDCxtabs<-function (formula, data = parent.env(), ...) 
{
    res = list()
    data=recodeVDCdf(data)
    res[[1]] = do.call("xtabs", list(formula = formula, data = as.name("data"), 
        ...))
    # apply VDC variable labels
    var.labels=attr(data,"var.labels")
    if (!is.null(var.labels)) {
	tmpdim=attr(res[[1]],"dimnames")
	names(tmpdim)=paste(names(tmpdim),
		var.labels[match(names(attr(res[[1]],"dimnames")),names(data))]
		,sep=": ")
	attr(res[[1]],"dimnames")=tmpdim
    }
    attr(res, "ftable") = ft = ftable(res[[1]])
    attr(res, "stats") = summary(res[[1]])
    attr(res, "rowTotals") = apply(ft, 1, sum)
    attr(res, "colTotals") = apply(ft, 2, sum)
    attr(res, "total") = sum(attr(res, "colTotals"))
    attr(res, "rowPercent") = prop.table(ft, 1)
    attr(res, "colPercent") = prop.table(ft, 2)
    class(res) = "VDCxtabs"
    return(res)
}
  
print.VDCxtabs<-function(x,...,
    wantPercentages=T, wantTotals=T, wantStats=T,nameLength=15) {
      ft = attr(x,"ftable")

      tmpat = attributes(ft)

      # handle long labels
      longnames=which(nchar(names(tmpat$col.vars))>nameLength)
      names(tmpat$col.vars)[longnames]=
        paste(substr(names(tmpat$col.vars)[longnames],1,10),"",sep="...")
      longnames=which(nchar(names(tmpat$row.vars))>nameLength)
      names(tmpat$row.vars)[longnames]=
        paste(substr(names(tmpat$row.vars)[longnames],1,10),"",sep="...")
      if (wantPercentages) {
         fmtrow = paste("(",round(attr(x,"rowPercent")*100,digits=1),"%)",sep="")
         fmtcol = paste("(",round(attr(x,"colPercent")*100,digits=1),"%)",sep="")
         ftfmt = paste(ft,fmtrow,fmtcol)
         dim(ftfmt)=dim(ft)
      }   else {
         ftfmt =   ft
      }

      if (wantTotals) {
         tmpat$dim[2]=tmpat$dim[2]+1
         tmpat$col.vars[[1]]=c(tmpat$col.vars[[1]],"Row Totals")
         ftfmt = cbind(ftfmt,attr(x,"rowTotals"))
      }

      attributes(ftfmt)=tmpat
      print(ftfmt,...)
      
      if (wantTotals) {
         m = matrix(c(attr(x,"colTotals"),attr(x,"total")),nrow=1)
         colnames(m) = attr(ftfmt,"col.vars")[[1]]
         colnames(m)[dim(m)[2]]="Grand Total"
         row.names(m)="Column Totals"
         cat("\n")
         print(m,...)
         cat("\n")
      }
      
      if (wantStats) {
         tmpc=x[[1]]; attr(tmpc,"call")=NULL
         print(summary(tmpc),...)
      }
      
      return(invisible(ft))
  }
  
  HTML.VDCxtabs<-function(x,...,
    wantPercentages=T, wantTotals=T, wantStats=T, nameLength=15) {
      ft = attr(x,"ftable")

      tmpat = attributes(ft)

      # handle long labels
      longnames=which(nchar(names(tmpat$col.vars))>nameLength)
      names(tmpat$col.vars)[longnames]=
        paste(substr(names(tmpat$col.vars)[longnames],1,10),"",sep="...")
      longnames=which(nchar(names(tmpat$row.vars))>nameLength)
      names(tmpat$row.vars)[longnames]=
        paste(substr(names(tmpat$row.vars)[longnames],1,10),"",sep="...")

      if (wantPercentages) {
         fmtrow = paste("<span class='VDCrowper'>(",round(attr(x,"rowPercent")*100,digits=1),
            "%)</span>",sep="")
         fmtcol = paste("<br/><span class='VDCrowper'>(",round(attr(x,"colPercent")*100,digits=1),
            "%)</span>",sep="")
         ftfmt = paste(ft,fmtrow,fmtcol)
         dim(ftfmt)=dim(ft)
      }   else {
         ftfmt =   ft
      }

      if (wantTotals) {
         tmpat$dim[2]=tmpat$dim[2]+1
         tmpat$col.vars[[1]]=c(tmpat$col.vars[[1]],"Row Totals")
         ftfmt = cbind(ftfmt,paste("<span class='VDCtotal'>",
            attr(x,"rowTotals"),"</span>"))
      }

      attributes(ftfmt)=tmpat
      HTML(ftfmt,...)

      if (wantTotals) {
         m = matrix(paste("<span class='VDCtotal'>",
                       c(attr(x,"colTotals"),attr(x,"total")),
                       "</span>"),
                       nrow=1)
         colnames(m) = attr(ftfmt,"col.vars")[[1]]
         colnames(m)[dim(m)[2]]="Grand Total"
         row.names(m)="Column Totals"
         HTML("<br/><br/>",...)
         HTML(m,...)
         HTML("<br/><br/>",...)
      }

      if (wantStats) {
         tmpc=x[[1]]; attr(tmpc,"call")=NULL
         HTML(summary(tmpc),...)
      }

      return(invisible(ft))
  }
  

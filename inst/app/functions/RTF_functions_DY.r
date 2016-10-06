
#*****************************************************************************************************
#-- Function to produce table or plot to RTF file
#
#-- Author: Michael Man, Duytrac Nguyen, Danni Yu
#
#*****************************************************************************************************

#-----------------------------------------------------------------------------------------------
#
###--- These functions are used for produce a nice RTF file
#
#-----------------------------------------------------------------------------------------------
if(!exists('muliHead.split')){
  muliHead.split <<- ";"
}

if (TRUE) {

  #--- produce to rtf file that as SAS output. It can be used for single table or table with multiple pages  
  rtf.table.out <- function(
    rtf,          # RTF object that is just needed to be declared once before calling the function
    tb,           # table out
    cell1=2, 
    cell2=1, 
    nheader=1,
    nline.body=40, #number of lines per page
    height=8.5, 
    width=11, 
    omi=c(1,1,1,1),
    cw=NULL,      #column width
    colFormat=c(rep("L", 1), rep("C",3)), #alignment (left, center, or right) for each column
    varName=NULL,  #variable name that want to be repeated when go to the next page
    var.ul="",     #key string in table defining the bottom line on each page.
#    titles="", 
#    footns="",
#    prd.status='QA', 
    header=FALSE,
    footer=FALSE,
    addSpaceHeader=0, #expand the width of header or footnotes
    addSpaceFoot=0,   #expand the width of header or footnotes
    page.disp=FALSE,   #whether diplay page number
#    Done=TRUE,
    ...
  ){
    # rtf: RTF object that is just needed to be declared once before calling the function
    # tb: table out
    # cw: column width
    # colFormat: alignment (left, center, or right) for each column
    # varName: variable name that want to be repeated when go to the next page
    # nline.body: number of lines per page
    # addSpaceFoot, addSpaceHeader: expand the width of header or footnotes
    # page.disp: whether diplay page number
    
    #--- generate blank pape (break page) if the data is more than a page
    if(nrow(tb) > nline.body) {
      idx <- seq(1, nrow(tb), by = nline.body)
      idx.n <- c(seq(nline.body, nrow(tb), by = nline.body), nrow(tb))
    }else {
      idx <- 1
      idx.n <- nrow(tb)
    }
    
    npage <- length(idx)
    
    for(k in 1:length(idx)) {
      if(idx[k]==idx.n[k])
        subTable <- tb
      else
        subTable <- tb[idx[k]:idx.n[k], ]
      #--- if nrow < nline.body, need adding some spaces to move footnotes to bottom of the page
      if(nrow(subTable) < nline.body) {
        addData <- data.frame(matrix("",ncol=ncol(subTable), nrow=nline.body-nrow(subTable)-1))
        colnames(addData) <- colnames(subTable)
        subTable <- rbind(subTable, addData)
      }
      
      #-- get values that will be in the 1st row of next page
      rownames(subTable) <- 1:nrow(subTable)
      
      #-- check if the last row of previous page with varName not matched the 1st row of next page
      if(k==1)
        string.val <- NULL
      if(k!=1 & !all(unique(subTable[1,varName]) %in% "") & !all(unique(subTable[1,varName]) %in% string.val))
        string.val <- unique(subTable[1,varName])
      
      if(k==1 & all(tb[min(nrow(tb), nrow(subTable)+1), varName] %in% "")) {
        for(m in 1:nrow(subTable))
          if(all(!subTable[m,varName] %in% ""))
            string.val <- subTable[m, varName]
      } else if(!all(tb[min(nrow(tb),nrow(subTable)+1), varName] %in% "")) {
        string.val <- NULL
      } else {
        #-- add last row of previous page to 1st row of next page
        if(!is.null(string.val))
          subTable[1, varName] <- string.val
        
        for(m in 1:nrow(subTable))
          if(all(!subTable[m,varName] %in% ""))
            string.val <- subTable[m, varName]
      }
      
      hd <- sapply(colnames(subTable), function(x) strsplit(x, muliHead.split)[[1]])
      ret <- rbind(hd, as.matrix(subTable))
      colnames(ret) <- 1:ncol(ret)
      rownames(ret) <- 1:nrow(ret)
      
      ret2 <- ret
      
      colnames(ret2) <- 1:ncol(ret2)
      rownames(ret2) <- 1:nrow(ret2)
      attributes(ret2) <- list(dim = dim(ret2), dimnames = dimnames(ret2), `start cell` = c(cell1, cell2))
      
      col.just <- matrix(c(rep(colFormat, nrow(ret2) + nheader)), ncol=ncol(ret2), byrow=TRUE)
      
      dat <- ret2
      st <- attributes(dat)$'start cell'[1]
      hd <- 1:(st-1)
      if(is.null(cw)) cw <- c(1.5,rep(.7,ncol(dat)-1))
      x <- dat
      cw2 <- matrix(rep(cw, nrow(x)), ncol=ncol(x), byrow=TRUE)
      if (st > 2) {
        y <- adj.width(x=dat[hd,],cw=cw, space=1)
        x[hd,] <- y$x
        cw2[hd,] <- y$cw
      }
      
      rtf.add.row <- function(rows=1:nrow(x)){
        ret <- "{\\pard\n"
        for (i in rows) if (i <= nrow(x)){
          sel <- which(cw2[i,]>0)
          ret <- paste(ret,
                       .add.table.row(
                         col.data =     x[i,sel],
                         col.widths = cw2[i,sel],
                         col.justify = col.just[i,sel],
                         font.size = fs,
                         border.top = i %in% c(1),
                         border.bottom = if (i > st-2) i %in% c(st-1,nrow(x),max(rows)) else x[i,sel] %in% var.ul
                       ),
                       sep='')
          if (i < st) if (!all(cw2[i,] == cw2[i+1,])) ret <- paste(ret,'{\\pard\\par}','', sep='')
        }
        ret <- paste(ret, "}\n\n", sep = "")
        rtf$.rtf <- paste(rtf$.rtf, ret, sep='')
      }
      
      rtf <- rtf
      fs <- rtf$.font.size
      rtf$.rtf <- gsub('Times New Roman', 'Courier New', rtf$.rtf)	# make "Courier New" as default
      rtf$.rtf <- gsub('field{\\fldinst{page}}', '', rtf$.rtf, fixed=TRUE)	# delete page number
      
      for (i in 1:1){
        rtf.add.row(rows=c(1:(st-1), 1:nline.body-1+st+nline.body*(i-1)))
      }
      
    }

    
  }#end rtf.table.out
  
  
  
  #--- this funciton to be used for single plot output or
  # when there are different plots that would be saved in the same RTF file
  rtf.plot.out <- function(rtf, plotOut, page.disp=FALSE,
                           height=8.5, width=11, omi.marg=c(1, 1, 1, 1),
                           width.plot=8, height.plot=5,
                           fs=10,
                           titles,footns,
                           prd.status="",
                           widthHeader=7, widthFoot=9,
                           breakPage=TRUE,...) {

    # rtf: RTF object that is just needed to be declared once before calling the function
    # plotOut: plot function
    # page.disp: whether diplay page number
    # width.plot, height.plot: width and height of plot
    # fs: font size for header and footnotes
    # titles,footns: title and footnotes
    # withHeader, widthFoot: width of header and footnotes
    # omi.marg: margin of the page
    # breakPage: if there are more than 1 page of the RTF file, need breakPage=TRUE,
    #            otherwise (just a single page), set breakPage=FALSE

    rtf <- rtf

    rtf$.font.size <- fs
    rtf$.rtf <- gsub('Times New Roman', 'Courier New', rtf$.rtf)  # make "Courier New" as default


    npage <- 1
#    systems <- c(ifelse(page.disp==TRUE, paste('Page \\chpgn  of ', npage, sep=''), ""),
#                 format(Sys.time(), "%H:%M %d%b%Y"),
#                 prd.status)
#    hd.m <- c(paste(titles,  collapse='\\line '),
#              paste(systems, collapse='\\line '))  # main header

#    tmp <- .add.table.row(
#      col.data =  hd.m,
#      col.widths = c(widthHeader,2),
#      col.justify = c('L', 'R'),
#      font.size = fs)

#    tmp2 <- .add.table.row(
#      col.data =  paste(footns, collapse='\\line '),
#      col.widths = widthFoot,
#      col.justify = 'L',
#      font.size = fs)

    #-- add headers
    #rtf$.rtf <- paste(rtf$.rtf,  '{\\pard\\par}', tmp,  '{\\pard\\par}', sep='')

    #-- add plot
    addPlot(rtf, plot.fun=plotOut, width=width.plot, height=height.plot, res=300)   #add plot

    #-- add footnotes
    #addParagraph(rtf, paste(footns,  collapse='\\line '))
    #rtf$.rtf <- paste(rtf$.rtf,  '{\\pard\\par}', tmp2,  '{\\pard\\par}', sep='')

    if(breakPage)
      addPageBreak(rtf, width=width, height=height, omi=omi.marg)
  }#end rtf.plot.out


}#end RTF output functions


#-----------------------------------------------------------------------------------------------
#
###--- These functions are original developed/modified from Michael Man
# in which he used RTF syntax (from tables and etable packages) rather than the RTF package.
#
#-----------------------------------------------------------------------------------------------

if (TRUE) {

  .convert <- function (x)
  {
    x <- gsub("\\n", " \\\\line ", x)
    x <- gsub("<=", "\\\\u8804\\\\3", x)
    x <- gsub(">=", "\\\\u8805\\\\3", x)
    x <- gsub("&gt;", ">", x)
    x <- gsub("&lt;", "<", x)
    x <- gsub("&Alpha;", "\\\\u0913\\\\3", x)
    x <- gsub("&Beta;", "\\\\u0914\\\\3", x)
    x <- gsub("&Gamma;", "\\\\u0915\\\\3", x)
    x <- gsub("&Delta;", "\\\\u0916\\\\3", x)
    x <- gsub("&Epsilon;", "\\\\u0917\\\\3", x)
    x <- gsub("&Zeta;", "\\\\u0918\\\\3", x)
    x <- gsub("&Eta;", "\\\\u0919\\\\3", x)
    x <- gsub("&Theta;", "\\\\u0920\\\\3", x)
    x <- gsub("&Iota;", "\\\\u0921\\\\3", x)
    x <- gsub("&Kappa;", "\\\\u0922\\\\3", x)
    x <- gsub("&Lambda;", "\\\\u0923\\\\3", x)
    x <- gsub("&Mu;", "\\\\u0924\\\\3", x)
    x <- gsub("&Nu;", "\\\\u0925\\\\3", x)
    x <- gsub("&Xi;", "\\\\u0926\\\\3", x)
    x <- gsub("&Omicron;", "\\\\u0927\\\\3", x)
    x <- gsub("&Pi;", "\\\\u0928\\\\3", x)
    x <- gsub("&Rho;", "\\\\u0929\\\\3", x)
    x <- gsub("&Sigma;", "\\\\u0931\\\\3", x)
    x <- gsub("&Tau;", "\\\\u0932\\\\3", x)
    x <- gsub("&Upsilon;", "\\\\u0933\\\\3", x)
    x <- gsub("&Phi;", "\\\\u0934\\\\3", x)
    x <- gsub("&Chi;", "\\\\u0935\\\\3", x)
    x <- gsub("&Psi;", "\\\\u0936\\\\3", x)
    x <- gsub("&Omega;", "\\\\u0937\\\\3", x)
    x <- gsub("&alpha;", "\\\\u0945\\\\3", x)
    x <- gsub("&beta;", "\\\\u0946\\\\3", x)
    x <- gsub("&gamma;", "\\\\u0947\\\\3", x)
    x <- gsub("&delta;", "\\\\u0948\\\\3", x)
    x <- gsub("&epsilon;", "\\\\u0949\\\\3", x)
    x <- gsub("&zeta;", "\\\\u0950\\\\3", x)
    x <- gsub("&eta;", "\\\\u0951\\\\3", x)
    x <- gsub("&theta;", "\\\\u0952\\\\3", x)
    x <- gsub("&iota;", "\\\\u0953\\\\3", x)
    x <- gsub("&kappa;", "\\\\u0954\\\\3", x)
    x <- gsub("&lambda;", "\\\\u0955\\\\3", x)
    x <- gsub("&mu;", "\\\\u0956\\\\3", x)
    x <- gsub("&nu;", "\\\\u0957\\\\3", x)
    x <- gsub("&xi;", "\\\\u0958\\\\3", x)
    x <- gsub("&omicron;", "\\\\u0959\\\\3", x)
    x <- gsub("&pi;", "\\\\u0960\\\\3", x)
    x <- gsub("&rho;", "\\\\u0961\\\\3", x)
    x <- gsub("&sigmaf;", "\\\\u0962\\\\3", x)
    x <- gsub("&sigma;", "\\\\u0963\\\\3", x)
    x <- gsub("&tau;", "\\\\u0964\\\\3", x)
    x <- gsub("&upsilon;", "\\\\u0965\\\\3", x)
    x <- gsub("&phi;", "\\\\u0966\\\\3", x)
    x <- gsub("&chi;", "\\\\u0967\\\\3", x)
    x <- gsub("&psi;", "\\\\u0968\\\\3", x)
    x <- gsub("&omega;", "\\\\u0969\\\\3", x)
    x <- gsub("TRUE", "Yes", x)
    x <- gsub("FALSE", "No", x)
    x
  }


  .get.space.before.after <- function (space.before = NULL, space.after = NULL)
  {
    ret <- ""
    if (!is.null(space.before)) {
      ret <- paste(ret, "\\sb", (space.before * 1440), sep = "")
    }
    if (!is.null(space.after)) {
      ret <- paste(ret, "\\sa", (space.after * 1440), sep = "")
    }
    ret
  }
 

  .add.table <- function (dat, col.widths = NULL, col.justify = NULL, header.col.justify = NULL,
                          font.size = 10, row.names = FALSE, indent = 0, NA.string = "-",
                          bd.top=NULL, bd.bottom=NULL,  # MZM: border option, need row numbers
                          max.table.width = NULL, space.before = NULL, space.after = NULL)
  {
    ret <- "{\\pard\n"
    if (!is.null(bd.top) | !is.null(bd.bottom)) {  #MZM: for complex table
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
      nc <- ncol(dat)
      if (is.null(col.widths)) col.widths <- rep(6.5/nc, nc)
      for (i in 1:nrow(dat)) {
        ret <- paste(ret, .add.table.row(as.character(dat[i, ]),
                                         col.widths, col.justify, font.size = font.size,
                                         last.row = FALSE, indent = indent, space.before = space.before, # MZM: change last.row
                                         border.top = i %in% bd.top,  border.bottom = i %in% bd.bottom,  # MZM: add top/bottom option
                                         space.after = space.after), sep = "")
      }
    }
    else if ("table" %in% class(dat)) {
      if (length(dim(dat)) == 1) {
        varnames <- names(dimnames(dat))[1]
        nc <- 2
        nr <- length(dimnames(dat)[[1]])
        if (is.null(col.widths)) {
          col.widths <- rep(6.5/nc, nc)
        }
        ret <- paste(ret, .add.table.header.row(c(names(dimnames(dat))[1],
                                                  " "), col.widths, header.col.justify, font.size = font.size,
                                                repeat.header = TRUE, indent = indent), sep = "")
        if (nrow(dat) > 1) {
          for (i in 1:(nrow(dat) - 1)) {
            rn <- rownames(dat)[i]
            ret <- paste(ret, .add.table.row(c(rn, as.character(dat[i])),
                                             col.widths, col.justify, font.size = font.size,
                                             indent = indent, space.before = space.before,
                                             space.after = space.after), sep = "")
          }
        }
        rn <- rownames(dat)[nrow(dat)]
        ret <- paste(ret, .add.table.row(c(rn, as.character(dat[nrow(dat)])),
                                         col.widths, col.justify, font.size = font.size,
                                         indent = indent, border.bottom = TRUE, space.before = space.before,
                                         space.after = space.after), sep = "")
      }
      else if (length(dim(dat)) == 2) {
        varnames <- names(dimnames(dat))
        nc <- ncol(dat) + 1
        nr <- nrow(dat)
        if (is.null(col.widths)) {
          col.widths <- rep(6.5/nc, nc)
        }
        ret <- paste(ret, .add.merged.table.row(c(" ", paste("\\b ",
                                                             varnames[2], " \\b0", sep = ""), rep(" ", nc -
                                                                                                    2)), col.widths, font.size = font.size, indent = indent,
                                                border.top = TRUE), sep = "")
        ret <- paste(ret, .add.table.row(c(paste("\\b ",
                                                 varnames[1], " \\b0", sep = ""), colnames(dat)),
                                         col.widths, col.justify, font.size = font.size,
                                         indent = indent, border.bottom = TRUE), sep = "")
        if (nrow(dat) > 1) {
          for (i in 1:(nrow(dat) - 1)) {
            rn <- rownames(dat)[i]
            ret <- paste(ret, .add.table.row(c(rn, as.character(dat[i,
                                                                    ])), col.widths, col.justify, font.size = font.size,
                                             indent = indent, space.before = space.before,
                                             space.after = space.after), sep = "")
          }
        }
        rn <- rownames(dat)[nrow(dat)]
        ret <- paste(ret, .add.table.row(c(rn, as.character(dat[nrow(dat),
                                                                ])), col.widths, col.justify, font.size = font.size,
                                         indent = indent, border.bottom = TRUE, space.before = space.before,
                                         space.after = space.after), sep = "")
      }
      else {
        stop("Table dimensions can't be written")
      }
    }
    else if ("xtab" %in% class(dat)) {
      nc <- ncol(dat$counts) + 2
      nr <- nrow(dat$counts)
      if (is.null(col.widths)) {
        col.widths <- rep(6.5/nc, nc)
      }
      ret <- paste(ret, .add.merged.table.row(c(" ", paste("\\b ",
                                                           dat$varnames[2], " \\b0", sep = ""), rep(" ", nc -
                                                                                                      2)), col.widths, font.size = font.size, indent = indent,
                                              border.top = TRUE), sep = "")
      ret <- paste(ret, .add.table.row(c(paste("\\b ", dat$varnames[1],
                                               " \\b0", sep = ""), colnames(dat$counts), "Total"),
                                       col.widths, col.justify, font.size = font.size, indent = indent,
                                       border.bottom = TRUE), sep = "")
      grand.total <- sum(dat$col.margin)
      if (nrow(dat$counts) > 1) {
        for (i in 1:(nrow(dat$counts))) {
          rn <- rownames(dat$counts)[i]
          ret <- paste(ret, .add.table.row(c(rn, as.character(dat$counts[i,
                                                                         ]), paste(dat$row.margin[i], " (", sprintf("%0.1f",
                                                                                                                    dat$row.margin[i]/grand.total * 100), "%)",
                                                                                   sep = "")), col.widths, col.justify, font.size = font.size,
                                           indent = indent, space.before = space.before,
                                           space.after = space.after), sep = "")
          ret <- paste(ret, .add.table.row(c(" ", paste("(",
                                                        sprintf("%0.1f", dat$counts[i, ]/dat$row.margin[i] *
                                                                  100), "% R)", sep = ""), " "), col.widths,
                                           col.justify, font.size = font.size, indent = indent,
                                           space.before = space.before, space.after = space.after),
                       sep = "")
          ret <- paste(ret, .add.table.row(c(" ", paste("(",
                                                        sprintf("%0.1f", dat$counts[i, ]/dat$col.margin *
                                                                  100), "% C)", sep = ""), " "), col.widths,
                                           col.justify, font.size = font.size, indent = indent,
                                           space.before = space.before, space.after = space.after),
                       sep = "")
          ret <- paste(ret, .add.table.row(rep(" ", nc),
                                           col.widths, col.justify, font.size = font.size,
                                           indent = indent, space.before = space.before,
                                           space.after = space.after), sep = "")
        }
      }
      ret <- paste(ret, .add.table.row(c("Total", paste(as.character(dat$col.margin),
                                                        paste(" (", sprintf("%0.1f", dat$col.margin/grand.total *
                                                                              100), "%)", sep = "")), as.character(grand.total)),
                                       col.widths, font.size = font.size, last.row = TRUE,
                                       indent = indent, space.before = space.before, space.after = space.after),
                   sep = "")
    }
    else if ("matrix" %in% class(dat) & !is.null(attributes(dat)$"start cell")) {
      start.row <- attributes(dat)$"start cell"[1]
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
      if (is.null(col.widths) & !is.null(max.table.width)) {
        col.widths <- .optimize.col.widths(dat, include.row.names = row.names,
                                           max.table.width = max.table.width, font.size = font.size)
      }
      nc <- ncol(dat)
      if (is.null(col.widths)) {
        col.widths <- rep(6.5/nc, nc)
      }
      if (nrow(dat) > 1) {
        for (i in 1:(nrow(dat) - 1)) {
          if (i < start.row) {
            border.top = FALSE
            border.bottom = FALSE
            if (i == 1) {
              border.top = TRUE
            }
            if (i == (start.row - 1)) {
              border.bottom = TRUE
            }
            ret <- paste(ret, .add.table.row(paste("\\b ",
                                                   as.character(dat[i, ]), " \\b0", sep = ""),
                                             col.widths, col.justify, font.size = font.size,
                                             indent = indent, border.top = border.top,
                                             border.bottom = border.bottom, space.before = space.before,
                                             space.after = space.after), sep = "")
          }
          else {
            ret <- paste(ret, .add.table.row(as.character(dat[i,
                                                              ]), col.widths, col.justify, font.size = font.size,
                                             indent = indent, space.before = space.before,
                                             space.after = space.after), sep = "")
          }
        }
        ret <- paste(ret, .add.table.row(as.character(dat[nrow(dat),
                                                          ]), col.widths, col.justify, font.size = font.size,
                                         last.row = TRUE, indent = indent, space.before = space.before,
                                         space.after = space.after), sep = "")
      }
    }
    else if ("data.frame" %in% class(dat) || "matrix" %in% class(dat)) {
      if ("matrix" %in% class(dat)) {
        dat <- as.data.frame(dat)
      }
      rnames <- rownames(dat)
      is.na(dat) <- is.na(dat)
      dat <- data.frame(lapply(dat, as.character), stringsAsFactors = FALSE,
                        check.names = FALSE)
      dat[is.na(dat)] <- NA.string
      dat[dat == "NA"] <- NA.string
      rownames(dat) <- rnames
      if (is.null(col.widths) & !is.null(max.table.width)) {
        col.widths <- .optimize.col.widths(dat, include.row.names = row.names,
                                           max.table.width = max.table.width, font.size = font.size)
      }
      nc <- ncol(dat)
      if (row.names == TRUE) {
        nc <- nc + 1
      }
      if (is.null(col.widths)) {
        col.widths <- rep(6.5/nc, nc)
      }
      if (row.names == TRUE) {
        ret <- paste(ret, .add.table.header.row(c(" ", colnames(dat)),
                                                col.widths, header.col.justify, font.size = font.size,
                                                repeat.header = TRUE, indent = indent), sep = "")
      }
      else {
        ret <- paste(ret, .add.table.header.row(colnames(dat),
                                                col.widths, header.col.justify, font.size = font.size,
                                                repeat.header = TRUE, indent = indent), sep = "")
      }
      if (nrow(dat) > 1) {
        for (i in 1:(nrow(dat) - 1)) {
          if (row.names == TRUE) {
            rn <- rownames(dat)[i]
            ret <- paste(ret, .add.table.row(c(rn, as.character(dat[i,
                                                                    ])), col.widths, col.justify, font.size = font.size,
                                             indent = indent, space.before = space.before,
                                             space.after = space.after), sep = "")
          }
          else {
            ret <- paste(ret, .add.table.row(as.character(dat[i,
                                                              ]), col.widths, col.justify, font.size = font.size,
                                             indent = indent, space.before = space.before,
                                             space.after = space.after), sep = "")
          }
        }
      }
      if (row.names == TRUE) {
        rn <- rownames(dat)[nrow(dat)]
        ret <- paste(ret, .add.table.row(c(rn, as.character(dat[nrow(dat),
                                                                ])), col.widths, col.justify, font.size = font.size,
                                         last.row = TRUE, indent = indent, space.before = space.before,
                                         space.after = space.after), sep = "")
      }
      else {
        ret <- paste(ret, .add.table.row(as.character(dat[nrow(dat),
                                                          ]), col.widths, col.justify, font.size = font.size,
                                         last.row = TRUE, indent = indent, space.before = space.before,
                                         space.after = space.after), sep = "")
      }
    }
    else {
      warning("No suitable RTF converter for object class!")
    }
    ret <- paste(ret, "}\n\n", sep = "")
    ret
  }
 
  .add.table.row <- function (col.data = c("c1", "c2", "c3"), col.widths = c(1, 4.5,
                                                                             1), col.justify = NULL, font.size = 10, last.row = FALSE,
                              indent = 0, border.top = FALSE, border.bottom = FALSE, space.before = NULL,
                              space.after = NULL)
  {
    header <- paste("\\trowd\\trgaph100\\trleft", indent, sep = "")
    if (length(col.data) != length(col.widths)) {
      stop(paste("The number of data columns (", length(col.data),
                 ") doesn't match the column widths (", length(col.widths),
                 ")!  Input data: ", col.data, sep = ""))
    }
    justify <- vector()
    justify["L"] <- "\\ql"
    justify["R"] <- "\\qr"
    justify["C"] <- "\\qc"
    justify["J"] <- "\\qj"
    justify.v <- rep(justify["L"], length(col.data))
    numeric.cols <- which(!is.na(suppressWarnings(as.numeric(col.data))))
    if (length(numeric.cols) > 0) {
      justify.v[numeric.cols] <- justify["R"]
    }
    if (!is.null(col.justify)) {
      if (length(col.justify) == 1) {
        if (col.justify %in% names(justify)) {
          justify.v <- rep(justify[col.justify], length(col.data))
        }
        else {
          stop(paste("col.justify parameter not recognized: ",
                     col.justify, " (should be L, R, C, or J)",
                     sep = ""))
        }
      }
      else if (length(col.justify) == length(col.data)) {
        justify.v <- justify[col.justify]
      }
      else {
        stop(paste("The number of data columns (", length(col.data),
                   ") doesn't match the col.justify (", length(col.justify),
                   ") parameter!  Input data: ", paste(col.data,
                                                       sep = "", collapse = ", "), sep = ""))
      }
    }
    btop <- ""
    bbottom <- ""
    if (any(border.top))  # MZM
      btop <- sapply(border.top, ifelse, "\\clbrdrt\\brdrs\\brdrw15",'') #MZM: make border.top as a vector option
    if (last.row == TRUE | any(border.bottom))  #MZM
      bbottom <- sapply(border.bottom, ifelse, "\\clbrdrb\\brdrs\\brdrw15",'') #MZM: make border.bottom as a vector option
    cols.prefix <- paste("\\clvertalt\\clshdrawnil\\clwWidth",
                         round(col.widths * 1440, 0), "\\clftsWidth3\\clheight260\\clpadl100\\clpadr100\\gaph",
                         btop, bbottom, "\\cellx", c(1:length(col.widths)), "\n",
                         sep = "", collapse = "")
    cols <- paste("\\pard", justify.v, .get.space.before.after(space.before,
                                                               space.after), "\\widctlpar\\intbl\\fi0\\f2\\fs", font.size *
                    2, " ", .convert(col.data), "\\cell\n", sep = "", collapse = "")
    end.row <- "\\widctlpar\\intbl\\row\n"
    paste(header, cols.prefix, cols, end.row, sep = "")
  }
 

  # add width basing on duplication
  adj.width <- function(x, cw, space=1){
    ret <- array(0, dim=dim(x))
    x2 <- x
    dup <- t(apply(x,1, function(x) c(FALSE, unlist(x[-1])==unlist(x[-length(x)]))))
    for (i in 1:nrow(x)) {
      for (j in ncol(x):1) {
        if (i > 1 & j <= ncol(x)) if (!dup[i-1,j]) dup[i,j] <- FALSE
        if (dup[i,j]) {
          ret[i,j-1] <- ret[i,j]+space*cw[j]
          ret[i,j  ] <-         -space*cw[j]
          x2[i,j] <- ''
        }
      }
      ret[i,] <- ret[i,]+cw
    }
    list(x=x2, cw=ret)
  }#end

}#end mist functions


if (TRUE) {
  #-----------------------------------------------------------------------------------------------
  #
  ###--- These functions are used for formatting dataframe, before produce it to RTF file
  #
  #-----------------------------------------------------------------------------------------------
  # cat the expression and its result.
  cat.exp <- function(x, start='\n----', sep2=ifelse(Print, '\n', ': '), end='\n',
                      Print=is.na(match(class(eval(x)), c('integer', 'numeric', 'character')))) {
    cat(start, deparse(substitute(x)), sep2)
    if (!Print) cat(eval(x),end) else print(eval(x))
  }#end
  
  
  # Function to add blank line between item (record) from table output.
  add.blank.line.between <- function(dat, index_blank) {
    #--- adding blank lines between subjects
    dat2 <- NULL
    for(k in 1:length(index_blank)) {
      if(k==1) {
        if(index_blank[k]==index_blank[k+1]-1) {
          tmp <- rbind(dat[1:index_blank[k], ], "")
        } else {
          tmp <- rbind(dat[(index_blank[k]:(index_blank[k+1]-1)), ], "")
        }
      } else if(k==length(index_blank)) {
        tmp <- rbind(dat[(index_blank[k]:nrow(dat)), ], "")
      } else {
        tmp <- rbind(dat[(index_blank[k]:(index_blank[k+1]-1)), ], "")
      }
      if(is.null(dat2)) dat2 <- tmp else dat2 <- rbind(dat2, tmp)
    }
    dat_with_blank <- dat2
    
    return(dat_with_blank)
  }#end
  
  add_blank_line <- function(dat, index_blank) {
    #--- adding blank lines into data
    dat2 <- NULL
    for(k in 1:length(index_blank)) {
      if(k==1) {
        tmp <- rbind(dat[1:index_blank[k], ], "")
      } else {
        tmp <- rbind(dat[(index_blank[k-1]+1):index_blank[k], ], "")
      } 
      
      if(is.null(dat2)) dat2 <- tmp else dat2 <- rbind(dat2, tmp)
    }
    dat_with_blank <- rbind(dat2, dat[(index_blank[k]+1):nrow(dat), ])  # add the last part
    
    return(dat_with_blank)
  }#end
  
  # format numbers before outputing
  format.num <- function(x, num.sigf=2, width=1, num.space=1, trun=FALSE) {
    num <- formatC(format(round(x, num.sigf), nsmall=num.sigf), 
                   width=width, flag=paste(rep("", num.space), collapse=" "))
    if(trun==TRUE) {
      num <- gsub(" ", "", num)
    }
    return(num)
  }#end format_num
  
  
  sep.space <- function(n) {
    paste(rep(" ", times=n), collapse="")
  }#end
  
  
  #--- exclude col with all NA value
  ex.col <- function(dat, excluded=c(NA, "")) {
    for(i in colnames(dat))
      if(all(dat[,i] %in% excluded)) {
        print(i)
        dat <- dat[,-which(names(dat) %in% i)]
      }
    dat
  }
  
  
  #--- excluded row with all NA value
  ex.row <- function(dat, na.str=c(NA, "NA", "", " ", ".")) {
    ex.r <- NULL
    for(i in 1:nrow(dat)) {
      x <- dat[i,]
      if(all(x %in% na.str))
        ex.r <- c(ex.r, i)
    }
    if(!is.null(ex.r))
      ret <- dat[-ex.r,] else
        ret <- dat
      return(ret)
  }#end
  
  
  #--- function to make the 1st letter of a word to uppercase
  simpleCap <- function(x) {
    x2 <- tolower(x)
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x2, perl=TRUE)
  }#end
  
  
  # reorder levels 
  level.order <- function(x, index=c(1,2,3)) {
    y <- factor(x)
    levels(y) <- index
    y <- as.numeric(as.character(y))
    return(y)
  }#end
  
  
  # remove duplcate. For example, a ID has multiple rows, just displaying
  # the ID in the 1st row, the rest is empty
  removed.dup <- function(dat, dupBy=NULL, varName=NULL) {
    id.dup <- which(duplicated(dat[,dupBy]))
    dat[id.dup, varName] <- ""
    return(dat)
  }#end
  
  
  #--- function to replace value
  replace.val <- function(x, replaceVal=NA, byVal=NULL) {
    id <- which(x %in% replaceVal)
    x[id] <- byVal
    x
  }#end replace_val
  
  
  # some cases needed to convert NA to zero
  convert.NA.to.zero <- function(dat) {
    check.NA <- function(x, width=2) {
      if(any(is.na(x)))
        x[x%in%NA] <- format(0, justify='right', width=width)
      x
    }
    
    tmp <- apply(dat, 2, check_NA)
    ret <- data.frame(tmp)
    return(ret)
  }#end
  
  
  # Function to Generate rtf table column widths
  col.width <- function(tb){  
    cws <- NULL
    for(i in 1:length(names(tb))) {
      ncName <- nchar(names(tb)[i])
      ncString <- max(nchar(tb[,i]))
      nc <- max(ncName, ncString)
      cw <- nc*0.89/10                 # assume 10 characters per an inch
      cws <- c(cws, cw)
    }
    return(cws)
  }#end
  
  
  # Fill missing/blank as NA
  fill.missing <- function(x) {
    x2 <- x
    if(any(x2 %in% c(NA, "", ".", " "))) {
      x2[which(x2 %in% c(NA, "", ".", " "))] <- NA
    }
    # make sure class of x would be the same as its original class
    class(x2) <- class(x)
    return(x2)
  }#end
  
  
  # make two data have the same class()
  # apply for original and derived data
  class.data <- function(dat1, dat2) {
    # dat1: original
    # dat2: derived
    if(ncol(dat1) != ncol(dat2))
      stop
    for(i in 1:ncol(dat1)) {
      class(dat2[,i]) <- class(dat1[,i])
    }
    return(dat2)
  }#end
  
  # function to map Lilly color with chart R color
  color.code <- function(str.color) {
    colz <- NULL
    for(i in 1:length(str.color)) {
      colz[i] <- str.color[i]
      if(str.color[i] %in% "red")     {colz[i] <- '#D52B1E'} 
      if(str.color[i] %in% "blue")    {colz[i] <- '#00A1DE'}  
      if(str.color[i] %in% "darkblue")    {colz[i] <- '#263F6A'} 
      if(str.color[i] %in% "green")   {colz[i] <- '#00AF3F'}  
      if(str.color[i] %in% "lightgreen")  {colz[i] <- '#C2EDCE'}
      if(str.color[i] %in% "darkgreen")   {colz[i] <- '#275E37'}
      if(str.color[i] %in% "yellow")  {colz[i] <- '#FED100'}
      if(str.color[i] %in% "orange")  {colz[i] <- '#FF6D22'}
      if(str.color[i] %in% c('grey', 'gray'))           {colz[i] <- '#A59D95'}
      if(str.color[i] %in% c('lightgrey', 'lightgray')) {colz[i] <- '#D5D2CA'}
      if(str.color[i] %in% c('darkgrey', 'darkgray')) {colz[i] <- '#82786F'}
      if(str.color[i] %in% "brown")    {colz[i] <- '#4E2E2D'}
      
    }#end
    return(colz)
  }#end
  
  stat_summary <- function(xval, quantile=FALSE) {  
    # xval: a numeric vector
    summ <- summary(xval)
    SD <- sd(xval, na.rm=T) 
    if(quantile) {
      result <- c(summ[names(summ)%in%'Mean'], 
                  SD, 
                  summ[names(summ)%in%'1st Qu.'],
                  summ[names(summ)%in%'Median'], 
                  summ[names(summ)%in%'3rd Qu.'],
                  summ[names(summ)%in%'Min.'], 
                  summ[names(summ)%in%'Max.'])
    } else {
      result <- c(summ[names(summ)%in%'Mean'], 
                  SD, 
                  summ[names(summ)%in%'Median'], 
                  summ[names(summ)%in%'Min.'], 
                  summ[names(summ)%in%'Max.'])
    }
    return(result)
  }#end stat_summary
  
}#end

#------------------------------------------------------------------------------------------------










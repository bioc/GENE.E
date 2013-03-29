library(h5r)
library(RCurl)
	
write.gctx <- function(mdat, row.annotations=NULL, column.annotations=NULL, write.rownames=T, write.colnames=T, row.hclust=NULL, column.hclust=NULL, file) {
	if(!is.null(row.hclust)) {
		mdat <- mdat[row.hclust$order,]
		if(!is.null(row.annotations)) {
			 row.annotations <- row.annotations[row.hclust$order,]
		}
	}
	if(!is.null(column.hclust)) {
		mdat <- mdat[,column.hclust$order]
		if(!is.null(column.annotations)) {
			 column.annotations <- column.annotations[column.hclust$order,]
		}
	}
	mdat <- t(mdat)
	h5 <- H5File(file, 'w')
	zero <- createH5Group(h5, '0')
	
	createH5Dataset(createH5Group(createH5Group(zero, 'DATA'), '0'), "matrix", mdat, overwrite=T)
	
	meta <- createH5Group(zero, 'META')
	col <- createH5Group(meta, 'COL')
	row <- createH5Group(meta, 'ROW')
	if(!is.null(row.hclust)) {
		id <- paste (0:(ncol(mdat)-1),'X',sep='')
		createH5Dataset(row, 'gene id', id[row.hclust$order], overwrite=T) 
		r2atr(h5, row.hclust, TRUE)
	}
	if(!is.null(column.hclust)) {
		id <- paste (0:(nrow(mdat)-1),'X',sep='')
		createH5Dataset(col, 'array id', id[column.hclust$order], overwrite=T) 
		r2atr(h5, column.hclust, FALSE)
	}
	if(!is.null(row.annotations) || !is.null(colnames(mdat))) {
		write.meta(row, colnames(mdat), row.annotations, write.rownames)
	}
	if(!is.null(column.annotations) || !is.null(rownames(mdat))) {
		write.meta(col, rownames(mdat), column.annotations, write.colnames)
	}
	
	
	rm(zero) 
	rm(meta) 
	rm(col) 
	rm(row) 
	rm(h5) 
	gc()
}

write.meta <- function(node, n, annotations, write.names) {
	if(!is.null(n) && write.names) {
		createH5Dataset(node, 'Name', n, overwrite=T) # row or column names
	}
	if(!is.null(annotations)) {
		names <- colnames(annotations)
		number.of.names <- length(names)
		if(number.of.names > 0) {
			for(i in 1:number.of.names) {
				name = names[i]
				v <- annotations[[i]]
				if(class(v) == 'factor') {
					v <- as.character(v)
				}
				createH5Dataset(node, name, v, overwrite=T)
			}
		}
	}
}

read.gctx = function (src, matrix=TRUE) {
	f <-  H5File(src)
	mat <-  NULL
	if(matrix) {
		mat <-  getH5Dataset(getH5Group(f, "0/DATA/0"), "matrix")
		mat <-  t(as.matrix(mat[][]))
	}	
	row.meta <-  read.meta(f, "0/META/ROW")
	column.meta <- read.meta(f, "0/META/COL")
	rm(f)
	gc()
	return(list(row.annotations=row.meta, column.annotations=column.meta, matrix=mat))
}

#read.hclust = function (f, datasetName) {
#    group <- getH5Group(f, '/0/DATA/')
#	data <- getH5Dataset(group, datasetName)   
#	con <- textConnection(data, encoding='bytes')
#	dend <- xcluster2r(con)
#	close(con)
#}

read.meta = function (f, path) {
	meta <-  getH5Group(f, path)
	contents <-  listH5Contents(meta)
    fields <-  names(contents)
	fields <-  fields[fields != "."]
    i <-  1;
    meta.data = NULL
    for ( field in fields ) {
    	data <-  getH5Dataset(meta, field)
    	if(i == 1) {
    		meta.data <-  data.frame(matrix(nrow = length(data), ncol = length(fields)))
 			colnames(meta.data) <-  fields
    	}
 		meta.data[,i] <- data[]
 		i = i+1
    }	
    return(meta.data)
}

r2atr <- function(h5, hc, rows)
{
  height <- hc$height
  n <- length(height)
  node <- 1:n
  node <- paste ('NODE',node,'X',sep='')

  merge1  <- hc$merge[,1]
  merge11 <- paste ('NODE',merge1,'X',sep='')
  merge12 <- paste ( -1-merge1,'X',sep='')
  merge1[hc$merge[,1]>0] <- merge11[hc$merge[,1]>0]
  merge1[hc$merge[,1]<0] <- merge12[hc$merge[,1]<0]
  
  merge2  <- hc$merge[,2]
  merge11 <- paste ('NODE',merge2,'X',sep='')
  merge12 <- paste (-1-merge2,'X',sep='')
  merge2[hc$merge[,2]>0] <- merge11[hc$merge[,2]>0]
  merge2[hc$merge[,2]<0] <- merge12[hc$merge[,2]<0]

  if(rows) {
  	 n <- createH5Group(h5, '/0/DATA/row_dendrogram')
  } else {
  	 n <- createH5Group(h5, '/0/DATA/column_dendrogram')
  }
 
  createH5Dataset(n, 'id', node, overwrite=T)
  createH5Dataset(n, 'left', merge1, overwrite=T)
  createH5Dataset(n, 'right', merge2, overwrite=T)
  createH5Dataset(n, 'distance', height, overwrite=T) 
}

from.genee <- function(url='http://localhost:9998') {
    url <- get.genee.url(url)
	f <- tempfile()
	download.file(paste(url, 'api/from', sep='/'), f, mode = "wb", cacheOK=F)
	on.exit(unlink(f))
 	m <- read.gctx(f)
	return(m)
}


to.genee <- function (mdat, row.annotations=NULL, column.annotations=NULL, show.rownames=T, show.colnames=T, row.hclust=NULL, column.hclust=NULL, url='http://localhost:9998' ) {
    url <- get.genee.url(url)
	name <- deparse(substitute(mdat))
	dir = tempdir()
	if(file.exists('/tmp/shm') && file.access('/tmp/shm', mode=2) == 0) {
		dir = '/tmp/shm'
	}
	f <- tempfile(pattern = "file", tmpdir = dir, fileext = ".gctx")
	on.exit(unlink(f))
	write.gctx(mdat, row.annotations, column.annotations, show.rownames, show.colnames, row.hclust, column.hclust, f)
	#curl_setopt($curl, CURLOPT_HTTPHEADER, array('Expect:'));
	x <-postForm(paste(url, 'api/to', sep='/'), binary=T, "fileName"=name, "fileData" = fileUpload(f,  contentType = 'application/gctx'), .opts = list(verbose = FALSE, header = TRUE))	
}


get.genee.url <- function(url)
{
    if (nchar(Sys.getenv("GENE_E_URL")))
        Sys.getenv("GENE_E_URL")
    else
        url
}




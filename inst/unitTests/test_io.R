z = matrix(rnorm(30),nrow=5,ncol=6);
row.names(z) <- LETTERS[1:NROW(z)];
colnames(z) <- LETTERS[1:NCOL(z)];
row.annotations <- data.frame(y=1:5, char = I(letters[1:5]))
L3 <- LETTERS[1:3]  
column.annotations <- data.frame(y=1:6, char = I(letters[1:6]), fac=sample(L3, 6, replace=TRUE))
to.genee(z, row.annotations, column.annotations);
Sys.sleep(5)
fz = from.genee();
	
test_matrix <- function() {
	checkEqualsNumeric(fz$matrix, z, tolerance=1.0e-7)
}

test_columns <- function() {
	checkEquals(as.character(colnames(z)),as.character(fz$column.annotations[,'Name']))
	checkEquals(as.character(column.annotations[,'y']),as.character(fz$column.annotations[,'y']))
	checkEquals(as.character(column.annotations[,'char']),as.character(fz$column.annotations[,'char']))
	checkEquals(as.character(column.annotations[,'fac']),as.character(fz$column.annotations[,'fac']))
}

test_rows <- function() {
	checkEquals(as.character(row.names(z)),as.character(fz$row.annotations[,'Name']))
	checkEquals(as.character(row.annotations[,'y']),as.character(fz$row.annotations[,'y']))
	checkEquals(as.character(row.annotations[,'char']),as.character(fz$row.annotations[,'char']))
}
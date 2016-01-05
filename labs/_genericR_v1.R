# =============================================
# genericR_v1.R: many useful utility functions
#   for R
#
# by Nick Matzke
# Copyright 2011-infinity
# matzkeATberkeley.edu
# January 2011
#
# Please link/cite if you use this, email me if you have 
#   thoughts/improvements/corrections.
#
##############################################################
#
# Free to use/redistribute under:
# Attribution-NonCommercial-ShareAlike 3.0 Unported (CC BY-NC-SA 3.0) 
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the above license, linked here:
# 
# http://creativecommons.org/licenses/by-nc-sa/3.0/
# 
# Summary:
#
# You are free:
#
#   * to Share — to copy, distribute and transmit the work
#   * to Remix — to adapt the work
#
# Under the following conditions:
#
#   * Attribution — You must attribute the work in the manner 
#     specified by the author or licensor (but not in any way that 
#     suggests that they endorse you or your use of the work).
#   * Noncommercial — You may not use this work for commercial purposes. 
#
#   * Share Alike — If you alter, transform, or build upon this work,
#     you may distribute the resulting work only under the same or
#     similar license to this one. 
#
# http://creativecommons.org/licenses/by-nc-sa/3.0/
# 
###################################################################


# Generic utility functions for R
# Load with:
#sourcedir = '/_njm/'
#source3 = '_genericR_v1.R'
#source(paste(sourcedir, source3, sep=""))

# abbreviations
adf <- function(x)
	{
	return(as.data.frame(x, row.names=NULL, stringsAsFactors=FALSE))
	}

# length, like in Python
len <- function(x)
	{
	return(length(x))
	}

# the number of distinct values may be an issue,
# let's count those
count_uniq <- function(x)
	{
	length(unique(x))
	}
count_zeros <- function(x)
	{
	sum(x==0)
	}



# Normalize values to a large positive number
normalize_vals <- function(char_dtf, maxvals = NULL, baseval=0, normval=100)
	{
	
	# Normalize values to between 0 and 1
	# set these vals to 1
	if (is.null(maxvals))
		{
		maxvals = apply(char_dtf, 2, max)
		}
	char_dtf_fraction_of_one = char_dtf
	for (rownum in 1:nrow(char_dtf))
		{
		char_dtf_fraction_of_one[rownum, ] = char_dtf[rownum, ] / maxvals
		} 
	
	# Multiply by 100
	char_dtf_fraction_of_100 = char_dtf_fraction_of_one * 100
	
	# Add 1000
	char_dtf_1000 = char_dtf_fraction_of_100 + 1000
	
	# to reverse, subtract 1000, divide by 100, multiply by maxval
	}
	

# Summarize object
summ <- function(x)
	{
	cat("\n")
	cat("SUMM(): PROVIDING OVERALL SUMMARY OF OBJECT...\n")
	cat("\nCLASS OF OBJECT:\n")
	print(class(x))

	cat("\nDIMENSIONS OF OBJECT:\n")
	print(dim(x))

	cat("\nLENGTH OF OBJECT:\n")
	print(length(x))

	cat("\nATTRIBUTES OF OBJECT:\n")
	print(attributes(x))

	cat("\nSUMMARY() OF OBJECT:\n")
	print(summary(x))
	
	cat("\ncls.df(): print the classes of each column (if it's a data.frame):\n")
	cls.df(x, printout=TRUE)
	
	}

print.default2 <- function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, right = FALSE, max = NULL, useSource = TRUE, ...)
	{
	if (is.numeric(x))
		{
		x <- as.numeric(sprintf("%7.3f", x))
		}
	noOpt <- missing(digits) && missing(quote) && missing(na.print) && missing(print.gap) && missing(right) && missing(max) && missing(useSource) && length(list(...)) == 0L
	
	.Internal(print.default(x, digits, quote, na.print, print.gap, right, max, useSource, noOpt))
	}


prflag <- function(x, printflag=TRUE)
	{
	# A standard function to print (or not) certain variables,
	#   based on a master printflag
	# This avoids having to comment in/out various code chunks
	#   while debugging.
	if (printflag == TRUE)
		{
		# CAT instead of PRINT if it's a string or numeric
		if (is.character(x))
			{
			cat(x, "\n", sep="")
			}
		if (is.numeric(x))
			{
			cat(x, "\n", sep="")
			} else {
			print(x)
			}
		}
	else
		{
		pass="BLAH"
		}
	}


# Backup file, if it exists
fn_bkup <- function(tmpdir, fn)
	{
	# Check if the old log file exists
	files_list = list.files(path=tmpdir)
	if (fn %in% files_list)
		{
		cmdstr = paste("mv ", fn, " ", paste(fn, ".bkup", sep=""), sep="")
		system(cmdstr)
		}
	}

printall <- function(dtf, chunksize_toprint = 50, printflag=TRUE)
	{
	# Print everything in a data frame, in chunks of e.g. 50 rows
	if (nrow(dtf) <= chunksize_toprint)
		{
		prflag(dtf)
		return(dtf)
		}
	rows_toprint = seq(1, nrow(dtf), chunksize_toprint)
	for (i in 1 : (length(rows_toprint)-1) )
		{
		tmp_rows_toprint_start = rows_toprint[i]
		tmp_rows_toprint_end = rows_toprint[i+1]
		prflag(dtf[tmp_rows_toprint_start:tmp_rows_toprint_end, ])
		}
	}

# Get indices of all matches to a list
findall <- function(what, inlist)
	{
	TFmatches = inlist == what
	indices = 1:length(inlist)
	matching_indices = indices[TFmatches]
	return(matching_indices)
	}

# print to screen the header of a file
headf <- function(fn)
	{
	lines = scan(file=fn, what="character", sep="\n")
	for (i in 1:5)
		{
		print(lines[i])
		}
	}

# print to screen the tail of a file
tailf <- function(fn)
	{
	lines = scan(file=fn, what="character", sep="\n")
	numlines = length(lines)
	for (i in (numlines-5):numlines)
		{
		print(lines[i])
		}
	}



# remove punctuation
remove_punct <- function(tempstr)
	{
	# Do find-replace to convert the names to all underscores, no spaces, no ' or "
	# spaces to _
	tempstr = gsub(" ", "_", tempstr)
	# - to _
	tempstr = gsub("-", "_", tempstr)
	# . to _
	tempstr = gsub("\\.", "_", tempstr)
	# "'" to ""
	tempstr = gsub("'", "", tempstr)
	# '"' to ''
	tempstr = gsub('"', '', tempstr)
	
	return(tempstr)
	}

# remove punctuation from a list
remove_punct_list <- function(templist)
	{
	for (k in 1:length(templist))
		{
		tempstr = templist[[k]]
		templist[[k]] = remove_punct(tempstr)
		}
	return(templist)
	}


# split out: Taxon (subtaxon)
split_out_subtaxon <- function(templist)
	{
	templist_col1 = templist
	templist_col2 = templist
	for (k in 1:length(templist))
		{
		tempstr = templist[[k]]
		words = strsplit(tempstr, " \\(")[[1]]
		if (length(words) == 1)
			{
			templist_col1[[k]] = words[1]
			templist_col2[[k]] = ""
			}
		if (length(words) == 2)
			{
			templist_col1[[k]] = words[1]
			templist_col2[[k]] = gsub("\\)", "", words[2])
			}
		
		}
	
	newcols = cbind(templist_col1, templist_col2)
	return(newcols)
	}



# http://r.789695.n4.nabble.com/Remove-empty-list-from-list-td806706.html
delete.NULLs <- function(x.list)
	{
	# delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]
	} 

remove_len_zero <- function(tmparr)
	{
	newarr = tmparr[lapply(tmparr, nchar) != 0]
	return(newarr)
	}




# match using 2 criteria
match_items <- function(tmplist, crit1, crit2)
	{
	outlist = c()
	
	crit1_list = c()
	for (i in 1:length(tmplist))
		{
		if (grepl(crit1, tmplist[i]))
			{
			crit1_list = c(crit1_list, tmplist[i])
			}
		}

	crit2_list = c()	
	for (i in 1:length(crit1_list))
		{
		if (grepl(crit2, crit1_list[i]))
			{
			crit2_list = c(crit2_list, crit1_list[i])
			}
		
		}
	return(crit2_list)
	}



# match using crit1, but skip (leave out) if they have crit2
match_item_nomatch <- function(tmplist, crit1, crit2)
	{
	outlist = c()
	
	crit1_list = c()
	for (i in 1:length(tmplist))
		{
		if (grepl(crit1, tmplist[i]))
			{
			crit1_list = c(crit1_list, tmplist[i])
			}
		}

	crit2_list = c()	
	for (i in 1:length(crit1_list))
		{
		if (grepl(crit2, crit1_list[i]))
			{
			blah = "blah"
			}
		else
			{
			crit2_list = c(crit2_list, crit1_list[i])
			}
		
		}
	return(crit2_list)
	}


replace_str_file <- function(fn, oldstr, newstr)
	{
	# Read a text file into a list of strings
	lines = scan(fn, what="character", sep="\n")

	# Replace output date-calibrated parsimony trees file
	newlines = replace_str_lines(lines, oldstr, newstr)
	
	# Write the list of lines to a file
	write.table(newlines, file=fn, quote=FALSE, append=FALSE, sep="", row.names = FALSE, col.names=FALSE)
	
	cat("In: ", fn, ", '", oldstr, "' replaced with '", newstr, "'\n", sep="")
	
	return(fn)
	}


replace_str_lines <- function(lines, oldstr, newstr)
	{

	# Replace output date-calibrated parsimony trees file
	lines = gsub(oldstr, newstr, lines)
	
	return(lines)
	}


# replace each in a text file that matches, with newstr
replace_each_matching_line <- function(fn, str_to_find, newstr)#, blankstr="___")
	{
	# Read a text file into a list of strings
	lines = scan(fn, what="character", blank.lines.skip=FALSE, sep="\n")

	for (i in 1:length(lines))
		{
	#	if (lines[i] == "")
	#		{
	#		lines[i] = blankstr
	#		}
	#	else
	#		{
		if (grepl(str_to_find, lines[i]))
			{
			#print(str_to_find)
			#print(lines[i])
			lines[i] = newstr
			cat("In: ", fn, ", line #", i, " replaced with '", newstr, "'\n", sep="")
			}
	#		}
		}
	# Write the list of lines to a file
	newfn = paste(fn, "_new", sep="")
	write.table(lines, file=newfn, quote=FALSE, append=FALSE, sep="", row.names = FALSE, col.names=FALSE)
	
	return(newfn)
	}


# Null lines that match
null_lines <- function(list_of_lines, str_to_match)
	{
	
	list_of_lines2 = c()
	
	for (i in 1:length(list_of_lines))
		{
		if (grepl(str_to_match, list_of_lines[i]))
			{
			# This, in effect, deletes the row containing the string
			blah = 0
			}
		else
			{
			list_of_lines2 = c(list_of_lines2, list_of_lines[i])
			}
		}
	return(list_of_lines2)
	}





# Concatenate a list of files into one big file
cat_files <- function(infiles_list, outfn)
	{
	new_lines = c()
	list_num_files = c()
	
	for (i in 1:length(infiles_list))
		{
		# input file
		infn = infiles_list[i]
		
		lines = scan(infn, what="character", sep="\n")
		
		list_num_files = c(list_num_files, length(lines))

		new_lines = c(new_lines, lines)
		}
	
	new_lines = write.table(new_lines, file=outfn, quote=FALSE, append=FALSE, sep="", row.names = FALSE, col.names=FALSE)
	
	return(list_num_files)
	}




# print to screen the header of a file
moref <- function(fn, printnotcat = FALSE)
	{
	lines = scan(file=fn, what="character", sep="\n")
	
	if (printnotcat == TRUE)
		{
		for (i in 1:length(lines))
			{
			print(lines[i])
			}
		}
	else
		{
		for (i in 1:length(lines))
			{
			cat(paste(lines[i], "\n", sep=""))
			}
		}
	}

print_table_sigs <- function(x, numsig=4,  printout=FALSE)
	{
	new.table = signif_digits_df(dfnums_to_numeric(x), numsig, printout)
	cat("\n")
	print(new.table)
	cat("\n")
	return(new.table)
	}

print_table_row <- function(x, numsig=4,  printout=FALSE)
	{
	if (length(numsig) == 1)
		{
		new.table = signif(as.numeric(x), numsig)
		}
	else
		{
		tmplist = rep(NA, length(numsig))
		for (i in 1:length(numsig))
			{
			cmdstr = paste('tmplist[i] = sprintf("%1.', numsig[i], 'f", x[i])', sep="")
			eval(parse(text = cmdstr))
			}
		}
	
	outstr = ""
	for (item in tmplist)
		{
		outstr = paste(outstr, trim(item), sep="	")
		}
	
	print(paste(tmplist, sep="	"), quote=FALSE)
	#cat("\n")
	return(tmplist)
	}



# Get the classes of the columns in a data frame
cls.df <- function(dtf, printout=FALSE)
	{
	dtf_names = names(dtf)
	numcols = ncol(dtf)
	
	cls_col_list = c()
	for (i in 1:numcols)
		{
		# Get one column:
		cmdstr = paste("cls_col = class(dtf$", dtf_names[i], ")", sep="")
		eval(parse(text = cmdstr))
		
		#cat(i, ": ", dtf_names[i], "	=	", cls_col, "\n", sep="")
		cls_col_list[i] = cls_col
		}
	
	# row names
	colnum = 1:numcols
	
	dtf_classes = cbind(colnum, dtf_names, cls_col_list)
	dtf_classes = data.frame(dtf_classes, row.names=colnum)
	
	# Print the output if true
	if (printout)
		{
		cat("\n")
		cat("cls.df(dtf) reports: dataframe 'dtf' has ", nrow(dtf), " rows, ", numcols, " columns.\n", sep="")
		cat("...names() and classes() of each column below...\n", sep="")
		cat("\n")
		print(dtf_classes)
		cat("\n")
		}	
	return(dtf_classes)
	}




dfnums_to_numeric <- function(dtf, max_NAs=0.5, printout=FALSE)
	{
	dtf_classes = cls.df(dtf, printout=FALSE)
	
	dtf_names = names(dtf)
	numcols = ncol(dtf)
	
	cls_col_list = c()
	for (i in 1:numcols)
		{
		# Get one column:
		cmdstr = paste("cls_col = class(dtf$", dtf_names[i], ")", sep="")
		eval(parse(text = cmdstr))
		
		#cat(i, ": ", dtf_names[i], "	=	", cls_col, "\n", sep="")
		cls_col_list[i] = cls_col
		}
	
	for (i in 1:numcols)
		{
		if (cls_col_list[i] == "list")
			{
			next()
			}
		if (cls_col_list[i] != "numeric")
			{
			# Get one column, convert to numeric:
			cmdstr = paste("newcol = as.numeric(as.character(dtf$", dtf_names[i], "))", sep="")
			eval(parse(text = cmdstr))			
			
			# If it's less than 50% NAs (or max_NA NAs), then convert to numeric
			#print(newcol)
			#print(max_NAs * length(newcol))
			#print(sum(is.na(newcol)))
			if (sum(is.na(newcol)) < (max_NAs * length(newcol)))
				{
				# Get the column, convert to numeric:
				# (if it's a factor, you have to convert to character, then to numeric)
				# (if it's a character, you can convert to character anyway...)
				#cmdstr = paste("dtf$", dtf_names[i], " = as.numeric(as.character(dtf$", dtf_names[i], "))", sep="")
				cmdstr = paste("dtf$", dtf_names[i], " = newcol", sep="")
				eval(parse(text = cmdstr))				
				}
			}
		}
	tmp_classes = cls.df(dtf)
	dtf_classes$newclasses = tmp_classes[,ncol(tmp_classes)]
	
	if(printout)
		{
		cat("\n")
		cat("dfnums_to_numeric(dtf, max_NAs=", max_NAs, ") reports: dataframe 'dtf_classes' has ", nrow(dtf_classes), " rows, ", ncol(dtf_classes), " columns.\n", sep="")
		cat("...names() and classes() of each column below...\n", sep="")
		cat("\n")
		print(dtf_classes)
		}
	return(dtf)
	}


df_everything_to_char <- function(dtf, max_NAs=0.5)
	{
	
	dtf_names = names(dtf)
	numcols = ncol(dtf)
	
	cls_col_list = c()
	
	for (i in 1:numcols)
		{
			# Get one column, convert to character:
			cmdstr = paste("dtf$", dtf_names[i], " = as.character(dtf$", dtf_names[i], ")", sep="")
			eval(parse(text = cmdstr))			
		}
	
	return(dtf)
	}



df_factors_to_char <- function(dtf, max_NAs=0.5)
	{
	dtf_classes = cls.df(dtf, printout=TRUE)
	
	dtf_names = names(dtf)
	numcols = ncol(dtf)
	
	cls_col_list = c()
	for (i in 1:numcols)
		{
		# Get one column:
		cmdstr = paste("cls_col = class(dtf$", dtf_names[i], ")", sep="")
		eval(parse(text = cmdstr))
		
		#cat(i, ": ", dtf_names[i], "	=	", cls_col, "\n", sep="")
		cls_col_list[i] = cls_col
		}
	
	for (i in 1:numcols)
		{
		if (cls_col_list[i] == "factor")
			{
			# Get one column, convert to character:
			cmdstr = paste("newcol = as.character(dtf$", dtf_names[i], ")", sep="")
			eval(parse(text = cmdstr))			
			
			cmdstr = paste("dtf$", dtf_names[i], " = newcol", sep="")
			eval(parse(text = cmdstr))				
			}
		}
	tmp_classes = cls.df(dtf)
	dtf_classes$newclasses = tmp_classes[,ncol(tmp_classes)]
	cat("\n")
	cat("dfnums_to_numeric(dtf, max_NAs=", max_NAs, ") reports: dataframe 'dtf_classes' has ", nrow(dtf_classes), " rows, ", ncol(dtf_classes), " columns.\n", sep="")
	cat("...names() and classes() of each column below...\n", sep="")
	cat("\n")
	print(dtf_classes)
	
	return(dtf)
	}


# Remove factors from the character-like columns of a data.frame
# (leaves the numbers as numbers)
df_nonum_factors_to_char <- function(dtf, max_NAs=0.5)
	{
	dtf_classes = cls.df(dtf, printout=TRUE)
	
	dtf_names = names(dtf)
	numcols = ncol(dtf)
	
	cls_col_list = c()
	for (i in 1:numcols)
		{
		# Get one column:
		cmdstr = paste("cls_col = class(dtf$", dtf_names[i], ")", sep="")
		eval(parse(text = cmdstr))
		
		#cat(i, ": ", dtf_names[i], "	=	", cls_col, "\n", sep="")
		cls_col_list[i] = cls_col
		}
	
	for (i in 1:numcols)
		{
		if (cls_col_list[i] == "factor")
			{
			# Get one column, convert to character:
			cmdstr = paste("newcol = as.character(dtf$", dtf_names[i], ")", sep="")
			eval(parse(text = cmdstr))			
			
			cmdstr = paste("dtf$", dtf_names[i], " = newcol", sep="")
			eval(parse(text = cmdstr))				
			}
		}
	tmp_classes = cls.df(dtf)
	dtf_classes$newclasses = tmp_classes[,ncol(tmp_classes)]
	cat("\n")
	cat("dfnums_to_numeric(dtf, max_NAs=", max_NAs, ") reports: dataframe 'dtf_classes' has ", nrow(dtf_classes), " rows, ", ncol(dtf_classes), " columns.\n", sep="")
	cat("...names() and classes() of each column below...\n", sep="")
	cat("\n")
	print(dtf_classes)
	
	return(dtf)
	}



moving_average <- function(xvals, yvals, byvals)
	{
	# moving averages
	intervals = seq(from=min(xvals), to=max(xvals), by=byvals)
	xmeans = c()
	ymeans = c()
	for (i in 1:(length(intervals)-1))
		{
		starti = intervals[i]
		endi = intervals[i+1]
		
		indices1 = xvals >= starti
		indices2 = xvals < endi
		indices_in_range = (indices1 + indices2) > 1
		
		vals = yvals[indices_in_range]
		if (length(vals) > 0)
			{
			xmean = mean(c(starti, endi))
			ymean = mean(vals)
			xmeans = c(xmeans, xmean)
			ymeans = c(ymeans, ymean)
			}
		}
	tmp = cbind(xmeans, ymeans)
	moving_avgs = data.frame(tmp)
	return(moving_avgs)
	}



extract_lines_startnum_to_flag <- function(lines, row_to_start, string_to_stop_at)
	{
	doneflag = 0
	lines_to_keep = c()
	lines_to_keep_i = 0
	for (i in 1:length(lines))
		{
		line = lines[i]
	
		# skip the 1st two lines
		if (i < row_to_start)
			{
			blah = 0
			}
		else
			{		
			if (doneflag == 1)
				{
				blah = 0
				}
			else
				{
				# if line starts with a dash, you are done...
				if (grepl(string_to_stop_at, line))
					{
					#print("yo!")
					doneflag = 1
					}
				else
					{
					lines_to_keep_i = lines_to_keep_i + 1
					print(paste("Storing line #", lines_to_keep_i, sep=""))
					lines_to_keep[[lines_to_keep_i]] = line
					}
				}
			}
		
		}
	return(lines_to_keep)
	}


# This will only extract 1 group between 1st startflag & 1st doneflag
extract_lines_startstr_to_endstr <- function(lines, string_to_start_at, string_to_end_at, printflag=TRUE)
	{
	startflag = 0
	doneflag = 0
	lines_to_keep = c()
	lines_to_keep_i = 0
	for (i in 1:length(lines))
		{
		line = lines[i]
		
		if (grepl(string_to_start_at, line))
			{
			startflag = 1
			}
		if (grepl(string_to_end_at, line))
			{
			doneflag = 1
			}
		
		# end if done; if not, check startflag
		if (doneflag == 1)
			{
			blah = 0
			}
		else
			{		
			if (startflag == 1)
				{
				lines_to_keep_i = lines_to_keep_i + 1
				if(printflag)
					{			
					print(paste("Storing line #", lines_to_keep_i, sep=""))
					}
				lines_to_keep[[lines_to_keep_i]] = line				
				}
			else
				{
				blah = 0
				}
			}
		
		}
	return(lines_to_keep)
	}



# Extract the strings that have a certain word at a certain position
extract_lines_with_word_at_wordnum <- function(lines, word_to_find, wordnum=1, delimiter=" ", printflag=FALSE)
	{
	linenums_to_keep = c()
	
	for (i in 1:length(lines))
		{
		tmpline = lines[i]
		if (tmpline == "")
			{
			next
			}
		words = strsplit(tmpline, split=delimiter)[[1]]
		#print(words)
		if (words[wordnum] == word_to_find)
			{
			#print(words[wordnum])
			linenums_to_keep[length(linenums_to_keep)+1] = i
			if(printflag)
				{			
				print(paste("Storing line #", i, sep=""))
				}

			}
		}
	#print(linenums_to_keep)
	newlines = lines[linenums_to_keep]
	return(newlines)
	}


array_to_text <- function(inarray, spacer)
	{
	tmpstr = inarray[1]
	for (i in 2:length(inarray))
		{
		tmpstr = paste(tmpstr, inarray[i], sep=spacer)
		}
	return(tmpstr)
	}

minmax_pretty <- function(x)
	{
	minmax = c( min(pretty(x)), max(pretty(x)) )
	return(minmax)
	}

minmax_literal <- function(x)
	{
	minmax = c( min(x, na.rm=TRUE), max(x, na.rm=TRUE) )
	return(minmax)
	}




convert_nums_to_circular_lat <- function(nums, maxval=90)
	{
	mini = floor(min(nums))
	maxi = ceiling(max(nums))
	nums5 = rep(0, length(nums))

	# go through 0-maxval, maxval-2*maxval, 2*maxval-3*maxval, 3*maxval-4*maxval
	# translates:0-maxval, maxval-0,   0- -maxval,  -maxval-0
	edgeval = maxval * 4

	# get the nums in terms of edgevals
	nums1 = nums/edgeval
	
		
	# Don't change numbers between -maxval and maxval, but change the rest of them...

	##############################################	
	# degrees > 0 (positive)
	##############################################
	posvals = nums > 0
	
	# remove all cycles except the last
	nums2 = nums1[posvals] - floor(nums1[posvals])
	
	# convert back to lat:
	nums3 = nums2 * edgeval

	nums4 = nums3

	vals_to_change = (nums3 > maxval) + (nums3 <= 2*maxval) == 2
	nums4[vals_to_change] = 2*maxval - nums3[vals_to_change]

	vals_to_change = (nums3 > 2*maxval) + (nums3 <= 3*maxval) == 2
	nums4[vals_to_change] = -1*(nums3[vals_to_change] - 2*maxval)

	vals_to_change = (nums3 > 3*maxval) + (nums3 <= 4*maxval) == 2
	nums4[vals_to_change] = 4*maxval - nums3[vals_to_change]

	nums5[posvals] = nums4


	##############################################
	# degrees < 0 (negative)
	##############################################
	negvals = nums < 0
	
	# remove all cycles except the last
	nums2 = nums1[negvals] - ceiling(nums1[negvals])
	
	# convert back to lat:
	nums3 = nums2 * edgeval

	nums4 = nums3


	vals_to_change = (nums3 < -maxval) + (nums3 >= -2*maxval) == 2
	nums4[vals_to_change] = -2*maxval - nums3[vals_to_change]

	vals_to_change = (nums3 < -2*maxval) + (nums3 >= -3*maxval) == 2
	nums4[vals_to_change] = -1*(nums3[vals_to_change] - -1*2*maxval)

	vals_to_change = (nums3 < -3*maxval) + (nums3 >= -4*maxval) == 2
	nums4[vals_to_change] = -4*maxval - nums3[vals_to_change]

	nums5[negvals] = nums4

		
	return(nums5)
	}



convert_nums_to_long <- function(nums, maxval=180)
	{
	# get the nums in terms of 180s
	nums0 = nums
	nums1 = nums
	
	# For nums > 0
	TFnums_gt0 = nums > 0
	nums1[TFnums_gt0] = nums0[TFnums_gt0]/maxval
	
	# remove all cycles except the last
	nums1[TFnums_gt0] = nums1[TFnums_gt0] - floor(nums1[TFnums_gt0])
	
	# convert back to lat:
	nums1[TFnums_gt0] = nums1[TFnums_gt0] * maxval
	

	# For nums < 0
	TFnums_lt0 = nums < 0
	nums1[TFnums_lt0] = nums0[TFnums_lt0]/maxval
	
	# remove all cycles except the last
	nums1[TFnums_lt0] = nums1[TFnums_lt0] - ceiling(nums1[TFnums_lt0])
	
	# convert back to lat:
	nums1[TFnums_lt0] = nums1[TFnums_lt0] * maxval
	
	
	
	return(nums1)
	}








# Basic correlation plot
basic_xy_plot <- function(x, y, titlestart_txt="title\n", xlab=names(x), ylab=names(y), xlim=minmax_pretty(x), ylim=minmax_pretty(y))
	{
	cortest_xy = cor.test(x, y, xlim=c(-1,1), ylim=c(-1,1))
	lm_xy = lm(y ~ x)
	slope = round(lm_xy$coefficients[2], 2)
	intercept = round(lm_xy$coefficients[1], 2)
	corval = round(cortest_xy$estimate, 2)
	#pval = round(cortest_xy$p.value, 4)
	pval = cortest_xy$p.value
	
	
	titletxt = paste(titlestart_txt, "cor=", corval, "; slope=", slope, "; int=", intercept, "; p=", pval, sep="")
	plot(x, y, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
	
	# make segment
	minx = min(x, na.rm=TRUE)
	miny = min(y, na.rm=TRUE)
	maxx = max(x, na.rm=TRUE)
	maxy = max(y, na.rm=TRUE)
	
	x0 = minx
	x1 = maxx
	y0 = slope*x0 + intercept
	y1 = slope*x1 + intercept
	
	segments(x0, y0, x1, y1)
	
	title(titletxt)

	}

plot_basic_xy <- function(x, y, titlestart_txt="title\n", xlab=names(x), ylab=names(y), xlim=minmax_pretty(x), ylim=minmax_pretty(y))
	{
	cortest_xy = cor.test(x, y, xlim=c(-1,1), ylim=c(-1,1))
	lm_xy = lm(y ~ x)
	slope = round(lm_xy$coefficients[2], 2)
	intercept = round(lm_xy$coefficients[1], 2)
	corval = round(cortest_xy$estimate, 2)
	pval = cortest_xy$p.value
	
	titletxt = paste(titlestart_txt, "cor=", corval, "; slope=", slope, "; int=", intercept, "; p=", pval, sep="")
	plot(x, y, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
	
	# make segment
	minx = min(x, na.rm=TRUE)
	miny = min(y, na.rm=TRUE)
	maxx = max(x, na.rm=TRUE)
	maxy = max(y, na.rm=TRUE)
	
	x0 = minx
	x1 = maxx
	y0 = slope*x0 + intercept
	y1 = slope*x1 + intercept
	
	segments(x0, y0, x1, y1)
	
	title(titletxt)

	}



# Fancy correlation plots
#http://www.personality-project.org/R/r.useful.html
#some fancy graphics   -- adapted from help.cor
panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor)
	{
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	r = (cor(x, y,use="pairwise"))
	txt <- format(c(r, 0.123456789), digits=digits)[1]
	txt <- paste(prefix, txt, sep="")
	if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex = cex * abs(r))
	}
	
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
	{
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	r = (cor(x, y,use="pairwise"))
	txt <- format(c(r, 0.123456789), digits=digits)[1]
	txt <- paste(prefix, txt, sep="")
	if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex = cex )
	}
	
panel.hist <- function(x, ...)
	{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
	}

panel.points <- function(x, tmppch=20, ...)
	{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    par(pch=20)
    pts <- points(x, pch=".")
	}


pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE) 
	{
	if (smooth )
	 	{
		if (scale)
			{
			pairs(x, diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.smooth)
		    #pairs(x, diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=function(x) panel.points(x) )
		    #pairs(x, diag.panel=panel.hist, upper.panel=panel.cor.scale, lower.panel=function(x) points(x, pch=tmppch))
		    #pairs(x, diag.panel=panel.hist, upper.panel=panel.cor.scale, lower.panel=panel.points)#, lower.panel=panel.points(x, pch=tmppch))
			}
		else
			{
		    #pairs(x, diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=function(x) panel.points(x) )
			pairs(x, diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
			#pairs(x, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=function(x) points(x, pch=tmppch))
		    #pairs(x, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.points)#, lower.panel=panel.points(x, pch=tmppch))

			} #else  {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
		}
	else      #smooth is not true
		{
		if (scale)
			{
			pairs(x, diag.panel=panel.hist,upper.panel=panel.cor.scale)
			}
		else
			{
			pairs(x, diag.panel=panel.hist,upper.panel=panel.cor)
			}
		} #end of else (smooth)
	}   #end of function

 
pairs.panels.lm <- function (x,y,tmplm=TRUE,scale=FALSE) 
	{
	if (tmplm)
		{
		if (scale)
			{
			pairs(x, diag.panel=panel.hist, upper.panel=panel.cor.scale, lower.panel=panel.smooth, span=0.5, f=0.5)
		   }
		else
			{
			pairs(x, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)
	   	} #else  {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
		}
	else      #smooth is not true
		{
		if (scale)
			{
			pairs(x, diag.panel=panel.hist,upper.panel=panel.cor.scale)
			}
		else
			{
			pairs(x, diag.panel=panel.hist,upper.panel=panel.cor) }
			} #end of else (smooth)
	}   #end of function



#function to replace upper triangle of matrix with unattenuated correlations, and the diagonal with reliabilities
#input is a correlation matrix and a vector of reliabilities
	
 correct.cor <- function(x,y) { n=dim(x)[1]   
	   { x[1,1] <- y[1,1]
	   for (i in 2:n) {
		  x[i,i] <- y[1,i]   #put reliabilities on the diagonal
		  k=i-1
		  for (j in 1:k) {
		     x[j,i] <- x[j,i]/sqrt(y[1,i]*y[1,j])  }   #fix the upper triangular part of the matrix
	   
		    }
		  return(x)  }}
		 
	
 #difference of dependent (paired) correlations (following Steiger,J., 1980, Tests for comparing elements of a correlation matrix. Psychological Bulletin, 87, 245-251)
 paired.r <- function(xy,xz,yz,n) {
	  diff <- xy-xz
	  determin=1-xy*xy - xz*xz - yz*yz + 2*xy*xz*yz
	  av=(xy+xz)/2
	  cube= (1-yz)*(1-yz)*(1-yz)
	  t2 = diff * sqrt((n-1)*(1+yz)/(((2*(n-1)/(n-3))*determin+av*av*cube)))
	  return(t2)
	   }
	
 fisherz <- function(rho)  {0.5*log((1+rho)/(1-rho)) }   #converts r to z     



###################################################
# LaTeX-related functions
###################################################
#
# to run tex2div
library(tools)

# You also need a well-set-up .Rprofile file to make it work (see fun_w_Rprofile.R in /njm probably)

## Set working directory
##wd = "/Users/nick/Desktop/_IB286_Marshall_paleobio/"
#wd = "/Users/nick/Desktop/_2010-10-12_work/_Marshall_PBDB/_IB286_Marshall_paleobio/"
#setwd(wd)

runlatex <- function(fn, wd=getwd())
	{
	#swwds = c("/usr/local/texlive/2009basic/texmf-dist/tex/latex/base/", wd, "/usr/texbin/")
	
	# Setup directories for LaTex files and the working directories
	swwds = c("/usr/local/texlive/2008/texmf-dist/tex/latex/base/", wd, "/usr/texbin/")
	
	# These commands build and open the Sweave tex and PDF files...
	# permfn = "example-2.Snw"
	# permfn = "hw2_v2.Snw"
	permfn = fn
	Sweave(permfn)
	
	texfn = sub(".Snw", ".tex", permfn)
	texi2dvi(texfn, pdf="TRUE", quiet=FALSE, texinputs=swwds)
	
	pdffn = sub(".Snw", ".pdf", permfn)
	cmdstr = paste("open ", pdffn, sep="")
	system(cmdstr)
	
	return(cmdstr)
	}





##############################################################
# TreeRogue 0.1: TreeThief-like tree grabbing using x,y 
# coordinates digitized from an image of a phylogenetic tree.
##############################################################
# GOAL: to process x, y coordinates into a Newick-format tree
##############################################################
# by Nick Matzke
# Copyright 2010
# matzke@berkeley.edu
# 10/27/2010
##############################################################
#
# Free to use/redistribute under GNU General Public License, version 2 
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# 
# http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
# 
###################################################################
#
# #===================================================
# Run with these commands
# ===================================================
# library(ape)
# 
# put the text files (at bottom) into your working directory
# wd = "/Users/nick/Desktop/__projects/2010-11-01_cone_snails/"
# setwd(wd)
# 
# xy2 = treerogue_read_files()
# xy3 = treerogue_associate_branch_bottoms_with_nodes(xy2)
# tr = build_tree_using_corners(xy3)
# plot(tr)
#
####################################################################
#
# NOTES
# *Heavily* modified from a very limited script posted here by 
# bbolker@gmail.com:
# https://stat.ethz.ch/pipermail/r-sig-phylo/2008-November/000173.html
#
# Background: I worked up this script after getting frustrated at
# (1) the failure of the famous "TreeThief" to work on any modern machine;
# (2) my failure to get the newer "TreeSnatcher" to work on Mac OS X 10.4.11
# (some weirdness about Java and X11, as far as I can tell), and
# (3) no other good options.
#
# Summary: This script takes in x,y coordinates (with the lower left as the origin)
# of nodes, tips, and branch bottoms ("corners"), and builds a tree out of it.
# 
# It assumes:
# (1) Your tree is horizontal left-to-right, with the tips on the right
# (2) Your tree is a "square" tree (i.e. no diagonal/curved branches)
#
# I captured my x,y coordinates using GraphClick 3.0, available for $8 at:
# http://www.arizona-software.ch/graphclick/
#
# (there is a free trial download, but only lets you export 10 coordinates at a
# time, so it is pointless)
#
# REQUIRED INPUTS:
#   (for each, digitize the relevant points in GraphClick, and
#    File-->Export to a text file):
# 
# (Note: all text files should have a header line)
#
# 1. A tab-delimited text file with x and y for each internal node
#
# 2. A tab-delimited text file with x and y for each tip
#
# 2a. A text file with tip names, in order from top-to-bottom
# 
# 3. A tab-delimited text file with x and y for each tip for each "corner"
#    (i.e., the bottom of each branch).
#
# 4. For now, do NOT digitize the bottom of the root of the tree, if the
#    image you are digitizing has one.  You could add the root length later
#    manually, if you like.
#
# 5. The tree must be fully dichotomous (if the image you are digitizing is not,
#    you can "fake it" by resolving polytomies by clicking digitization points
#    to, in effect, manually resolve these polytomies with very short branches.
#    Note that you will have to add a corner for each internal node you add (!).
#
#    The R APE package can collapse short branches to polytomies later, if you like.
#
# Trees to not have to be ultrametric, and digitization does not have to be 
# exact -- the script will attempt to match the most likely branch bottoms
# to the nodes (a graphic is produced by R so that you can check the results
# and tinker if necessary).
#
# Requires the APE library.
# 
#############################################################

library(ape)


# Assumes default filenames
treerogue_read_files <- function()
	{
	internal = read_table_good("internal.txt")
	tips = read_table_good("tips.txt")
	tipnames = read_table_good("tipnames.txt")
	corners = read_table_good("corners.txt")
	
	# sort the tips from top to bottom in y
	tips = tips[order(tips$y, decreasing = TRUE), ]
	
	# sort the internals from left to right in x
	internal = internal[order(internal$x, decreasing=FALSE), ]
	
	if (nrow(tips) != nrow(tipnames))
		{
		print("ERROR: the number of tips must equal the length of the tipnames!")
		print(paste("Instead, nrow(tipnames) =", nrow(tipnames), "and nrow(tips) =", nrow(tips), sep=" "))
		}
	
	if ((nrow(tips)-1) != nrow(internal))
		{
		print("ERROR: the number of tips-1 must equal the number of the internal nodes!")
		print(paste("Instead, nrow(tips) =", nrow(tips), "and nrow(internal) =", nrow(internal), sep=" "))
		}
	
	nodetypes = c(rep("tip", nrow(tipnames)), rep("internal", nrow(internal)))
	nodenames = unlist(c(tipnames, rep("", nrow(internal))))
	xy = rbind(tips, internal)
	xy2 = cbind(xy, nodetypes, nodenames)
	xy2 = as.data.frame(xy2)
	names(xy2) = c("x", "y", "nodetypes", "nodenames")
	
	xy2 = df_nonum_factors_to_char(xy2, max_NAs=0.5)
	
	xy2$nodetypes[xy2$x == min(xy2$x)] = "root"
	
	if (nrow(corners) != (nrow(xy2)-1))
		{
		print("ERROR: the number of corners must equal the number of nodes-1  !")
		print(paste("Instead, length(nodes) =", nrow(xy2), "and nrow(corners) =", nrow(corners), sep=" "))
		}

	# sort file so that the tips are first
	# tip nodes in order from top to bottom:
	xytips = xy[xy$tipname != "", ]
	
	tips_in_order = xy2$tipname[xy2$tipname != ""]

	return(xy2)

	}


treerogue_associate_branch_bottoms_with_nodes <- function(xy2)
	{
	# Load the coordinates of the corners
	corners = read_table_good("corners.txt")
	bots = corners
	

	# Get xy data
	xx = xy2$x
	yy = xy2$y
	
	dtf = associate_branch_bottoms_with_nodes(xx, yy, bots)

	xy3 = cbind(xy2, dtf$chosen_node_bottoms_xx, dtf$chosen_node_bottoms_yy)
	names(xy3) = c(names(xy2), "cx", "cy")
	
	write.table(xy3, "linked_corners.txt", quote=FALSE, sep="	", row.names = FALSE,
		   col.names = TRUE)

	return(xy3)
	}

# Associate branch bottom coordinates with nodes, and plot the results;
# The user may then edit the output associations if they so desire.
associate_branch_bottoms_with_nodes <- function(xx, yy, bots)
	{
	# There should be one less branch bottom than there are internal nodes
	# (because the root node (should) have no digitized "corner" beneath it)
	nodes = 1:length(xx)
	if (length(nodes) != nrow(bots) +1)
		{
		print("ERROR: the number of corners must equal the number of nodes-1  !")
		print(paste("Instead, length(nodes) =", length(nodes), "and nrow(bots) =", nrow(bots), sep=" "))
		} else {
		# OK, find bottom of branch to go with each top of branch
		# an array saying which branches have a bottom
		node_with_no_bottom = rep(TRUE, length(nodes))

		# these are the remaining branch bottoms that have not been associated yet
		bots_with_no_top = rep(TRUE, nrow(bots))
		
		bxx = bots$x
		byy = bots$y
		botsnodes = 1:nrow(bots)
		
		# Empty values to hold nodes
		chosen_node_bottoms_xx = rep(NA, length(xx))
		chosen_node_bottoms_yy = rep(NA, length(yy))
		i=0
		while (sum(node_with_no_bottom) > 1)
			{
			i=i+1
			#print(i)
			# look for branch bottom coordinates in a narrow window to the left of the node
			# basically (a) smallest slope and (b) closest distance in x
			
			## find next node to include (the rightmost internal node)
			maxvals = xx * node_with_no_bottom
			nextnode <- which( maxvals == max(maxvals, na.rm=TRUE) )[1]
			
			####################################
			# Find the best matching branch bottom/corner for each node
			####################################
			# This is trial-and-error, you may have to plink to find a function 
			# that works.
			# That, or do manual edits to the tree later...
			####################################
			ydist <- byy - yy[nextnode]
			xdist <- bxx - xx[nextnode]

			# Rank of the y distances
			rank_ydist = rank(abs(ydist))

			# calculate the slops
			xyslopes <- abs(ydist/xdist)
			
			# the best ancestor will have a low slope to the branch bottom, and a short negative distance in x
			xdist_neg = xdist
			xdist_neg[xdist > 0] = 0
			xdist_neg[xdist < 0] = -1 * xdist_neg[xdist < 0]
			# normalize to units of minimum absolute distance
			min_dist = (min(abs(xdist[xdist!=0]), na.rm=TRUE))
			xdist_neg_norm = (xdist_neg / min_dist)

			# short positive distances are less good (half as good) than short negative distances
			xdist_pos = xdist
			xdist_pos[xdist < 0] = 0
			xdist_pos[xdist > 0] = xdist_pos[xdist > 0]
			xdist_pos_norm = (xdist_pos / min_dist) * 100

			rank_xdist = rank_ydist
			rank_xdist[xdist <= 0] = 1
			rank_xdist[xdist > 0] = 10000000
			
			###########################
			# Plink here especially...
			###########################
			rank_slope = (xyslopes^2)
			final_rank = rank_ydist * abs(ydist) + xyslopes^0.5 * xdist_neg_norm + xdist_pos_norm
			###########################

			branch_bot_fits = final_rank			
			best_fit = which(branch_bot_fits == min(branch_bot_fits, na.rm=TRUE))[1]
			
			#bottom_to_add = botsnodes[bots_with_no_top][best_fit]
			bottom_to_add = botsnodes[best_fit]
			
			chosen_node_bottoms_xx[nextnode] = bxx[bottom_to_add]
			chosen_node_bottoms_yy[nextnode] = byy[bottom_to_add]
			
			bxx = bxx[-bottom_to_add]
			byy = byy[-bottom_to_add]
			
			# remove the node from the list needing branch bottoms
			node_with_no_bottom[nextnode] = FALSE
			bots_with_no_top[bottom_to_add] = FALSE
			
			
			}
		}
	
	tmpdata = cbind(nodes, xx, yy, chosen_node_bottoms_xx, chosen_node_bottoms_yy)
	#print(tmpdata)
	
	dtf = as.data.frame(tmpdata)
	plot(dtf$y, dtf$chosen_node_bottoms_yy)

	
	plot(c(xx, bots$x), c(yy, bots$y), pch="")
	points(xx, yy, pch="n")
	points(bots$x, bots$y, pch="b")
	title("Use this plot to check if branch bottoms match nodes")
	segments(xx, yy, chosen_node_bottoms_xx, chosen_node_bottoms_yy)
	
	plot(c(xx, bots$x), c(yy, bots$y), pch="")
	text(xx, yy, label=paste("n", 1:length(xx), sep=""))
	text(bots$x, bots$y, labe=paste("b", 1:nrow(bots), sep=""))
	title("This plot has node numbers.")
	segments(xx, yy, chosen_node_bottoms_xx, chosen_node_bottoms_yy)
	

	return(dtf)
	}



build_tree_using_corners <- function(xy3)
	{
	# define the tip.labels
	tip.labels = xy3$nodenames
	tip.labels = tip.labels[tip.labels != ""]
	if (!missing(tip.labels))
		{
		ntips <- length(tip.labels)
		}
	
	
	xx = xy3$x
	yy = xy3$y
	cx = xy3$cx
	cy = xy3$cy
	
	nodes <- 1:length(xx)
	is.tip <- nodes <= ntips

	# keep track of the nodes which are unlinked
	unlinked_nodes = rep(TRUE, length(nodes))

	
	# Checks (kinda) if internal nodes are ordered from left-to-right
	if (which.min(xx) != ntips+1)
		{
		## 
		print("ERROR: reorder nodes the way ape/phylo expects! (tips first, then internals in order from left-to-right.")
		#yy[internal] <- rev(yy[!is.tip])[order(xx[!is.tip])]
		#xx[internal] <- rev(yy[!is.tip])[order(xx[!is.tip])]
		}

	edges <- matrix(nrow=0,ncol=2)
	edge.length <- numeric(0)
	nnode <- length(xx)-ntips

	while (sum(unlinked_nodes) > 1)
		{
		## find next node to include (the rightmost internal node)
		nextnode <- which(!is.tip & xx==max(xx[!is.tip]))[1]

		## find daughters
		
		# get the distance (in y) to all of the other corners
		ydist <- yy-yy[nextnode]
		xdist <- xx-xx[nextnode]

		# Check if it's the root
		if ( is.na(cy[nextnode]) )
			{
			cy[nextnode] = yy[nextnode]
			cx[nextnode] = 0			# leftmost coordinate must be 0!
			}
		
		cydist <- yy-cy[nextnode]
		cxdist <- xx-cx[nextnode]

		
		# find the TWO tips closest to this internal node, which are RIGHT of this node
		# this only finds the CLOSEST tip in Y, we want the TWO closest tips!!
		#daughters <- which(is.tip & dist==min(dist[is.tip]))
		
		# rank the ydistances in the y direction
		rank_of_ydists = order(cydist)
		
		# rank the xdistances in the x direction
		rank_of_xdists = order(cxdist)
		
		# get the node numbers in order; delete from this list as they are eliminated
		nodes_to_keep = nodes
		
		# daughter nodes must be to the right (in x) of the nextnode
		# (and they must be unlinked)
		nodes_to_keep = nodes_to_keep[unlinked_nodes][xdist[unlinked_nodes] > 0]
		
		# daughter nodes should be the two closest corners to nextnode (in y, mostly AND x)
		absolute_dist_from_node = 100*abs(cydist[nodes_to_keep]) + 1*abs(cxdist[nodes_to_keep])
		
		# sort the distances
		absolute_dist_from_node_sorted = sort(absolute_dist_from_node)
		
		# take the 2nd smallest absolute distance
		y_abs_dist_tokeep = absolute_dist_from_node_sorted[2]
		
		nodes_to_keep_final = nodes_to_keep[absolute_dist_from_node <= y_abs_dist_tokeep]
		print(paste("Linking: #", nodes_to_keep_final[1], " ", tip.labels[nodes_to_keep_final[1]], ", #", nodes_to_keep_final[2], " ", tip.labels[nodes_to_keep_final[2]], sep=""))
		
		#daughters <- which(is.tip & dist==min(dist[is.tip]))
		daughters = nodes_to_keep_final

		## be careful with numeric fuzz?
		edges <- rbind(edges,
					   nodes[c(nextnode,daughters[1])],
					   nodes[c(nextnode,daughters[2])])
		edge.length <- c(edge.length,xx[daughters]-xx[nextnode])

		# add nextnode to the list of tips (which are not available nodes for the nextnode)
		is.tip[nextnode] <- TRUE

		# remove the daughters & coordinates from the list of available nodes
		unlinked_nodes[daughters] = FALSE
		print(sum(unlinked_nodes))
		
		#xx <- xx[-daughters]
		#yy <- yy[-daughters]

		
		# remove the daughters from the list of possible nodes to link
		#unlinked_nodes
		
		#is.tip <- is.tip[-daughters]
		#nodes <- nodes[-daughters]
		}
	tr <- list(tip.labels=tip.labels,
			 edge=edges,
			 edge.length=edge.length,
			 Nnode=nnode)

	class(tr) <- "phylo"
	tr <- reorder(tr)
	tr$tip.labels = tip.labels
	return(tr)
	}



# This attempts to build the tree without corners.
# This works OK for young nodes, but gets increasingly bad
# with older nodes, unless you have a perfectly symmetrical tree.
build_tree_without_using_corners <- function(xx, yy, tip.labels, poly=numeric(0), debug=FALSE)
	{
	# define the tips
	if (!missing(tip.labels))
		{
		ntips <- length(tip.labels)
		}
	nodes <- 1:length(xx)
	is.tip <- nodes <= ntips
	
	# keep track of the nodes which are unlinked
	unlinked_nodes = rep(TRUE, length(nodes))
	
	if (which.min(xx) != ntips+1)
		{
		## 
		print("ERROR: reorder nodes the way ape/phylo expects! (tips first, then internals in order from left-to-right.")
		#yy[internal] <- rev(yy[!is.tip])[order(xx[!is.tip])]
		#xx[internal] <- rev(yy[!is.tip])[order(xx[!is.tip])]
		}

	edges <- matrix(nrow=0,ncol=2)
	edge.length <- numeric(0)
	nnode <- length(xx)-ntips

	
	


	while (sum(unlinked_nodes) > 1)
		{
		## find next node to include (the rightmost internal node)
		nextnode <- which(!is.tip & xx==max(xx[!is.tip]))[1]

		## find daughters
		
		# get the distance (in y) to all of the other nodes
		ydist <- yy-yy[nextnode]
		xdist <- xx-xx[nextnode]
		
		# find the TWO tips closest to this internal node, which are RIGHT of this node
		# this only finds the CLOSEST tip in Y, we want the TWO closest tips!!
		#daughters <- which(is.tip & dist==min(dist[is.tip]))
		
		# rank the ydistances in the y direction
		rank_of_ydists = order(ydist)
		
		# rank the xdistances in the x direction
		rank_of_xdists = order(xdist)
		
		# get the node numbers in order; delete from this list as they are eliminated
		nodes_to_keep = nodes
		
		# daughter nodes must be to the right (in x) of the nextnode
		# (and they must be unlinked)
		nodes_to_keep = nodes_to_keep[unlinked_nodes][xdist[unlinked_nodes] > 0]
		
		# daughter nodes should be the two closest to nextnode (in y)
		absolute_dist_from_node = abs(ydist[nodes_to_keep])
		
		# sort the distances
		absolute_dist_from_node_sorted = sort(absolute_dist_from_node)
		
		# take the 2nd smallest absolute distance
		y_abs_dist_tokeep = absolute_dist_from_node_sorted[2]
		
		nodes_to_keep_final = nodes_to_keep[absolute_dist_from_node <= y_abs_dist_tokeep]
		print(paste("Linking: #", nodes_to_keep_final[1], " ", tip.labels[nodes_to_keep_final[1]], ", #", nodes_to_keep_final[2], " ", tip.labels[nodes_to_keep_final[2]], sep=""))
		
		#daughters <- which(is.tip & dist==min(dist[is.tip]))
		daughters = nodes_to_keep_final

		## be careful with numeric fuzz?
		edges <- rbind(edges,
					   nodes[c(nextnode,daughters[1])],
					   nodes[c(nextnode,daughters[2])])
		edge.length <- c(edge.length,xx[daughters]-xx[nextnode])

		# add nextnode to the list of tips (which are not available nodes for the nextnode)
		is.tip[nextnode] <- TRUE

		# remove the daughters & coordinates from the list of available nodes
		unlinked_nodes[daughters] = FALSE
		
		#xx <- xx[-daughters]
		#yy <- yy[-daughters]

		
		# remove the daughters from the list of possible nodes to link
		unlinked_nodes
		
		#is.tip <- is.tip[-daughters]
		#nodes <- nodes[-daughters]
		}
	zz <- list(tip.labels=tip.labels,
			 edge=edges,
			 edge.length=edge.length,
			 Nnode=nnode)

	class(zz) <- "phylo"
	zz <- reorder(zz)
	zz$tip.labels = tip.labels
	return(zz)
	}


treethief <- function(xypts)
	{
	
	## from ?plot.tree:
	cat("(((Strix_aluco:4.2,Asio_otus:4.2):3.1,",
		"Athene_noctua:7.3):6.3,Tyto_alba:13.5);",
		file = "ex.tre", sep = "\n")
	tree.owls <- read.tree("ex.tre")
	#plot(tree.owls)
	unlink("ex.tre") # delete the file "ex.tre"
	
	#plot(tree.owls)
	xy <- get("last_plot.phylo",envir=.PlotPhyloEnv)
	xx <- xy$xx
	yy <- xy$yy
	points(xx,yy,col="white",pch=16,cex=2)
	text(xx,yy,col=2,1:length(xx))
	
	
	newtree <- build.tree(xx,yy,tree.owls$tip.label)
	
	data(bird.orders)
	plot(bird.orders,show.node.label=TRUE)
	xy <- get("last_plot.phylo",envir=.PlotPhyloEnv)
	points(xx,yy,col="white",pch=16,cex=2)
	text(xx,yy,col=2,1:length(xx))
	
	xx <- xy$xx
	yy <- xy$yy
	newtree2 <- build.tree(xx,yy,bird.orders$tip.label)
	}








####################################
# charnames_to_nexus_text
####################################
# Take a list of names, add numbers & quotes, to produce NEXUS character matrix input, e.g.:
#
# 		1 'it_is1', 2 'it_is1_blank', 3 'it_is1a', 4 'it_is1b', 5 'it_is1c', 6 not1, 7 'not1_blank', 8 not1a, 9 not1b, 10 the1, 11 'the1_blank', 12 intstr1, 13 'intstr1_blank', 14 intstr1b, 15 intstr1c, 16 intstr2, 17 'intstr2_blank', 18 intstr2b, 19 intstr2c, 20 of1, 21 the2, 22 'species1_blank', 23 species1a, 24 species1b, 25 that1, 26 'that1_blank', 27 survive1, 28 'survive1_blank', 29 survive1a, 30 survive1b, 31 of1, 32 'it_is2', 33 not2, 34 'not2_blank', 35 the3, 36 species2, 37 species2a, 38 species2b, 39 that2, 40 'that2_blank', 41 that2a, 42 survives2a, 43 'survives2_blank', 44 but1, 45 'but1_blank', 46 but1a, 47 rather, 48 'rather_blank', 49 rathera, 50 'it_is3_blank', 51 'it_is3a', 52 'it_is3b', 53 'it_is3c', 54 the4, 55 'the4_blank', 56 species3, 57 'species3_blank', 58 species3a, 59 that3, 60 'that3_blank', 61 survives3, 62 is1, 63 'is1_blank', 64 the5, 65 'the5_blank', 66 one, 67 that4, 68 'that4_blank', 69 is2, 70 'able_best', 71 'able_best_blank', 72 'able_best_a', 73 'able_best_b', 74 to1, 75 adapt1, 76 'adapt1_blank', 77 adapt1a, 78 adapt1b, 79 to2, 80 'to2_blank', 81 and1, 82 'and1_blank', 83 to3, 84 adjust1, 85 'adjust1_blank', 86 adjust1a, 87 adjust1b, 88 best1, 89 'best1_blank', 90 to4, 91 the6, 92 'the6_blank', 93 changing, 94 'changing_blank', 95 'long_end', 96 'long_end_blank', 97 'long_enda', 98 'long_endb', 99 'long_endc', 100 'long_endd', 101 comma1, 102 comma2, 103 comma3, 104 comma4, 105 'intel_before_strong', 106 'General_Field', 107 paraphrase, 108 'Darwin_attribution', 109 'Darwin_subsource_simp', 110 'if_evo_theory', 111 'if_Darwins_theory', 112 'if_Origin', 113 'cited_source' ; 

charnames_to_nexus_text <- function(inarray, quotemark="")
	{
	tmpstr = c()
	for (i in 1 : (length(inarray)-1) )
		{
		tmptmp = paste(i, " ", quotemark, inarray[i], quotemark, ", ", sep="")
		tmpstr = paste(tmpstr, tmptmp, sep="")
		}
	# add the last one, without comma
	tmpstr = paste(tmpstr, i, " ", quotemark, inarray[i+1], quotemark, " ", sep="")
	return(tmpstr)
	}


chars_to_nexus_fmt <- function(charmatrix, namewidth=12)
	{
	outstr = ""
	width = ncol(charmatrix)

	for (i in 1:nrow(charmatrix))
		{
		
		tmpstr1 = as.character(charmatrix$rownames[i])
		len = nchar(tmpstr1, type="chars")
		numspaces_to_add = namewidth - len
		spaces_list = rep(" ", numspaces_to_add)
		tmpstr2 = array_to_text(c(tmpstr1, spaces_list), spacer="")
		
		
		chars = charmatrix[i,2:width]
		tmpstr3 = array_to_text(inarray=chars, spacer="")

		# check for "&" chars: convert 1&2 to (1 2)
		if (grepl("&", tmpstr3))
			{
			for (j in 1:(width-1))
				{
				if (grepl("&", chars[j]))
					{
					char = strsplit(as.character(chars[j]), "&")
					newchars_str = array_to_text(char[[1]], spacer=" ")
					newchar = paste("(", newchars_str, ")", sep="")
					chars[j] = newchar
					cat("Ambiguous character detected & converted...", charmatrix[i,1], ", char #", j, ": ", newchar, "\n")
					}
				}
			tmpstr3 = array_to_text(inarray=chars, spacer="")
			}		
		tmpline = paste("	", tmpstr2, tmpstr3, "\n", sep="") 
		outstr = paste(outstr, tmpline, sep="")
		}
	outstr = trim(outstr)
	#outstr = paste("	", outstr, sep="")
	return(outstr)
	}


charmatrix_to_nexus_file <- function(charmatrix, nexus_outfn, quotemark="")
	{
	# initialize array of strings
	# l = lines
	l = c()
	
	l[1] = "#NEXUS"
	l[2] = "[ written by Nick Matzke R script 'quotescr_v11.R', written 9/1/2010 ]"
	l[3] = ""
	l[4] = "BEGIN TAXA;"
	l[5] = "	TITLE Taxa;"
	l[6] = paste("	DIMENSIONS NTAX=", nrow(charmatrix), ";", sep="")
	l[7] = "TAXLABELS"
	tmpstr = array_to_text(charmatrix$rownames, spacer=" ")
	l[8] = paste("		", tmpstr, sep="")
	l[9] = "	;"
	l[10] = ""
	l[11] = "END;"
	l[12] = ""
	l[13] = ""
	l[14] = "BEGIN CHARACTERS;"
	l[15] = "	TITLE Character_Matrix;"
	l[16] = paste("	DIMENSIONS NCHAR=", (ncol(charmatrix)-1), ";", sep="")
	
	tmpstr = "	FORMAT DATATYPE = STANDARD GAP = - MISSING = ?;"
	
	# maybe we can skip this?
	#SYMBOLS =   0 1 2 3 4 5 6 7 8 9 A B C D";"
	l[17] = tmpstr
	l[18] = "	CHARSTATELABELS "
	tmpstr = charnames_to_nexus_text(inarray = names(charmatrix)[2:ncol(charmatrix)] , quotemark="")
	
	l[19] = paste("		", tmpstr, ";", sep="")
	
	l[20] = "	MATRIX"
	
	# assemble all the characters into one big string
	l[21] = chars_to_nexus_fmt(charmatrix, namewidth=11)
	l[22] = ";"
	l[23] = ""
	l[24] = "END;"
	
	
	
	write.table(l, nexus_outfn, append=FALSE, quote=FALSE, row.names=FALSE, col.names=FALSE)
	headf(nexus_outfn)
	tailf(nexus_outfn)
	
	cat("Character matrix output to NEXUS: '", nexus_outfn, "'\n")
	}


charnames_to_tnt_text <- function(inarray, quotemark="")
	{
	tmpstr = c()
	for (i in 1:length(inarray) )
		{
		tmptmp = paste("{", i-1, " ", quotemark, inarray[i], quotemark, ";\n", sep="")
		tmpstr = paste(tmpstr, tmptmp, sep="")
		}
	return(tmpstr)
	}


chars_to_tnt_fmt <- function(charmatrix, namewidth=12)
	{
	outstr = ""
	width = ncol(charmatrix)

	for (i in 1:nrow(charmatrix))
		{
		
		tmpstr1 = as.character(charmatrix$rownames[i])
		#len = nchar(tmpstr1, type="chars")
		#numspaces_to_add = namewidth - len
		#spaces_list = rep(" ", numspaces_to_add)
		tmpstr2 = array_to_text(c(tmpstr1, "	"), spacer="")
		
		
		chars = charmatrix[i,2:width]
		tmpstr3 = array_to_text(inarray=chars, spacer="")

		# check for "&" chars: convert 1&2 to (1 2)
		if (grepl("&", tmpstr3))
			{
			for (j in 1:(width-1))
				{
				if (grepl("&", chars[j]))
					{
					char = strsplit(as.character(chars[j]), "&")
					newchars_str = array_to_text(char[[1]], spacer=" ")
					newchar = paste("[", newchars_str, "]", sep="")
					chars[j] = newchar
					cat("Ambiguous character detected & converted...", charmatrix[i,1], ", char #", j, ": ", newchar, "\n")
					}
				}
			tmpstr3 = array_to_text(inarray=chars, spacer="")
			}		
		tmpline = paste("", tmpstr2, tmpstr3, "\n", sep="") 
		outstr = paste(outstr, tmpline, sep="")
		}
	outstr = trim(outstr)
	#outstr = paste("", outstr, sep="")
	return(outstr)
	}




charmatrix_to_TNT_file <- function(charmatrix, tnt_outfn, quotemark="")
	{
	# initialize array of strings
	# l = lines
	l = c()
	
	l[1] = "xread"
	l[2] = paste(ncol(charmatrix)-1, nrow(charmatrix)) 

	tmpstr = chars_to_tnt_fmt(charmatrix)
	
	l[3] = tmpstr
	l[4] = ";"

	l[5] = ""
	l[6] = "cnames"
	l[7] = charnames_to_tnt_text( names(charmatrix)[2:ncol(charmatrix)] )
	l[8] = ";"
	l[9] = ""
	l[10] = ""
	l[11] = "proc /;"
	l[12] = "comments 0"
	l[13] = ";"
	l[14] = ""
	l[15] = ""
	
	
	write.table(l, tnt_outfn, append=FALSE, quote=FALSE, row.names=FALSE, col.names=FALSE)
	headf(tnt_outfn)
	tailf(tnt_outfn)
	
	cat("Character matrix output to TNT: '", tnt_outfn, "'\n")
	}




scale_2values_axis <- function(observed_nums, observed_axis_inbreaks, desired_axis)
	{
	xlims_v1_min = min(observed_nums)
	xlims_v1_max = max(observed_nums)
	xlims_min_inbreaks = min(observed_axis_inbreaks)
	xlims_max_inbreaks = max(observed_axis_inbreaks)
	xlims_min_desired = min(desired_axis)
	xlims_max_desired = max(desired_axis)

	x = c(xlims_v1_min, xlims_v1_max)
	y = c(xlims_min_inbreaks, xlims_max_inbreaks)
	reg_result = lm(y~x)
	
	slope = reg_result$coefficients[2]
	intercept = reg_result$coefficients[1]
	
	xlims_touse_min = slope * xlims_min_desired + intercept
	xlims_touse_max = slope * xlims_max_desired + intercept
	
	xlims_touse = c(xlims_touse_min, xlims_touse_max)
	
	return(xlims_touse)
	}


make_multihist <- function(l, desired_xlims, colors, numbreaks, tmptitle, xlabel)
	{
	
	for (i in 1:length(l))
		{
		if (i==1)
			{
			hist(l[[i]], breaks=numbreaks, col=colors[i], border="white", xlim=desired_axis, main=tmptitle, xlab=xlabel, add=FALSE)
			} else {
			hist(l[[i]], breaks=numbreaks, col=colors[i], border="white", xlim=desired_axis, add=TRUE)
			}

		#col=colors, border=c("white"), xlim=xlims_touse, plot.it=FALSE)
		#plot(tmphist, col=colors[i], border="white", xlim=desired_axis)

		#tmphist = hist(l[[i]], breaks=numbreaks, plot=FALSE)
		#plot(tmphist, col=colors[i], border="white", add=TRUE)
		}
	
	#border=c("white"),
	
	#offsets = min(tmphist$breaks)
	#barwidth = tmphist$breaks[1]-tmphist$breaks[0]
	
	#xrange = c(0, max(tmphist$breaks))
	
	
	#barplot(tmphist$out, offset=offsets), width=barwidth, xlim=c(min(xrange), max(xrange)), ylim=c(0, 1.2*max(tmphist$out)))
	#, beside=TRUE, col=colors, xlim=desired_axis, ylim=c(0, 1.2*max(tmphist$out)), bty="o", xaxt="n")
	
	#axis(1, at=pretty(), label=pretty(desired_axis), tick=TRUE, tcl=ticklength, pos=0)	
	}


# Limit each numeric column in a dataframe to e.g. 4 significant digits
signif_digits_df <- function(dtf, numsig=4, printout=FALSE)
	{
	dtf_classes = cls.df(dtf, printout)
	
	for (i in 1:length(dtf_classes$cls_col_list))
		{
		cls_col = dtf_classes$cls_col_list[i]
		if (cls_col == "numeric")
			{
			# Convert numeric column to one with signif digits
			colname = dtf_classes$dtf_names[i]
			cmdstr = paste("dtf$", colname, " = signif(dtf$", colname, ", digits=", numsig, ")", sep="")
			eval(parse(text = cmdstr))				
			}
		}
	return(dtf)
	}


	
read_table_good <- function(fn)
	{
	# Read in the data, store in variable d
	# This has all of Nick's preferred options
	dtf = read.table(fn, header=TRUE, sep="	", quote="", stringsAsFactors = FALSE, strip.white=TRUE, fill=TRUE)
	return(dtf)
	}

write_table_good <- function(dtf, outfn)
	{
	write.table(dtf, file=outfn, append=FALSE, quote=FALSE, sep="	", row.names=FALSE, col.names=TRUE)
	}

write_line_good <- function(dtf, outfn)
	{
	write.table(dtf, file=outfn, append=TRUE, quote=FALSE, sep="	", row.names=FALSE, col.names=TRUE)
	}


write_table_good_w_rownames <- function(dtf, outfn)
	{
	write.table(dtf, file=outfn, append=FALSE, quote=FALSE, sep="	", row.names=TRUE, col.names=TRUE)
	}



is.not.NA <- function(x)
	{
	return(is.na(x) == FALSE)
	}

is.not.na <- function(x)
	{
	return(is.na(x) == FALSE)
	}


# remove NA rows from list
na.remove <- function(x)
	{
	outx = x[is.not.NA(x)]
	return(outx)
	}
	
allneg <- function(inlist)
	{
	x = (inlist <= 0)
	sumneg = sum(x, na.rm=TRUE)
	lenneg = length(na.remove(x))
	if (lenneg == sumneg)
		{
		return(TRUE)
		}
	else
		{
		return(FALSE)
		}
	}

allpos <- function(inlist)
	{
	x = (inlist >= 0)
	sumpos = sum(x, na.rm=TRUE)
	lenpos = length(na.remove(x))
	if (lenpos == sumpos)
		{
		return(TRUE)
		}
	else
		{
		return(FALSE)
		}
	}


remove_NA_rows <- function(dtf, colname)
	{
	cat("\n")
	cat("remove_NA_rows: initial #rows = ", nrow(dtf), "\n")

	cmdstr = paste("rows_to_keep = is.not.NA(dtf$", colname, ")", sep="")
	#print(cmdstr)
	eval(parse(text = cmdstr))
	
	#print(dim(df))
	#print(length(rows_to_keep))
	#print(rows_to_keep)
	
	outdf = dtf[rows_to_keep, ]

	cat("remove_NA_rows: ending #rows = ", nrow(outdf), "\n")
	return(outdf)
	}




# check if all elements match
do_all_match <- function(arr1, arr2)
	{
	matches = (arr1 == arr2)
	if (sum(matches) == length(matches))
		{
		return(TRUE)
		} else {
		return(FALSE)
		}
	}


# split strings on whitespace
strsplit_whitespace <- function(tmpline)
	{
	# split on 1 or more whitespaces
	temp = strsplit(tmpline, "[ \t]+")
	
	# get the list
	list_of_strs = temp[[1]]
	
	# remove any leading/trailing ""
	list_of_strs = list_of_strs[list_of_strs != ""]
	
	return(list_of_strs)
	}


# split strings on whitespace
strsplit_on_tabs_remove_whitespace <- function(tmpline)
	{
	# split on 1 or more whitespaces
	temp = strsplit(tmpline, "[\t]")
	
	# get the list
	list_of_strs = temp[[1]]
	
	# remove any leading/trailing ""
	list_of_strs = list_of_strs[list_of_strs != ""]
	
	for (i in 1:length(list_of_strs))
		{
		tmpstr = list_of_strs[i]
		list_of_strs[i] =  gsub(" ", "", tmpstr)
		}
		
	return(list_of_strs)
	}



# catch "integer(0)" e.g. from non-matching grep
is.numzero <- function(x)
	{
	if (length(x) == 0)
		{
		return(TRUE)
		}
	else
		{
		return(FALSE)
		}
	}

is.inf <- function(x)
	{
	return(x == Inf)
	}


is.not.inf <- function(x)
	{
	return(is.inf(x) == FALSE)
	}


return_items_not_NA <- function(x)
	{
	y = x[is.not.NA(x)]
	return(y)
	}

plot_point <- function(x, y, pt_title, color)
	{
	points(x, y, type="p", pch=19, cex=2, col=color)
	text(x, y, pt_title, col=color, pos=4, bg="white")

	}


# print out the last 5 rows of a data frame
foot <- function(dtf)
	{
	nrows = nrow(dtf)
	print(dtf[((nrows-5) : nrows), ])
	}
	
# print out the header, footer, and basic info on a dataframe (df)
sumdf <- function(dtf)
	{
	print(dim(dtf))
	print(head(dtf))
	foot(dtf)
	
	# print the class of each column
	for (col in 1:ncol(dtf))
		{
		cat("Col#", col, " class = ", class(dtf[1,col]), "\n", sep="")
		}
	
	}


	
make_cdf <- function(list_rel_probs)
	{
	if (sum(list_rel_probs) == 0)
		{
		print("make_cdf() ERROR: sum(list_rel_probs) is ZERO")
		print(list_rel_probs)
		return(NA)
		}
	
	if (class(list_rel_probs) != "data.frame")
		{
		dim(list_rel_probs) = c(length(list_rel_probs), 1)
		list_rel_probs = data.frame(list_rel_probs)
		}
	cdf_rel_probs = rep(0, nrow(list_rel_probs))
	dim(cdf_rel_probs) = c(length(cdf_rel_probs), 1)
	cdf_rel_probs = data.frame(cdf_rel_probs)
	cdf_rel_probs = cbind(cdf_rel_probs, cdf_rel_probs)
	names(cdf_rel_probs) = c("bottom", "top")
	
	for (i in 1:nrow(list_rel_probs))
		{
		if (i==1)
			{
			cdf_rel_probs$bottom[i] = 0
			cdf_rel_probs$top[i] = list_rel_probs[i, 1]
			}
		else
			{
			cdf_rel_probs$bottom[i] = cdf_rel_probs$top[i-1]
			cdf_rel_probs$top[i] = cdf_rel_probs$top[i-1] + list_rel_probs[i, 1]
			}
		}
	
	cdf_rel_probs_new = cdf_rel_probs / sum(list_rel_probs)
	return(cdf_rel_probs_new)
	}


# empirical p-values (for empirical PDF, cdf, percentiles, quantiles)
empirical_pval_fast <- function(x, observed_val)
	{
	num_lt_observed = sum(x <= observed_val)
	
	pval = num_lt_observed / length(x)
	
	return(pval)
	}


# empirical p-values (for empirical PDF, cdf, percentiles, quantiles)
empirical_pval_slow <- function(x, observed_val)
	{
	freqtab = f.freq(x)
	
	pval = freqtab$cumul_fraction[freqtab$x == observed_val]
	
	return(pval)
	}




f.freq <- function(x)
	{
	freqtab <- data.frame(table(x))
	freqtab$fraction <- freqtab$Freq / length(x)
	freqtab$cumul_fraction[1] <- freqtab$fraction[1]
	for(i in 2:length(freqtab[,1]))
		{
		freqtab$cumul_fraction[i] = freqtab$cumul_fraction[i-1] + freqtab$fraction[i]
		}
	return(freqtab)
	}



# Convert unique items to characters
unique_items_to_chars <- function(tempy, y, namestr)
	{
	col_w_name = match(namestr, names(tempy), nomatch=NaN)
	tmpname = names(y)[col_w_name]
	cmdstr = paste("charstates = unique(tempy$", tmpname, ")", sep="")
	eval(parse(text = cmdstr))	
	charstates = charstates[charstates != "?"]
	charindexes = seq(1, length(charstates))
	cat("\n")
	cat(namestr, " - ", "# of states: ", length(charstates), "\n", sep="")
	cat("code	state\n")
	# Display the characters
	for (i in 1:length(charstates))
		{
		#y$General_Field[tempy$General_Field == charstates[i]] = mesquite_character_states[charindexes[i]]
		cat(mesquite_character_states[charindexes[i]], ":	", charstates[i], "\n", sep="")
		cmdstr = paste("y$", tmpname, "[tempy$", tmpname, " == charstates[i]] = mesquite_character_states[charindexes[i]]", sep="")
		eval(parse(text = cmdstr))
		}
	return(y)
	}


# Retrieve chars
# Note: only works if there is a 1-to-1 mapping of codes to text
# 
# inchars_outchars = retrieve_char_text(tempy, y, namestr)
# inchars = inchars_outchars[[1]]
# outchars = inchars_outchars[[2]]
retrieve_char_text <- function(tempy, y, namestr)
	{
	tmpname = namestr
	
	# Get numerical characters
	cmdstr = paste("charstates = unique(tempy$", tmpname, ")", sep="")
	eval(parse(text = cmdstr))	

	# Get text states
	cmdstr = paste("textstates = unique(y$", tmpname, ")", sep="")
	eval(parse(text = cmdstr))	
	
	inchars = c()
	outchars = c()
	for (i in 1:length(charstates))
		{
		charstate = charstates[i]
		
		charstate_index = match(charstate, charstates)
		textstate = textstates[charstate_index]
		
		inchars = c(inchars, charstate)
		outchars = c(outchars, textstate)
		
		}
		
	inchars_outchars = list(inchars, outchars)
	return(inchars_outchars)
	}




print_chars <- function(y, namestr)
	{
	col_w_name = match(namestr, names(y), nomatch=NaN)
	tmpname = names(y)[col_w_name]
	cmdstr = paste("charstates = unique(y$", tmpname, ")", sep="")
	eval(parse(text = cmdstr))	
	cat("\n")
	cat(namestr, " - ", "# of states: ", length(charstates), "\n", sep="")
	cat("List of the character states:\n")
	# Display the characters
	for (i in 1:length(charstates))
		{
		cat(charstates[i], "\n", sep="")
		}	
	}

print_char_key <- function(inchars, outchars)
	{
	cat("\n")
	cat("Printing codes for characters:\n")
	for (i in 1:length(inchars))
		{
		cat(outchars[i], ": ", inchars[i], "\n", sep="")
		}
	}

print_chars_catlist <- function(y, namestr)
	{
	mesquite_character_states = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "K", "M", "N", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "l", "m", "n", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

	
	col_w_name = match(namestr, names(y), nomatch=NaN)
	tmpname = names(y)[col_w_name]
	cmdstr = paste("charstates = unique(y$", tmpname, ")", sep="")
	eval(parse(text = cmdstr))	
	cat("\n")
	cat(namestr, " - ", "# of states: ", length(charstates), "\n", sep="")
	cat("\n")
	cat("List of the character states:\n")


	# Display the characters -- text input
	cat("inchars = c(")
	for (i in 1:(length(charstates)-1))
		{
		cat('"', charstates[i], '", \n', sep="")
		}	
	cat('"', charstates[length(charstates)], '")\n', sep="")
	cat("\n")

	# Display the characters -- suggested coded output
	cat("outchars = c(")
	charcount = 0
	for (i in 1:(length(charstates)-1))
		{
		if (charstates[i] != "?")
			{
			charcount = charcount + 1
			cat('"', mesquite_character_states[charcount], '", \n', sep="")
			}
		else
			{
			cat('"?"', ', \n', sep="")			
			}
		}	
	if (charstates[length(charstates)] != "?")
		{
		charcount = charcount + 1
		cat('"', mesquite_character_states[charcount], '")\n', sep="")
		}
	else
		{
		cat('"?"', ')\n', sep="")			
		}
	cat("\n")


	}


convert_items_to_items <- function(y, namestr, name1list, name2list)
	{
	for (i in 1:length(name1list))
		{
		name1 = name1list[i]
		name2 = name2list[i]
		y = convert_item_to_item(y, namestr, name1, name2)
		}
	return(y)
	}


convert_item_to_item <- function(y, namestr, name1, name2)
	{
	col_w_name = match(namestr, names(y), nomatch=NaN)
	tmpname = names(y)[col_w_name]
	cmdstr = paste("y$", tmpname, "[y$", tmpname, ' == "', name1, '"] = "', name2, '"', sep="")
	print(cmdstr)
	eval(parse(text = cmdstr))	
	return(y)
	}

convert_grepl_item_to_item <- function(y, namestr, name1, name2)
	{
	col_w_name = match(namestr, names(y), nomatch=NaN)
	tmpname = names(y)[col_w_name]
	cmdstr = paste("rows_to_change = grepl(name1, y$", tmpname, ")", sep="")
	eval(parse(text = cmdstr))
	cmdstr = paste("y$", tmpname, "[rows_to_change] = '", name2, "'", sep="")
	print(cmdstr)
	eval(parse(text = cmdstr))	
	return(y)
	}



# Extract rows with indices matching rows_to_keep
gather_rows_with_indices <- function(tmptable, rows_to_keep)
	{
	# make empty matrix
	newtable = matrix(data=NA, nrow = length(rows_to_keep), ncol=ncol(tmptable))
	names(newtable) = names(tmptable)
	for (i in 1:nrow(newtable))
		{
		# insert the new row 
		newtable = tmptable[rows_to_keep[i], ]
		}
	return(newtable)
	}

# remove rows from temp_table, based on col matching list_to_remove
remove_rows_with_list_to_remove <- function(temp_table, col, list_to_remove)
	{
	# get TRUE/FALSE for which rows in species col are undefined, according to list_to_remove
	truefalse_rows_match = match_list1_in_list2(col, list_to_remove)
	cat("remove_rows_with_list_to_remove(): initial table #rows = ", nrow(temp_table), "\n")
	
	cat("Removing ", (sum(truefalse_rows_match)), " rows with c(", list2str(c(list_to_remove)), ") in them.\n", sep="")
	temp_table = temp_table[truefalse_rows_match == FALSE, ]
	cat("remove_rows_with_list_to_remove(): revised table #rows = ", nrow(temp_table), "\n")
		
	return(temp_table)
	}	


list2str <- function(list1, spacer=" ")
	{
	
	for (i in 1:length(list1))
		{
		if (i == 1)
			{
			tmpstr = as.character(list1[1])
			if (length(list1) == 1)
				{
				return(tmpstr)
				}
			next
			}
		addstr = as.character(list1[i])
		tmpstr = paste(tmpstr, addstr, sep=spacer)
		}
	return(tmpstr)
	}


list2str_unlist <- function(list1, spacer="	")
	{
	outlist = c("startlist")
	for (i in 1:length(list1))
		{
		listitem = list1[i]
		if (length(listitem) > 1)
			{
			outlist = append(outlist, list2str(unlist(listitem), spacer="	"))
			}
		else
			{
			addstr = as.character(list1[i])
			#tmpstr = paste(tmpstr, addstr, sep=spacer)
			outlist = append(outlist, addstr)
			}
		}
	tmpstr = list2str(outlist, spacer="	")
	return(tmpstr)
	}


return_unlist <- function(list1, spacer="	")
	{
	outlist = c("startlist")
	for (i in 1:length(list1))
		{
		listitem = list1[i]
		if (length(listitem) > 1)
			{
			outlist = append(outlist, unlist(listitem), spacer="	")
			}
		else
			{
			addstr = as.character(list1[i])
			#tmpstr = paste(tmpstr, addstr, sep=spacer)
			outlist = append(outlist, addstr)
			}
		}
	
	return(outlist)
	}


unlist_dtf_cols <- function(dtf, printflag=FALSE)
	{
	# Sometimes cbind makes each column a list, this can screw up use/searching of
	#  the column later on.  
	# Unlist each column...
	for (i in 1:ncol(dtf))
		{
		tmpstr = paste("unlisting col: ", names(dtf)[i], "...", sep="")
		prflag(tmpstr, printflag=printflag)		
		
		# catch a possible error from unlisting
		# the number of rows needs to stay the same!!
		tmpcol = unlist(dtf[, i])
		if (length(tmpcol) != length(dtf[, i]))
			{
			tmpstr = paste("...failed! unlist(col) length=", length(tmpcol), "; nrow(dtf) = ", nrow(dtf), sep="")
			prflag(tmpstr, printflag=printflag)
			} 
		else
			{
			dtf[, i] = tmpcol
			tmpstr = paste(" ", " ", sep="")
			prflag(tmpstr, printflag=printflag)
			}
		}
	return(dtf)
	}




char_descriptions_to_NEXUS <- function(namestr_list, inchars_list, outchars_list)
	{
	# Create l, an empty list of lines...
	l = list()
	
	#l[(h=h+1)] = "	CHARSTATELABELS"
	
	tmpcharstr_list = c()
	for (i in 1:length(namestr_list))
		{
		tmpname = namestr_list[i]

		# filter out ? and ''
		inchars = inchars_list[[i]]
		outchars = outchars_list[[i]]

		chars_to_keep1 = outchars != ""
		chars_to_keep2 = outchars != "?"
		
		# remove the character states which are "" or "?" in the coded section
		chars_to_keep = ((chars_to_keep2 + chars_to_keep1) == 2)
		inchars2 = inchars[chars_to_keep]

		oldstr = "'"
		newstr = ""	
		tmp_charstates_list_noapost = gsub(oldstr, newstr, inchars2)

		oldstr = " "
		newstr = "_"	
		tmp_charstates_list_nospace = gsub(oldstr, newstr, tmp_charstates_list_noapost)

		
		#tmp_charstates_str = list2str(inchars_list[[i]], spacer='" "')
		tmp_charstates_str = list2str(tmp_charstates_list_nospace, spacer="' '")
		
		tmp_charstates_str2 = paste("'", tmp_charstates_str, "'", sep="")
		
		tmpcharstr = paste(i, tmpname, "/ ", tmp_charstates_str2)
		
		tmpcharstr_list = c(tmpcharstr_list, tmpcharstr)
		}
	
	bigstr_chardescs = list2str(tmpcharstr_list, spacer=", ")
	charstatelabels = paste("		", bigstr_chardescs, " ;", sep="")
	
	
	return(charstatelabels)
	}


# Add character state labels to a preexisting NEXUS file...
add_charstatelabels_to_NEXUS <- function(charstatelabels, infn, outfn)
	{
	lines = scan(file=infn, what="character", sep="\n")
	
	# Go through lines
	line_with_charstatelabels = NULL
	
	for (i in 1:length(lines))
		{
		# Get line
		line = lines[i]
		
		# Check if line matches CHARSTATELABELS
		if (grepl("CHARSTATELABELS", line))
			{
			line_with_charstatelabels = i
			}
		else
			{
			blah=TRUE
			}
		}
	
	
	# Assuming you found line_with_charstatelabels...
	lines[line_with_charstatelabels+1] = charstatelabels
	
	write.table(lines, file=outfn, quote=FALSE, append=FALSE, sep="", row.names = FALSE, col.names=FALSE)
	
	return(lines)
	
	}



count_rows_NOT_containing_substr_in_col <- function(dtf, col, tmp_substr)
	{
	list_to_kill = grepl(tmp_substr, col, fixed=TRUE)
	list_to_keep = (list_to_kill == FALSE)
	print(sum(list_to_keep))
	return(sum(list_to_keep))
	}

count_rows_containing_substr_in_col <- function(dtf, col, tmp_substr)
	{
	list_to_keep = grepl(tmp_substr, col, fixed=TRUE)
	print(sum(list_to_keep))
	return(sum(list_to_keep))
	}



remove_rows_containing_substr_in_col <- function(dtf, col, tmp_substr)
	{
	list_to_kill = grepl(tmp_substr, col, fixed=TRUE)
	list_to_keep = (list_to_kill == FALSE)
	return(dtf[list_to_keep, ])
	}


retain_rows_containing_substr_in_col <- function(dtf, col, tmp_substr)
	{
	list_to_keep = grepl(tmp_substr, col, fixed=TRUE)
	return(dtf[list_to_keep, ])
	}


extract_rows_with_col_matching_findthis_str <- function(temp_table, col, findthis)
	{
	# Extract North-America-specific modern extinctions
	matches = find_str_inlist(findthis, tmplist=col)
	cat("# of rows matching '", findthis, "': ", sum(matches), "\n", sep="")
	
	temp_table = temp_table[matches, ]
	return(temp_table)
	}


# Find a string inside another string...
find_instring <- function(findthis, string)
	{
	result = grep(findthis, string)
	if (length(result) > 0)
		{
		#print("match")
		match = TRUE
		}
	else
		{
		match = FALSE
		}
	return(match)
	}

find_str_inlist <- function(findthis, tmplist)
	{
	# make empty list
	outlist = rep(NA, length(tmplist))
	for (i in 1:length(tmplist))
		{
		string = tmplist[i]
		outlist[i] = find_instring(findthis, string)
		
		}
	return(outlist)
	}

# return matching TRUE/FALSE values
# list1 (.e.g. a big list) TRUE if it is found in list2 (e.g. a smaller list)
match_list1_in_list2 <- function(list1, list2)
	{
	matchlist = list1 %in% list2
	return(matchlist)
	}


compare_2cols_against_another_2cols_find_matching_pairs_in_list2 <- function(list1, list2)
	{
	list1_paste = paste(list1[,1], ",", list1[,2], sep="")
	list2_pasteA = paste(list2[,1], ",", list2[,2], sep="")
	list2_pasteB = paste(list2[,1], ",", list2[,2], sep="")
	
	list2_matches1 = match_list1_in_list2(list2_pasteA, list1_paste)
	list2_matches2 = match_list1_in_list2(list2_pasteB, list1_paste)
	
	list2_matches = list2_matches1 + list2_matches2 > 0
	return(list2_matches)
	}



# NOTE!!! THESE MATCH FUNCTIONS JUST RETURN THE *FIRST* MATCH, *NOT* ALL MATCHES
# (argh)
# return indices in 2nd list matching the first list
# It WILL return one match for each item in the list, though...
get_indices_where_list1_occurs_in_list2 <- function(list1, list2)
	{
	match_indices = match(list1, list2)
	return(match_indices)
	}


get_indices_where_string_in_list1_occurs_in_list2 <- function(list1, list2)
	{
	match_indices = unlist(lapply(list1, get_index_in_list2_matching_string, list2))
	match_indices[is.na(match_indices) == TRUE] = ""
	return(match_indices)
	}

# returns the index of the FIRST hit
get_index_in_list2_matching_string <- function(tmpstr, list2)
	{
	matches_TF = lapply(tmpstr, grepl, list2)[[1]]
	first_hit_index = match(TRUE, matches_TF)
	return(first_hit_index)
	}


# return indices in 2nd list matching the first list
get_indices_where_list1_occurs_in_list2_noNA <- function(list1, list2)
	{
	match_indices = match(list1, list2)
	match_indices = return_items_not_NA(match_indices)
	return(match_indices)
	}

# this is f-ed up!!
get_real_indices_where_list1_occurs_in_list2 <- function(list1, list2)
	{
	print("get_real_indices_where_list1_occurs_in_list2()")
	print("WARNING: THIS FUNCTION RETURNS THE INDICES OF NOT NA, NOT THE INDICES OF THE MATCHES")
	indices = 1:length(list1)
	
	matches_or_NAs = get_indices_where_list1_occurs_in_list2(list1, list2)
	not_NA = is.not.NA(matches_or_NAs)
	indices_to_return = indices[not_NA]
	return(indices_to_return)
	}


# place items with string matching list2 in new column of size list2
#place_items_matching_str_in_list1_in_newcol <- function()



# colmax, max.col

colmax <- function(dtf)
	{
	maxvals = apply(dtf, 2, max, na.rm=TRUE)
	return(maxvals)
	}

colmin <- function(dtf)
	{
	minvals = apply(dtf, 2, min, na.rm=TRUE)
	return(minvals)
	}

normalize_by_colmax <- function(dtf)
	{
	maxvals = colmax(dtf)
	# this outputs the results into columns, instead of corresponding rows 
	x = apply(dtf, 1, "/", maxvals)
	
	# so transpose
	normalized_df = t(x)
	normalized_df = as.data.frame(normalized_df)
	names(normalized_df) = names(dtf)
	
	return(normalized_df)
	}



##############################################
# Matrix functions
##############################################
# symmetric flip across the diagonal of a matrix
flipdiag <- function(mat)
	{
	newmat = mat
	for (i in ncol(mat))
		{
		for (j in nrow(mat))
			{
			newmat[j,i] = mat[i,j]
			}
		}
	return(newmat)
	}


nondiagTF <- function(m)
	{
	# Returns TF for non-diagonals
	diag(m) = "blah"
	
	TF = m != "blah"
	
	return(TF)
	}

nondiag <- function(m)
	{
	# Returns non-diagonals
	nondiag_m = m[nondiagTF(m)]
	
	return(nondiag_m)
	}



# Compare lower and upper diagonals
compare_lower_upper_diag <- function(x)
	{
	xtri = x[lower.tri(x)]
	
	ytmp = flipdiag(x)
	ytri = ytmp[lower.tri(ytmp)]
	
	(cor(cbind(xtri, ytri)))
	dif = c(mean(xtri))-c(mean(ytri))
	prstr = paste(mean(c(xtri)), mean(c(ytri)), dif, sep=" ")
	print(prstr)
	
	plot(xtri, ytri, type="p", pch=".")
	
	return(dif)
	}

# Compare row/col
compare_row_col <- function(x, rownum)
	{
	xnums = c(x[rownum, ])
	ynums = c(x[, rownum])
	
	(cor(cbind(xnums, ynums)))
	dif = c(mean(xnums))-c(mean(ynums))
	prstr = paste(mean(c(xnums)), mean(c(ynums)), dif, sep=" ")
	print(prstr)
	
	plot(xnums, ynums, type="p", pch=".")
	
	return(dif)
	}


# Find rows contains
# (note that this 
findrows_containing <- function(x, thing_to_find)
	{
	rows_to_keep = 1:nrow(x)
	for (i in 1:nrow(x))
		{
		if(sum(x[i, ] == thing_to_find, na.rm = TRUE) > 0)
			{
			rows_to_keep[i] = TRUE
			}
		else
			{
			rows_to_keep[i] = FALSE
			}
		}
	return(rows_to_keep)
	}


# Find rows containing no NAs
findrows_containing_noNA <- function(x)
	{
	rows_to_keep = is.na(rowSums(x))
	return(rows_to_keep == FALSE)
	}


# Find rows containing no NAs
findrows_containing_noINF <- function(x)
	{
	rows_to_keep = is.inf(rowSums(x))
	return(rows_to_keep == FALSE)
	}

# Find rows containing no NAs, no Infs
return_rows_containing_noNA_noInf <- function(x)
	{
	rows_to_keep = 1:nrow(x)
	rows_to_keep = findrows_containing_noNA(x)
	y = x[rows_to_keep, ]
	rows_to_keep = findrows_containing_noINF(y)	
	z = y[rows_to_keep, ]
	return(z)
	
	}


##############################################
# Plotting functions
##############################################

# Make some subplots
subplots <- function(nrows=1, ncols=2)
	{
	q = quartz(width=7*ncols, height=7*nrows)
	par(mfcol = c(nrows, ncols))
	return(q)
	}


# Pairs plot with histograms
panel.hist <- function(x, ...)
	{
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
	}

pairs_with_hist <- function(datamat, ...)
	{
	pairs(datamat, diag.panel=panel.hist, ...)
	return()
	}





# =========================================================
# Google-related functions
# =========================================================
# Copy the corrected google numbers to each matching quote 
# (one could also split the results amongst the matching quotes; see next function)
copy_fixed_quantchars_to_each_matching <- function(quantchars, quotes_list_no_extra_spaces_to_google, fixed_goog, cols_in_fixed_goog, unique_quotes_to_google, divide_by_frequency = TRUE)
	{
	quantchar_copy_nums_to_repeated_quotes = NULL
	q = NULL
	
	# the full list of quotes (from whatever source; full quantchars dataset, or subset)
	rownames = quantchars$rownames
	quotetxt = quotes_list_no_extra_spaces_to_google
	
	numcols = ncol(quantchars) - 1
	tmprow = rep(NA, numcols)
	
	# store the accumulating rows here
	tmpmat = NULL

	names_cols_in_fixed_goog = names(fixed_goog)[cols_in_fixed_goog]
	
	# default (no spreading out of Google Hits across multiple hits of quote)
	divide_by_factor = 1
	for (i in 1:nrow(quantchars))
		{
		tmp_quotetxt_in_bigmatrix_list = c(quotetxt[i])
		tmp_quotetxt_in_bigmatrix = quotetxt[i]
		matching_unique_quotetxt_row = get_indices_where_list1_occurs_in_list2(tmp_quotetxt_in_bigmatrix_list, unique_quotes_to_google)
		#print(matching_unique_quotetxt_row)
		
		# get number of other guys showing that match in the big list
		num_in_biglist_matching = sum(quotes_list_no_extra_spaces_to_google == tmp_quotetxt_in_bigmatrix)
		print(num_in_biglist_matching)
		
		# if you do want to change the numbers by dividing equally amongst multiple hits of the quote,
		# do so here...
		if (divide_by_frequency == TRUE)
			{
			divide_by_factor = num_in_biglist_matching
			} else {
			pass = "blah"
			}

		tmprow = as.numeric(fixed_goog[matching_unique_quotetxt_row, cols_in_fixed_goog]) / divide_by_factor
		numchars = nchar(tmp_quotetxt_in_bigmatrix)
		numwords = length(strsplit(tmp_quotetxt_in_bigmatrix, " ")[[1]])
		
		tmprow2 = c(as.data.frame(numchars), as.data.frame(numwords), tmprow)
		tmpmat = rbind(tmpmat, tmprow2)
		}
	tmpmat2 = cbind(as.data.frame(rownames), as.data.frame(quotetxt), as.data.frame(tmpmat, row.names=1:nrow(tmpmat)))

	outdf = as.data.frame(tmpmat2, row.names=1:nrow(tmpmat2))
	names(outdf) = c("rownames", "quotetxt", "numchars", "numwords", names_cols_in_fixed_goog)

	quantchar_copy_nums_to_repeated_quotes = outdf
	return(quantchar_copy_nums_to_repeated_quotes)

	}


# If a list has [[1]], [[2]], or is a dataframe, kill that with this
get_items_from_nested_list <- function(nested_list)
	{
	tmpout = c()
	for (i in 1:length(nested_list))
		{
		item = nested_list[[i]]
		tmpout = c(tmpout, item)
		}
	return(tmpout)
	}

# If a list has [[1]], [[2]], or is a dataframe, kill that with this
get_items_from_nonnested_list <- function(nested_list)
	{
	tmpout = c()
	for (i in 1:length(nested_list))
		{
		item = nested_list[i]
		tmpout = c(tmpout, item)
		}
	return(tmpout)
	}


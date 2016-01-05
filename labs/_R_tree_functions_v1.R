# =============================================
# _R_tree_functions_v1.R: many useful utility functions
#   for dealing with phylogenetic trees in R
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
#   * to Share Ñ to copy, distribute and transmit the work
#   * to Remix Ñ to adapt the work
#
# Under the following conditions:
#
#   * Attribution Ñ You must attribute the work in the manner 
#     specified by the author or licensor (but not in any way that 
#     suggests that they endorse you or your use of the work).
#   * Noncommercial Ñ You may not use this work for commercial purposes. 
#
#   * Share Alike Ñ If you alter, transform, or build upon this work,
#     you may distribute the resulting work only under the same or
#     similar license to this one. 
#
# http://creativecommons.org/licenses/by-nc-sa/3.0/
# 
###################################################################

# R functions for dealing with trees
#sourcedir = '/_njm/'
#source3 = '_R_tree_functions_v1.R'
#source(paste(sourcedir, source3, sep=""))



fixtree <- function(tree)
	{
	# Write a tree to Newick, then read back in
	# (hopefully this fixes whatever formatting issue was a problem)
	tmpfn = "/_njm/tmp_junktree.tree"
	#write.tree(tree, tmpfn)
	#newtree2 = read.tree(tmpfn)
	
	newtree2 = write.tree(tree)
	
	return(newtree2)
	}

get_newick_str <- function(tree)
	{
	write.tree(tree, "temp.newick")
	str1 = read.table("temp.newick", skip=2)
	str2 = str1[1,1]
	return(str2)
	}


dtf_to_phyDat <- function(char_dtf, datatype="USER")
	{
	# convert columns to character
	d = apply(char_dtf, 2, as.character)
	d2 = as.matrix(d)
	tmplevels = as.character(sort(unique(unlist(char_dtf))))
	phyd = phyDat(d2, type="USER", levels=tmplevels)
	attr(phyd, "names") = row.names(char_dtf)
	
	return(phyd)
	}

phyDat_to_str_dtf <- function(phyDat_seqs)
	{
	seqs2 = as.character(phyDat_seqs)
	
	(seqs3 = apply(seqs2, 1, paste, collapse=""))
	
	seqs4 = adf(seqs3)
	names(seqs4) = c("data")
	row.names(seqs4) = names(phyDat_seqs)
	
	return(seqs4)
	}

phyDat_to_seqlist <- function(phyDat_seqs)
	{
	# Get the length (number of taxa)
	numseqs = length(phyDat_seqs)
	
	# convert to character
	seqs2 = as.character(phyDat_seqs)
	tmp_rownames = row.names(seqs2)
	
	# for each sequence, make it a character list
	seqs3 = vector("list", length=numseqs)
	
	for (i in 1:numseqs)
		{
		seqs3[[i]] = seqs2[i, ]
		}
	
	# Add the names
	attr(seqs3, "names") = tmp_rownames
	
	return(seqs3)
	}


phyDat_to_nexus_good <- function(phyDat_seqs, outfn="tmpnex.nex", printflag=FALSE)
	{
	numseqs = length(phyDat_seqs)
	
	# convert sequences to sequence list, with names as taxa names
	seqs3 = phyDat_to_seqlist(phyDat_seqs)

	# write to NEXUS using Nick's function, not default	
	write.nexus.data(seqs3, file=outfn, format="DNA", datablock = FALSE, interleaved=FALSE, gap = "-", missing = "?")
	
	if (printflag == TRUE)
		{
		moref(outfn)
		}
	
	printstr = paste("phyDat_to_nexus_good(): ", numseqs, " sequences written to ", outfn, "\n", sep="")
	cat(printstr)

	return(outfn)
	}


dtf_to_phylip <- function(char_dtf, outfn = "tmpdata.phylip")
	{
	# write a data.frame to phylip
	# you may want to put columns with different numbers of states into different columns!!
	
	# convert columns to character
	d = apply(char_dtf, 2, paste)
	d2 = as.data.frame(d)
	tmplevels = as.character(sort(unique(unlist(char_dtf))))
	numcols = ncol(d2)
	
	
	str1 = paste(nrow(d2), numcols, sep=" ")
	write(str1, file=outfn, ncolumns=1, append=FALSE, sep="")
	
	for (i in 1:nrow(d2))
		{
		tmpname = row.names(char_dtf)[i]
		numchars = nchar(tmpname)
		if(numchars > 9)
			{
			tmpname = strtrim(tmpname, 9)
			numspaces_to_add = 0
			} else {
			numspaces_to_add = 9-numchars
			}
		
		spaces_to_add = paste(rep(" ", numspaces_to_add), collapse="")
		
		charstring = paste(d2[i, ], collapse="")
		outrowstr = paste(tmpname, spaces_to_add, " ", charstring, sep="")
		
		write(outrowstr, file=outfn, ncolumns=1, append=TRUE, sep="")
		}

	return(outfn)	
	}


resample_dtf_matrix <- function(char_dtf)
	{
	nrows = nrow(char_dtf)
	new_dtf = apply(char_dtf, 2, sample, size=nrows, replace=FALSE)
	row.names(new_dtf) = row.names(char_dtf)
	return(new_dtf)
	}




# assign node labels in bottom-up, left-first format (as in e.g. rates)
nodenums_bottom_up <- function(tr)
	{
	tr4 = as(tr, "phylo4")
	
	numnodes = nNodes(tr4) + nTips(tr4)
	newnodes = matrix(NA, nrow=numnodes, ncol=1)
	
	# Find root row
	rootnode = nodeId(tr4, type="root")
	#newnodes[rootnode] = 1
	
	startnode = rootnode
	childnum = 0
	traverse_records = c()
	traverse_records$childnum = 0
	traverse_records$newnodes = newnodes
	traverse_records = traverse_up(tr4, startnode, traverse_records)
	
	newnodes = traverse_records$newnodes
	old2new_nodes_table = cbind(1:numnodes, newnodes)
	old2new_nodes_table = adf(old2new_nodes_table)
	names(old2new_nodes_table) = c("orig_nodenums", "LR_nodenums")
	
	return(old2new_nodes_table)
	}

# convert a prt_tree into a phylo4 object
prt_tree_to_phylo4 <- function(prt_tr)
	{
	newtr = cbind(prt_tr$label, prt_tr$node, prt_tr$ancestor, prt_tr$edge.length, prt_tr$node.type)
	newtr = adf(newtr)
	names(newtr) = c("label", "node", "ancestor", "edge.length", "node.type")
	
	return(newtr)
	}

# traverse the tree from node up to the tips
traverse_up <- function(tr4, startnode, traverse_records)
	{
	tmp_children = children(tr4, startnode)
	traverse_records$childnum = traverse_records$childnum + 1
	traverse_records$newnodes[startnode] = traverse_records$childnum

	# print if desired
	#cat(paste(startnode, "	", traverse_records$childnum, "\n", sep=""))
	
	# if at a tip, return down the tree
	if (length(tmp_children) == 0)
		{
		return(traverse_records)
		}
	
	for (child in tmp_children)
		{	
		traverse_records = traverse_up(tr4, child, traverse_records)
		}
	return(traverse_records)
	}


steps_dtf <- function(tree, char_dtf, by_char=FALSE)
	{
	# Calculate CI, given a tree and a data.frame
	# Currently using default (Sankoff) parsimony, but on equal weights
	# From IB200a notes:
	# ci = m/s  -----where m = minimum number of steps in a character (number of states -1) 
	#       s = steps actually realized on a given tree 

	
	# For CI by-character
	if (by_char == TRUE)
		{
		ncols = ncol(char_dtf)
		steps_list = rep(NA, ncols)
		
		for (i in 1:ncols)
			{
			# Get one column
			char_col = adf(char_dtf[,i])
			row.names(char_col) = row.names(char_dtf)
			
			# actual steps for that column
			phyd = dtf_to_phyDat(char_col)
			steps = parsimony(tree, phyd)
			
			steps_list[i] = steps
			}
		
		return(steps_list)
		}
	
	# actual steps (Sankoff parsimony)
	phyd = dtf_to_phyDat(char_dtf)
	steps = parsimony(tree, phyd)

	return(steps)	
	}


g_dtf <- function(tree, char_dtf, by_char=FALSE)
	{
	# polytomize tree to a star
	biggest_brlen = max(tree$edge.length)
	star_tree = di2multi(tree, tol=(biggest_brlen+1) )

	# calculate m
	minsteps_list = apply(char_dtf, 2, function(x) length(unique(x)))

	
	# For CI by-character
	if (by_char == TRUE)
		{
		ncols = ncol(char_dtf)
		g_list = rep(NA, ncols)
		
		for (i in 1:ncols)
			{
			# Get one column
			char_col = adf(char_dtf[,i])
			row.names(char_col) = row.names(char_dtf)
			
			# actual steps for that column
			phyd = dtf_to_phyDat(char_col)
			
			# g for that column
			g = parsimony(star_tree, phyd)
			g_list[i] = g
			}
		
		return(g_list)
		}
	
	# actual steps (Sankoff parsimony)
	phyd = dtf_to_phyDat(char_dtf)

	# g for that column
	g = parsimony(star_tree, phyd)

	return(g)
	}


minsteps_dtf <- function(tree, char_dtf, by_char=FALSE)
	{
	# calculate m
	minsteps_list = apply(char_dtf, 2, function(x) length(unique(x)))
	
	if (by_char == TRUE)
		{
		return(minsteps_list)
		}
	
	return(sum(minsteps_list))
	}


CI_dtf <- function(tree, char_dtf, by_char=FALSE)
	{
	# Calculate CI, given a tree and a data.frame
	# Currently using default (Sankoff) parsimony, but on equal weights
	# From IB200a notes:
	# ci = m/s  -----where m = minimum number of steps in a character (number of states -1) 
	#       s = steps actually realized on a given tree 

	
	# calculate m
	minsteps_list = apply(char_dtf, 2, function(x) length(unique(x)))
	
	# For CI by-character
	if (by_char == TRUE)
		{
		ncols = ncol(char_dtf)
		steps_list = rep(NA, ncols)
		
		for (i in 1:ncols)
			{
			# Get one column
			char_col = adf(char_dtf[,i])
			row.names(char_col) = row.names(char_dtf)
			
			# actual steps for that column
			phyd = dtf_to_phyDat(char_col)
			steps = parsimony(tree, phyd)
			
			steps_list[i] = steps
			}
		
		CIvals = minsteps_list / steps_list
		return(CIvals)
		}
	
	# For CI totals
	minsteps = sum(minsteps_list)
	
	# actual steps (Sankoff parsimony)
	phyd = dtf_to_phyDat(char_dtf)
	steps = parsimony(tree, phyd)

	# calculate CI
	CIval = minsteps/steps
	return(CIval)	
	}



RI_dtf <- function(tree, char_dtf, by_char=FALSE)
	{
	# From IB200a
	# ri = (g Ð s)/(g Ð m)  ------where g= minimum steps on the worst tree (=bush) 
	
	# polytomize tree to a star
	biggest_brlen = max(tree$edge.length)
	star_tree = di2multi(tree, tol=(biggest_brlen+1) )

	# calculate m
	minsteps_list = apply(char_dtf, 2, function(x) length(unique(x)))

	
	# For CI by-character
	if (by_char == TRUE)
		{
		ncols = ncol(char_dtf)
		steps_list = rep(NA, ncols)
		g_list = rep(NA, ncols)
		
		for (i in 1:ncols)
			{
			# Get one column
			char_col = adf(char_dtf[,i])
			row.names(char_col) = row.names(char_dtf)
			
			# actual steps for that column
			phyd = dtf_to_phyDat(char_col)
			steps = parsimony(tree, phyd)
			steps_list[i] = steps
			
			# g for that column
			g = parsimony(star_tree, phyd)
			g_list[i] = g
			}
		
		
		RIvals = (g_list - steps_list) / (g_list - minsteps_list)
		return(RIvals)
		}
	
	# For RCI totals
	minsteps = sum(minsteps_list)
	
	# actual steps (Sankoff parsimony)
	phyd = dtf_to_phyDat(char_dtf)
	steps = parsimony(tree, phyd)

	# g for that column
	g = parsimony(star_tree, phyd)

	# calculate CI
	RIval = (g - steps) / (g - minsteps)
	return(RIval)
	}

parsim_stats_fast <- function(tree, char_dtf, by_char=TRUE)
	{
	# From IB200a
	# ri = (g Ð s)/(g Ð m)  ------where g= minimum steps on the worst tree (=bush) 
	
	# polytomize tree to a star
	biggest_brlen = max(tree$edge.length)
	star_tree = di2multi(tree, tol=(biggest_brlen+1) )

	# calculate m
	minsteps_list = apply(char_dtf, 2, function(x) length(unique(x))-1)

	
	# For CI by-character
	if (by_char == TRUE)
		{
		ncols = ncol(char_dtf)
		steps_list = rep(NA, ncols)
		g_list = rep(NA, ncols)
		
		for (i in 1:ncols)
			{
			# Get one column
			char_col = adf(char_dtf[,i])
			row.names(char_col) = row.names(char_dtf)
			
			# actual steps for that column
			phyd = dtf_to_phyDat(char_col)
			steps = parsimony(tree, phyd)
			steps_list[i] = steps
			
			# g for that column
			g = parsimony(star_tree, phyd)
			g_list[i] = g
			}
		
		
		#RIvals = (g_list - steps_list) / (g_list - minsteps_list)
		
		parsim_counts = adf(cbind(minsteps_list, g_list, steps_list))
		names(parsim_counts) = c("minstep", "g", "nstep")
		
		parsim_counts_ttls = colSums(parsim_counts)
		parsim_counts = rbind(parsim_counts, parsim_counts_ttls)
		
		CI = round(parsim_counts$minstep / parsim_counts$nstep, 3)
		RI =  round((parsim_counts$g - parsim_counts$nstep) / (parsim_counts$g - parsim_counts$minstep), 3)
		RCI =  round( RI * CI, 3)
		
		parsim_counts = cbind(parsim_counts, CI, RI, RCI)
		
		row.names(parsim_counts)[nrow(parsim_counts)] = "total"
		
		return(parsim_counts)
		}
	
	# For RCI totals
	minsteps = sum(minsteps_list)
	
	# actual steps (Sankoff parsimony)
	phyd = dtf_to_phyDat(char_dtf)
	steps = parsimony(tree, phyd)

	# g for that column
	g = parsimony(star_tree, phyd)

	# calculate CI
	CI = round((minsteps / steps), 3)
	RI = round((g - steps) / (g - minsteps), 3)
	RCI = round(CI * RI, 3)
	
	parsim_ttls = adf(c(minsteps, g, steps, CI, RI, RCI))
	names(parsim_ttls) = c("minstep", "g", "nstep", "CI", "RI", "RCI")
	row.names(parsim_ttls) = "total"
	return(parsim_ttls)
	}



RCIcalc <- function(CIvals, RIvals)
	{
	# These problems for CI noted above may be overcome by excluding autapomorphies OR calculating a Rescaled 
	# Consistency Index.   
	# 
	# RC = RI*CI   
	# 
	# This removes the impact of any characters that do not
	# contribute to the ÒfitÓ of the data to the tree (e.g., 
	# autapomorphies ci=1.0 and ri=0.0) 
	# 
	
	RCIvals = CIvals * RIvals
	return(RCIvals)
	}

CI_phyd <- function(tree, phyd)
	{
	# Calculate total CI, given a phyDat dataset and a tree
	# To get individual CIs, use CI_dtf
	#
	# From IB200a notes:
	# ci = m/s  -----where m = minimum number of steps in a character (number of states -1) 
	#       s = steps actually realized on a given tree 

	# convert phyDat object to data.frame:
	x = as.character(phyd)
	minsteps_list = apply(x, 2, function(x) length(unique(x)))
	minsteps = sum(minsteps_list)
	
	# actual steps
	steps = parsimony(tree, phyd)

	# calculate CI
	CIval = minsteps/steps
	return(CIval)	
	}


parsim_stats <- function(tree, char_dtf)
	{
	# Calculate the standard parsimony statistics
	CIval = CI_dtf(tr2, char_dtf)
	CIvals = CI_dtf(tr2, char_dtf, by_char=TRUE)
	
	RIval = RI_dtf(tr2, char_dtf)
	RIvals = RI_dtf(tr2, char_dtf, by_char=TRUE)

	RCIval = RCIcalc(CIval, RIval)
	RCIvals = RCIcalc(CIvals, RIvals)

	steps_list = steps_dtf(tr2, char_dtf, by_char=TRUE)
	steps = sum(steps_list)
	
	minsteps_list = minsteps_dtf(tr2, char_dtf, by_char=TRUE)
	minsteps = sum(minsteps)

	g_list = g_dtf(tr2, char_dtf, by_char=TRUE)
	g_ttl = sum(g_list)

	
	# Assemble the output
	minstep = c(minsteps_list, minsteps)
	g = c(g_list, g_ttl)
	nstep = c(steps_list, steps)
	CI = round(c(CIvals, CIval), 3)
	RI = round(c(RIvals, RIval), 3)
	RCI = round(c(RCIvals, RCIval), 3)
	
	parstats = cbind(minstep, g, nstep, CI, RI, RCI)
	parstats = adf(parstats)
	row.names(parstats) = c(names(char_dtf), "total")
	
	return(parstats)
	
	}









# Get the log likelihood given data, phylogeny, and Q rate matrix
# (diagonals are negative so rows add to 0)
calc_loglike <- function(x, phy, Qmat)
	{
	nl = nrow(Qmat)
	
	nb.node = phy$Nnode
	nb.tip = length(phy$tip.label)
	
	# likelihoods computed at all of these nodes
	comp <- numeric(nb.tip + nb.node)
	
	liks <- matrix(0, nb.tip + nb.node, nl)
	TIPS <- 1:nb.tip
	
	# initial likelihoods of all the observed states = 1
	liks[cbind(TIPS, x)] <- 1
	
	# This is CRUCIAL!!
	phy <- reorder(phy, "pruningwise")

	for (i in seq(from = 1, by = 2, length.out = nb.node))
		{
		j <- i + 1
		anc <- phy$edge[i, 1]
		des1 <- phy$edge[i, 2]
		des2 <- phy$edge[j, 2]
		
		#print("Checking Q matrix")
		#cat("p=", p, ", rate=", rate, "\n", sep=" ")
		#print(Q)
		#print(phy$edge.length[i])
		#v.l <- matexpo(Qmat * phy$edge.length[i]) %*% liks[des1, 
		#  ]
		#v.r <- matexpo(Qmat * phy$edge.length[j]) %*% liks[des2, 
		#  ]
		v.l <- expm(Qmat * phy$edge.length[i], method="Ward77") %*% liks[des1, 
		  ]
		v.r <- expm(Qmat * phy$edge.length[j], method="Ward77") %*% liks[des2, 
		  ]
		v <- v.l * v.r
		comp[anc] <- sum(v)
		liks[anc, ] <- v/comp[anc]
		}
	#output_loglike = -2 * sum(log(comp[-TIPS]))
	output_loglike = 2 * sum(log(comp[-TIPS]))
	
	# matexpo sometimes produces log-likelihoods greater than 0 (???)
	if (output_loglike >= 0)
		{
		output_loglike = NaN
		}
	
	return(output_loglike)
	}


calc_loglike_lotsa_sims <- function(y, phy, Qmat)
	{
	nl = nrow(Qmat)
	
	nb.node = phy$Nnode
	nb.tip = length(phy$tip.label)
	
	# likelihoods computed at all of these nodes
	comp <- numeric(nb.tip + nb.node)
	
	liks <- matrix(0, nb.tip + nb.node, nl)
	TIPS <- 1:nb.tip
	
	# This is CRUCIAL!!
	phy <- reorder(phy, "pruningwise")
	
	numsims = dim(y)[3]
	output_loglike_list = array(NA, numsims)
	for (simn in 1:numsims)
		{
		x = y[, , simn]
		
		# initial likelihoods of all the observed states = 1
		liks[cbind(TIPS, x)] <- 1
		

		for (i in seq(from = 1, by = 2, length.out = nb.node))
			{
			j <- i + 1
			anc <- phy$edge[i, 1]
			des1 <- phy$edge[i, 2]
			des2 <- phy$edge[j, 2]
			
			#print("Checking Q matrix")
			#cat("p=", p, ", rate=", rate, "\n", sep=" ")
			#print(Q)
			#print(phy$edge.length[i])
			v.l <- matexpo(Qmat * phy$edge.length[i]) %*% liks[des1, 
			  ]
			v.r <- matexpo(Qmat * phy$edge.length[j]) %*% liks[des2, 
			  ]
			v <- v.l * v.r
			comp[anc] <- sum(v)
			liks[anc, ] <- v/comp[anc]
			}
		#output_loglike = -2 * sum(log(comp[-TIPS]))
		tmp_output_loglike = 2 * sum(log(comp[-TIPS]))
		output_loglike_list[j] = tmp_output_loglike
		}
	
	if (sum(is.na(output_loglike_list)) > 0)
		{
		print(paste("ERROR: ", sum(is.na(output_loglike_list)), " sims produced NAs for log like", sep=""))
		output_loglike = NaN
		}
	else
		{
		output_loglike = sum(output_loglike_list)
		}
	return(output_loglike)
	}






# Fill a rate matrix from CRP
make_rate_matrix = function(nrows, alpha=1, rel_rate_prior=c(0,10))
	{
	num_rates = nrows * nrows - nrows
	temprates = rchinese(num_rates, alpha)
	
	uniqs = sort(unique(temprates))
	num_uniq = length(uniqs)
	lambdas = rep(NA, num_uniq)
	lambdas = runif(num_uniq, rel_rate_prior[1], rel_rate_prior[2])
	
	outmat = matrix(NA, nrow=nrows, ncol=nrows)
	diag(outmat) = 0
	outmat[is.na(outmat)] = temprates
	
	for (i in 1:num_uniq)
		{
		outmat[outmat==uniqs[i]] = lambdas[i]
		}
	
	diag(outmat) = -rowSums(outmat)
	
	return(outmat)
	}



# print tree in hierarchical format
prt <- function(t, printflag=TRUE, relabel_nodes = FALSE)
	{
	# assemble beginning table
	
	# check if internal node labels exist
	if ("node.label" %in% attributes(t)$names == FALSE)
		{
		rootnum = get_nodenum_structural_root(t)
		
		new_node_labels = paste("inNode", rootnum:(rootnum+t$Nnode-1), sep="")
		t$node.label = new_node_labels
		}
	
	# or manually relabel the internal nodes, if desired
	if (relabel_nodes == TRUE)
		{
		rootnum = get_nodenum_structural_root(t)
		
		new_node_labels = paste("inNode", rootnum:(rootnum+t$Nnode-1), sep="")
		t$node.label = new_node_labels
		}
	
	labels = c(t$tip.label, t$node.label)
	ordered_nodenames = get_nodenums(t)
	#nodenums = 1:length(labels)
	node.types1 = rep("tip", length(t$tip.label))
	node.types2 = rep("internal", length(t$node.label))
	node.types2[1] = "root"
	node.types = c(node.types1, node.types2)
	
	# These are the index numbers of the edges below each node
	parent_branches = get_indices_where_list1_occurs_in_list2(ordered_nodenames, t$edge[,2])
	#parent_edges = parent_branches
	brlen_to_parent = t$edge.length[parent_branches]
	
	parent_nodes = t$edge[,1][parent_branches]
	daughter_nodes = lapply(ordered_nodenames, get_daughters, t)
	
	# print out the structural root, if desired
	root_nodenum = get_nodenum_structural_root(t)
	tmpstr = paste("prt(t): root=", root_nodenum, "\n", sep="")
	prflag(tmpstr, printflag=printflag)
	
	levels_for_nodes = unlist(lapply(ordered_nodenames, get_level, t))
	#tmplevel = get_level(23, t)
	#print(tmplevel)
	
	
	#height above root
	hts_at_end_of_branches_aka_at_nodes = t$edge.length
	hts_at_end_of_branches_aka_at_nodes = get_all_node_ages(t)
	h = hts_at_end_of_branches_aka_at_nodes

	# times before present, below (ultrametric!) tips
	# numbers are positive, i.e. in millions of years before present
	#                       i.e. mybp, Ma
	times_before_present = get_max_height_tree(t) - h

	
	# fill in the ages of each node for the edges
	edge_ages = t$edge
	edge_ages[,1] = h[t$edge[,1]]	# bottom of branch
	edge_ages[,2] = h[t$edge[,2]]	# top of branch


	# fill in the times before present of each node for the edges
	edge_times_bp = t$edge
	edge_times_bp[,1] = times_before_present[t$edge[,1]]	# bottom of branch
	edge_times_bp[,2] = times_before_present[t$edge[,2]]	# top of branch
	
	
	
	tmpdtf = cbind(1:length(ordered_nodenames), ordered_nodenames, levels_for_nodes, node.types, parent_branches, brlen_to_parent, parent_nodes, daughter_nodes, h, times_before_present, labels)
	
	dtf = as.data.frame(tmpdtf, row.names=NULL)
	# nd = node
	
	# edge.length is the same as brlen_2_parent
	names(dtf) = c("node", "ord_ndname", "node_lvl", "node.type", "parent_br", "edge.length", "ancestor", "daughter_nds", "node_ht", "time_bp", "label")
	
	# convert the cols from class "list" to some natural class
	dtf = unlist_dtf_cols(dtf, printflag=FALSE)
	
	# print if desired
	prflag(dtf, printflag=printflag)
	
	#tree_strings = c()
	#root_str = get_node_info(root_nodenum, t)
	return(dtf)
	}



get_all_daughter_tips_of_a_node <- function(nodenum, t)
	{
	subtree = extract.clade(t, nodenum)
	temp_tips = subtree$tip.label
	return(temp_tips)
	}


trace_parents_up <- function(nodenum, t, depthtime)
	{
	# Trace from a node up to its parents etc., a specified distance
	parent_node = get_parent_for_trace_parents_up(nodenum, t)
	
	length_to_parent = t$edge.length[t$edge[,2] == nodenum]
	if (length_to_parent == depthtime)
		{
		print("ERROR: trace_parents_up() doesn't want to find an EXACT match between depthtime and a node; this will lead to problems in hook addition!")
		}
	if (length_to_parent > depthtime)
		{
		# you're done!
		return(nodenum)
		}
	else
		{
		# burrow up to parents
		depthtime = depthtime - length_to_parent
		parent_node = trace_parents_up(parent_node, t, depthtime)
		return(parent_node)
		}
	return(nodenum)
	}


get_parent_for_trace_parents_up <- function(nodenum, t)
	{
	matching_edges = findall(nodenum, t$edge[,2])
	parent_nodenum = t$edge[,1][matching_edges][1]
	
	print(paste("nodenum=", nodenum, " parent_nodenum=", parent_nodenum, sep=""))
	if (is.na(parent_nodenum))
		{
		print(paste("get_parent(): node ", nodenum, " has no parent, it's probably the root!\nAnd you missed whatever parent you were actually trying to find!", sep=""))
		}
	return(parent_nodenum)
	}
	

dist_between_direct_ancestors <- function(ancestor_node, descendant_node, t, totaldist=0)
	{
	# Recursive algorithm to get distance between descendent and ancestor
	
	if (ancestor_node == descendant_node)
		{
		print("dist_between_direct_ancestors(): ancestor_node == descendant_node")
		return(totaldist)
		}
	
	parent_node = get_parent(descendant_node, t)
	dist_to_parent = t$edge.length[t$edge[,2] == descendant_node]
	
	totaldist = dist_to_parent + totaldist
	
	#print(paste(parent_node, ancestor_node, sep=""))
	if (parent_node == ancestor_node)
		{
		return(totaldist)
		}
	else
		{
		totaldist = dist_between_direct_ancestors(ancestor_node, parent_node, t, totaldist)
		return(totaldist)
		}
	}

get_daughters <- function(nodenum, t)
	{
	daughter_edgenums = findall(nodenum, t$edge[,1])
	daughter_nodenums = t$edge[,2][daughter_edgenums]
	return(daughter_nodenums)
	}

get_parent <- function(nodenum, t)
	{
	matching_edges = findall(nodenum, t$edge[,2])
	parent_nodenum = t$edge[,1][matching_edges][1]
	return(parent_nodenum)
	}
	
get_level <- function(nodenum, t, tmplevel=0)
	{
	parent_nodenum = get_parent(nodenum, t)
	if (is.na(parent_nodenum))
		{
		#tmplevel = 0
		return(tmplevel)
		}
	else
		{
		#print(paste("parent_nodenum: ", parent_nodenum, " level: ", tmplevel, sep=""))
		tmplevel = tmplevel + 1
		tmplevel = get_level(parent_nodenum, t, tmplevel)
		return(tmplevel)
		}
	# If an error occurs
	return(NA)
	}

get_node_info <- function(nodenum, t, printthis=FALSE)
	{
	# find all of the edges descending from this node
	edgenums = findall(nodenum, t$edge[,1])
	# find the daughters of these nodes
	daughters = t$edge[edgenums,2]
	# find the length of the branches attaching to these nodes
	brlens = t$edge.length[edgenums]
	
	# find all of the edges mother to this node (hopefully, just one!)
	mother_edgenums = findall(nodenum, t$edge[,2])
	if (length(mother_edgenums) > 1)
		{
		print(paste("ERROR: get_node_info found more than one mother for node ", nodenum, sep=""))
		}
	if (length(mother_edgenums) == 0)
		{
		print(paste("get_node_info found root at node ", nodenum, sep=""))
		mother = ""
		}
	mother = t$edge[mother_edgenums, 1]
	mother_brlens = t$edge.length[mother_edgenums]
	
	if (printthis == TRUE)
		{
		print(paste("edgenums: ", edgenums, sep=""))
		print(paste("daughters: ", daughters, sep=""))
		print(paste("brlens: ", brlens, sep=""))
		print(paste("mother_edgenums: ", mother_edgenums, sep=""))
		print(paste("mother: ", mother, sep=""))
		print(paste("mother_brlens: ", mother_brlens, sep=""))
		}
	
	# Store data in a structure
	nodeinfo = c()
	nodeinfo$numdaughters = length(daughters)
	nodeinfo$edgenums = edgenums
	nodeinfo$daughters = daughters
	nodeinfo$brlens = brlens
	nodeinfo$mother_edgenums = mother_edgenums
	nodeinfo$mother = mother
	nodeinfo$mother_brlens = mother_brlens
	return(nodeinfo)	
	}



get_edge_times_before_present <- function(t)
	{
	#height above root
	hts_at_end_of_branches_aka_at_nodes = t$edge.length
	hts_at_end_of_branches_aka_at_nodes = get_all_node_ages(t)
	h = hts_at_end_of_branches_aka_at_nodes

	# times before present, below (ultrametric!) tips
	# numbers are positive, i.e. in millions of years before present
	#                       i.e. mybp, Ma
	times_before_present = get_max_height_tree(t) - h

	
	# fill in the ages of each node for the edges
	edge_ages = t$edge
	edge_ages[,1] = h[t$edge[,1]]	# bottom of branch
	edge_ages[,2] = h[t$edge[,2]]	# top of branch

	# fill in the times before present of each node for the edges
	edge_times_bp = t$edge
	edge_times_bp[,1] = times_before_present[t$edge[,1]]	# bottom of branch
	edge_times_bp[,2] = times_before_present[t$edge[,2]]	# top of branch
	
	return(edge_times_bp)
	}



get_edge_ages_above_root <- function(t)
	{
	#height above root
	hts_at_end_of_branches_aka_at_nodes = t$edge.length
	hts_at_end_of_branches_aka_at_nodes = get_all_node_ages(t)
	h = hts_at_end_of_branches_aka_at_nodes

	# fill in the ages of each node for the edges
	edge_ages = t$edge
	edge_ages[,1] = h[t$edge[,1]]	# bottom of branch
	edge_ages[,2] = h[t$edge[,2]]	# top of branch
	
	return(edge_ages)
	}



# Trace the height of the tree from the startnode to the tip
find_sisters <- function(t, nodenum)
	{
	# Find the mother node
	mother_edgenums = findall(nodenum, t$edge[,2])
	
	
	}

# this returns the NUMBERS identifying each node
get_nodenums <- function(t)
	{
	# get just the unique node numbers from the edge list (left column: start node; right column: end node):
	nodenames = unique(c(t$edge))
	ordered_nodenames = nodenames[order(nodenames)]
	return(ordered_nodenames)
	}

get_nodenum_structural_root <- function(t, print_nodenum=FALSE)
	{
	#numnodes = length(t$tip.label) + length(t$node.label)
	#ordered_nodes = 1:length(numnodes)
	
	ordered_nodes = get_nodenums(t)

	root_nodenums_list = c()
	for (n in 1:length(ordered_nodes))
		{
		tmpnode = ordered_nodes[n]
		if (tmpnode %in% t$edge[,2])
			{
			blah = TRUE
			}
		else
			{
			if (print_nodenum == TRUE)
				{
				cat("get_nodenum_structural_root(): Root nodenum = ", tmpnode, sep="")
				}
			root_nodenums_list = c(root_nodenums_list, tmpnode)
			}
		}
	return(root_nodenums_list)
	}

# This assumes that the root node is the first node in the numbered list 1:Nnode,
#  *after* the tip nodes, which are labeled 1:ntips
get_rootnodenum_implied <- function(t)
	{
	rootnodenum = length(t$tip.label) + 1
	return(rootnodenum)
	}

get_root_name <- function(obj)
	{
	bts = branching.times(obj)
	rootname = names(bts[1])
	cat("get_root_name(obj: Root name = ", rootname, "\n")
	return(rootname)
	}

get_root_as_num <- function(obj)
	{
	rootname = get_nodenum_structural_root(obj)
	root_node_num = as.numeric(rootname)
	return(root_node_num)
	}

get_TF_tips <- function(obj)
	{
	# Get TF for nodes being tips
	
	# BIG CHANGE?
	#TF_tips = match_list1_in_list2(1:length(dists_from_root), obj$tip.label)
	TF_tips = match_list1_in_list2(1:length(obj$edge), 1:length(obj$tip.label))
	#TF_tips = obj$tip.label[TF_tips_indices]
	return(TF_tips)
	}

get_node_ages_of_tips <- function(obj)
	{
	TF_tips = get_TF_tips(obj)
	root_node_num = get_nodenum_structural_root(obj)
	dists_from_root = dist.nodes(obj)[root_node_num, ]
	node_ages_of_tips = dists_from_root[TF_tips]
	return(node_ages_of_tips)
	}

get_all_node_ages <- function(obj)
	{
	node_ages = dist.nodes(obj)[get_nodenum_structural_root(obj), ]
	return(node_ages)
	}

get_max_height_tree <- function(obj)
	{
	max_height = max(get_node_ages_of_tips(obj))
	return(max_height)
	}

get_node_names_of_tips <- function(obj)
	{
	TF_tips = get_TF_tips(obj)
	root_node_num = get_root_as_num(obj)
	dists_from_root = dist.nodes(obj)[root_node_num, ]
	node_names_of_tips = names(dists_from_root[TF_tips])
	return(node_names_of_tips)
	}

get_TF_tips_alive <- function(obj)
	{
	node_ages_of_tips = get_node_ages_of_tips(obj)
	TF_tips_alive = (round(node_ages_of_tips, digits=5) == round(max(node_ages_of_tips), digits=5))
	return(TF_tips_alive)
	}


get_indices_of_tip_nodes <- function(obj)
	{
	tip_indices = 1:length(obj$tip.label)
	return(tip_indices)
	}
	
get_indices_of_branches_under_tips <- function(obj)
	{
	tip_indices = get_indices_of_tip_nodes(obj)
	branchnums_under_tips = get_indices_where_list1_occurs_in_list2_noNA(tip_indices, obj$edge[, 2])
	return(branchnums_under_tips)
	}


midpoint <- function(tree)
	{
	# Root a tree at the midpoint
	# Source: https://stat.ethz.ch/pipermail/r-sig-phylo/2010-September/000750.html
	require(phangorn)
	
	dm = cophenetic(tree)
	tree = unroot(tree)
	rn = max(tree$edge)+1
	maxdm = max(dm)
	ind =  which(dm==maxdm,arr=TRUE)[1,]
	
	# "Ancestors" function requires phangorn
	#e <- simpleError("Error: 'Ancestors' function inside of 'midpoint' requires: 'library(phangorn)' in your script.")
	#tryCatch(Ancestors(tree, ind[1], "parent"), error = function(e) e, finally=print("tryCatch error message"))
	
	# If it works:
	library(phangorn)
	tmproot = Ancestors(tree, ind[1], "parent")
	
	tree = phangorn:::reroot(tree, tmproot)
	edge = tree$edge
	el = tree$edge.length
	children = tree$edge[,2]
	left = match(ind[1], children)
	tmp = Ancestors(tree, ind[2], "all")
	tmp= c(ind[2], tmp[-length(tmp)])
	right = match(tmp, children)
	if(el[left]>= (maxdm/2))
		{
		edge = rbind(edge, c(rn, ind[1]))
		edge[left,2] = rn
		el[left] = el[left] - (maxdm/2)
		el = c(el, maxdm/2)
		}
	else
		{
	    sel = cumsum(el[right])
	    i = which(sel>(maxdm/2))[1]
	    edge = rbind(edge, c(rn, tmp[i]))
	    edge[right[i],2] = rn
	    eltmp =  sel[i] - (maxdm/2)
		#el = c(el, sel[i] - (maxdm/2))
	    el = c(el, el[right[i]] - eltmp)
	    el[right[i]] = eltmp
		}
	tree$edge.length = el
	tree$edge=edge
	tree$Nnode  = tree$Nnode+1
	phangorn:::reorderPruning(phangorn:::reroot(tree, rn))
	}




min_phyd <- function(phyd)
	{
	###############################################################################
	# min_phyd: find the minimums of a phylogenetic distance matrix, for each tip
	###############################################################################
	# replace diagonal 0s with NAs
	# also replace the upper Triangle
	tmpphyd = phyd
	diag(tmpphyd) = NA
	upperTriangle(tmpphyd) = NA
	
	maxrow = nrow(phyd)
	
	# leave out the last column of tmpphyd, which is just NA
	mindists = colmin(tmpphyd[, 1:(ncol(tmpphyd)-1) ])
	
	list_of_min_pairs = c()
	for (i in 1:ncol(tmpphyd))
		{
		# avoid repeating pairs...with phyd[(i+1):maxrow, i]
		tmp_closest_tips = which(tmpphyd[, i] == mindists[i])
		
		tmplist = cbind(rep(i, length(tmp_closest_tips)), tmp_closest_tips)
		
		list_of_min_pairs = rbind(list_of_min_pairs, tmplist)
		}
	return(list_of_min_pairs)
	}


extend_tips_to_ultrametricize <- function(obj, age_of_root, tips_end_at_this_date=NA)
	{
	
	#t3 = 
	
	print("node ages of tips:")
	tip_ages = age_of_root + get_node_ages_of_tips(obj)
	#print(tip_ages)
	
	
	if (is.na(tips_end_at_this_date))
		{
		tips_end_at_this_date = max(tip_ages)
		}
	
	nums_to_add_to_tip_to_ultrametricize = tips_end_at_this_date - tip_ages
	
	indices_of_branches_under_tips = get_indices_of_branches_under_tips(obj)

	obj$edge.length[indices_of_branches_under_tips] = obj$edge.length[indices_of_branches_under_tips] + nums_to_add_to_tip_to_ultrametricize
	
	return(obj)
	}



edges_existing_at_correct_time_bp_TF <- function(time_slice, edge_times_bp)
	{
	# find the edges that exist in the right time
	edges_that_start_below_time = edge_times_bp[, 1] > time_slice
	edges_that_end_after_time = edge_times_bp[, 2] <= time_slice
	edges_that_exist_in_the_right_time = edges_that_start_below_time + edges_that_end_after_time == 2
	return(edges_that_exist_in_the_right_time)
	}



################################################
# Tree drawing
################################################
drawtree_branches_heatmap <- function(tr, nums_for_edges, titletxt="", rescaled_colors="", tmp_br_wid=8)
	{
	# heat map
	# scale to 0-1, times 130, round down, +1, = scale from 20 to 120
	tmp_colors = rev(heat.colors(130, alpha=1))
	
	if (rescaled_colors == "")
		{
		rescaled_colors = 15+floor( 99*((nums_for_edges-min(nums_for_edges)) / (max(nums_for_edges)-min(nums_for_edges))) )
		}
	br.col = tmp_colors[rescaled_colors]
	plot(tr, edge.color=br.col, edge.width=tmp_br_wid, label.offset=0.05)
	
	# Legend
	value_range = pretty(nums_for_edges)
	value_range[1] = round(min(nums_for_edges), digits=2)
	value_range[length(value_range)] = round(max(nums_for_edges), digits=2)

	value_range_rescaled = 15+floor( 99*((value_range-min(nums_for_edges)) / (max(nums_for_edges)-min(nums_for_edges))) )
	
	color_range = tmp_colors[value_range_rescaled]
	legend(x="bottomleft", legend=value_range, bty="0", fill=color_range, y.intersp=0.75, pt.cex=2, cex=1, xjust=0, yjust=0, title=titletxt)

	}
	
drawtree_branches_bluered <- function(tr, nums_for_edges, titletxt="", rescaled_colors="", tmp_br_wid=8)
	{
	# blue-red
	# scale to 0-1, times 100, round down, +1, = scale from 1 to 100
	tmp_colors = rev(rainbow(110, start=0, end=4.5/6, alpha=1))
	if (rescaled_colors == "")
		{
		rescaled_colors = 5+floor( 99*((nums_for_edges-min(nums_for_edges)) / (max(nums_for_edges)-min(nums_for_edges))) )
		}
	br.col = tmp_colors[rescaled_colors]
	plot(tr, edge.color=br.col, edge.width=tmp_br_wid, label.offset=0.05)

	# Legend
	value_range = pretty(nums_for_edges)
	value_range[1] = round(min(nums_for_edges), digits=2)
	value_range[length(value_range)] = round(max(nums_for_edges), digits=2)

	value_range_rescaled = 5+floor( 99*((value_range-min(nums_for_edges)) / (max(nums_for_edges)-min(nums_for_edges))) )
	
	color_range = tmp_colors[value_range_rescaled]
	legend(x="bottomleft", legend=value_range, bty="o", fill=color_range, y.intersp=0.75, pt.cex=2, cex=1, xjust=0, yjust=0, title=titletxt)

	}








################################################################################
# TREE MODIFICATION FUNCTIONS (e.g. adding hooks, choosing certain branches)
################################################################################
add_hooks <- function(t, list_of_times_before_present, brlen_of_side_branch = 0.001, plottree=FALSE)
	{
	# Take a list of ages, add hooks to any branch existing at that age

	# OK, RE-FUCKING DO
	# Gather a list of tip labels, and then record the distance below those tips that
	# you would go down to attach a hook
	hooktree = t
	list_of_daughter_tipnames_to_add_hooks_below = c()
	list_of_ages_below_daughter = c()   # assumes ultrametric
	ntips = length(hooktree$tip.label)
	for (i in 1:length(list_of_times_before_present))
		{
		# Get the edges that exist at the time_slice in question
		time_slice = list_of_times_before_present[i]
		edge_times_bp = get_edge_times_before_present(hooktree)
		edges_that_exist_in_the_right_time = edges_existing_at_correct_time_bp_TF(time_slice, edge_times_bp)
		
		# get the nodes daughter to the branches that match
		nodenums_to_add_hooks_to = hooktree$edge[,2][edges_that_exist_in_the_right_time]

		# calculate the times parent to these daughters at which to insert the hooks		
		times_before_daughter_nodes = time_slice - edge_times_bp[edges_that_exist_in_the_right_time, 2]
		
		# trace these nodes to their tips in the (UNALTERED ORIGINAL) tree
		for (j in 1:length(nodenums_to_add_hooks_to))
			{
			nodenum = nodenums_to_add_hooks_to[j]
			if (nodenum <= ntips)
				{
				list_of_daughter_tipnames_to_add_hooks_below = c(list_of_daughter_tipnames_to_add_hooks_below, hooktree$tip.label[nodenum])
				list_of_ages_below_daughter = c(list_of_ages_below_daughter, time_slice)			
				}
			else
				{
				temp_tips = get_all_daughter_tips_of_a_node(nodenum, hooktree)
				list_of_daughter_tipnames_to_add_hooks_below = c(list_of_daughter_tipnames_to_add_hooks_below, temp_tips[1])
				list_of_ages_below_daughter = c(list_of_ages_below_daughter, time_slice)
				}
			}
		}
		
	
	# Now, attach the hooks
	for (i in 1:length(list_of_daughter_tipnames_to_add_hooks_below))
		{
		print(paste("i=", i, sep=""))
		tipname = list_of_daughter_tipnames_to_add_hooks_below[i]
		depthtime = list_of_ages_below_daughter[i]
		
		hooktree = add_hook(hooktree, tipname, depthtime, plottree=plottree)
		}
	
	return(hooktree)
	}


add_hook <- function(t, tipname, depthtime, brlen_of_side_branch=0.001, plottree = FALSE)
	{
	# Add a hook (a small side tip) to a phylogeny
	#
	# e.g.:
	# Do spatial variogram by doing points from many different species
	# add tips to tree
	#cat("owls(((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3):6.3,Tyto_alba:13.5);", file = "ex.tre", sep = "\n")
	#t <- read.tree("ex.tre")
	#prt(t)
	#newtree = add_hook(t, brlen_of_side_branch = 0.001, plottree = TRUE)

	cat("add_hook(): Adding below tipname ",  tipname, "\n", sep="")

	newtree = t
	#daughter_nodenum_to_add_hook_below = 4
	#height_below_daughter_at_which_to_add_it = 1.0
	#brlen_of_side_branch = 0.001
	
	# find the node, below this you will add the 
	tip_nodenum = which(t$tip.label == tipname)
	print(paste("addhook(): tip_nodenum = ", tip_nodenum, sep=""))	

	daughter_nodenum_to_add_hook_below = trace_parents_up(tip_nodenum, t, depthtime)
	print(paste("addhook(): internal nodenum to add below = ", daughter_nodenum_to_add_hook_below, sep=""))	
	
	tip_to_ancestor_node_dist = dist_between_direct_ancestors(daughter_nodenum_to_add_hook_below, tip_nodenum, t, totaldist=0)
	
	
	
	height_below_daughter_at_which_to_add_it = depthtime - tip_to_ancestor_node_dist
	
	
	# add a new tip to the list of tips (this is the hook)
	new_tip_nodenum = get_nodenum_structural_root(t)
	
	# bump up all of the nodenums above the node tip by 1
	newtree$edge[t$edge >= new_tip_nodenum] = t$edge[t$edge >= new_tip_nodenum] + 1
	
	# add a new internal node at the end
	new_inNode = max(newtree$edge) + 1
	
	# add two new edges, and replace the old edge
	#print(t$edge[,2])
	#print(daughter_nodenum_to_add_hook_below)
	#print(t$edge[,2] == daughter_nodenum_to_add_hook_below)
	old_edge_num = which(t$edge[,2] == daughter_nodenum_to_add_hook_below)
	
	# extract the edgenums before and after this insertion (exceptions for in case
	# if the modified row is the first or last row)
	if (old_edge_num == 1)
		{
		first_old_edges_rownums = NULL
		} else {
		first_old_edges_rownums = 1:(old_edge_num-1) #newtree$edge[1:(old_edge_num-1), ]
		}
	if (old_edge_num == nrow(t$edge))
		{
		second_old_edges_rownums = NULL
		} else {
		second_old_edges_rownums = (old_edge_num+1):nrow(t$edge) # newtree$edge[, ]
		}
	
	
	
	# replace the edge, keeping the old parent (which may have increased by 1! use newtree!!), put the new internal node as the daughter)
	replacement_edge_row = newtree$edge[old_edge_num, ]
	replacement_edge_row[2] = new_inNode
	
	# subtract the distance below the daughter, from the top
	replacement_edge_length = t$edge.length[old_edge_num] - height_below_daughter_at_which_to_add_it
	
	
	# make the new edge, which goes below the old daughter node
	# you have to bump the daughter_nodenum_to_add_hook_below if it is
	# >= to the new_tip_nodenum
	if (daughter_nodenum_to_add_hook_below >= new_tip_nodenum)
		{
		daughter_nodenum_to_add_hook_below = daughter_nodenum_to_add_hook_below + 1
		}
	new_edge_below_old_daughter_node = c(new_inNode, daughter_nodenum_to_add_hook_below)
	new_edge_below_old_daughter_node_edge_length = height_below_daughter_at_which_to_add_it
	
	# make the new edge, which goes below the new tip: c(parent, daughter)
	new_edge_below_new_tip = c(new_inNode, new_tip_nodenum)
	new_edge_below_new_tip_edge_length = brlen_of_side_branch
	
	
	# add the edge rows before the one that is replaced, then the replaced edge, then the other old edges, then the 2 new edges
	new_edge_table = rbind(newtree$edge[first_old_edges_rownums, ], replacement_edge_row, newtree$edge[second_old_edges_rownums, ], new_edge_below_old_daughter_node, new_edge_below_new_tip)
	
	new_edgelength_list = c(t$edge.length[first_old_edges_rownums], replacement_edge_length, t$edge.length[second_old_edges_rownums], new_edge_below_old_daughter_node_edge_length, new_edge_below_new_tip_edge_length)
	
	# it MAY be important that the node numbers be INTEGER, not NUMERIC
	newtree$edge = matrix(as.integer(new_edge_table), ncol=2, byrow=FALSE)
	
	#row.names(newtree$edge) = NULL
	#newtree$edge[,1] = as.integer(newtree$edge[,1])
	#newtree$edge[,2] = as.integer(newtree$edge[,2])
	
	newtree$edge.length = new_edgelength_list
	#row.names(newtree$edge.length) = NULL
	
	# update number of internal nodes
	newtree$Nnode = t$Nnode + 1
	
	# add the new tip to the end of the list of tips
	newtree$tip.label = c(t$tip.label, paste("hook", new_tip_nodenum, sep=""))
	
	cat("Adding ",  paste("hook", new_tip_nodenum, sep=""), "\n", sep="")
	
	
	# some crap to fix the tree formatting somehow
	# I mean, really, the tree was fucking logically correct, but
	# hanging plot and dist.nodes and probably anything
	# using reorder(phy, "pruningwise"), but I couldn't figure out why
	# I guess the order of the tips is important for some reason?
	# like maybe leftmost tip is leftmost in branching?
	# wtf kind of data architecture is this?
	# anyway, FUCK IT, writing to Newick and reading back fixes it.
	newtree = reorder(newtree)
	tmpfn = "/_njm/tmp_junktree.tree"
	write.tree(newtree, tmpfn)
	newtree2 = read.tree(tmpfn)

	
	# plot, if desired:
	if (plottree == TRUE)
		{
		cat("add_hook(): plotting/printing the resulting tree...\n", sep="")
		prt(newtree)
		plot(newtree)
		}
	
	return(newtree2)
	}








# For input into diversitree, you might want to put real diversities/population numbers
# into each tip of a skeleton tree.  This does that, in the correct format.
add_species_to_skeleton_tree <- function(t1, diversity_of_each_tip)
	{
	t1$names = c(t1$names, "clades")
	ntaxa = length(t1$tip.label)
	
	

	# blank clades list:
	#t1$clades = c()
	
	clades = c()

	#max_numsp = max(diversity_of_each_tip)
	max_numsp = sum(diversity_of_each_tip)
	#print(max_numsp)
	numdigits = nchar(as.character(ceiling(max_numsp)))
	#sprintf_format_string = "%04.0f"
	sprintf_format_string = paste("%0", numdigits, ".0f", sep="")
	
	old_numsp = double(1)
	for (i in 1:ntaxa)
		{
		tipname = t1$tip.label[i]
		
		numsp = diversity_of_each_tip[i]
		if (numsp < 2)
			{
			numsp = 1
			old_numsp = old_numsp + 1
			new_numsp = old_numsp + numsp
			}
		else
			{
			old_numsp = old_numsp + 1
			new_numsp = old_numsp + numsp			
			}
		#print(old_numsp)
		
		new_numsp = old_numsp + numsp
		
		spnums = sprintf(sprintf_format_string, old_numsp:new_numsp)
		tipname_splist = paste("sp", spnums, sep="")
		old_numsp = new_numsp

		#
		cmdstr = paste("clades$", tipname, " = tipname_splist", sep="")
		eval(parse(text = cmdstr))
		}
	# I guess we need this (?)
	#t1@order = "cladewise"
	#t1$order = "cladewise"
	attr(t1, "order") = "cladewise"
	
	t1$clades = clades
	
	class(t1) <- c("clade.tree", "phylo")
	
	# return to main
	return(t1)		
	}

















###########################################
# PHYLO-BETA DIVERSITY STUFF
###########################################

get_nonphylo_data <- function(obj, input_states, diversity_of_each_tip)
	{
	outlist = c()
	tmpnames = names(obj$clades)
	ntaxa = length(obj$tip.label)
	
	clades = c()

	old_numsp = 0
	for (i in 1:ntaxa)
		{
		tipname = t1$tip.label[i]
		
		numsp = diversity_of_each_tip[i]
		if (numsp < 2)
			{
			numsp = 1
			old_numsp = old_numsp + 1
			new_numsp = old_numsp + numsp			
			}
		else
			{
			old_numsp = old_numsp + 1
			new_numsp = old_numsp + numsp			
			}
		# To make a repetitive list of the grouping taxa (e.g. genera)
		# taxa_to_add = rep(tmpnames[i], length(old_numsp : new_numsp)
		# outlist = c(outlist, taxa_to_add)

		# To make a repetitive list of the character states in the grouping taxa (e.g. genera)
		states_to_add = rep(input_states[i], length(old_numsp : new_numsp))
		outlist = c(outlist, states_to_add)
		}
	
	return(outlist)
	}



# Read in the consensus tree from MrBayes .con file
get_treestring_from_mb_confile <- function(mb_con_fn, findthis="tree con_50_majrule = ", treenum_to_return=1)
	{
	# Read in the text file:
	lines = scan(mb_con_fn, what="character", sep="\n")

	# Initialize the treecount
	treecount = 0
	
	# Go through the lines
	for (i in 1:length(lines))
		{
		# Trim whitespace (trim comes from the library(gregmisc))
		line = trim(lines[i])
		#cat(line, "\n", sep="")
		
		# Take the first line which starts with "tree con_50_majrule = "
		#findthis = "tree con_50_majrule = "
		result = find_instring(findthis, string=line)

		if (result == TRUE)
			{
			treecount = treecount + 1
			
			if (treecount == treenum_to_return)
				{
				return_string = strsplit(line, findthis)[[1]][2]
				cat("\n")
				cat("get_treestring_from_mb_confile() found tree:", "\n", sep="")
				cat(return_string, "\n")
				return(return_string)
				}
			else
				{
				next()
				}
			}
		}
	cat("get_treestring_from_mb_confile(): ERROR: No tree found!!\n")
	return
	}

# Read in consensus tree from MrBayes to APE tree structure (phylo structure)
get_contree_from_mb_confile <- function(mb_con_fn, findthis="tree con_50_majrule = ", treenum_to_return=1)
	{
	treestr = get_treestring_from_mb_confile(mb_con_fn)
	
	# Read in the tree
	temptree = read.tree(text=treestr)
	
	return(temptree)
	}


# Plot NMMDS (Non-metric, multidimensional scaling, from NMDS package) plots of tree distances
plot_NMMDS <- function(nmds_results_conf)
	{
	ticklength = 0
	plot(nmds_results_conf, col="grey", xlab="", ylab="", tcl=ticklength, xaxt="n", yaxt="n")
	points(nmds_results_conf[1,1], nmds_results_conf[1,2], pch="*", cex=3, col="black")
	return()
	}

# plot all conf in a NMDS_results_list
plot_NMMDS_list <- function(NMDS_results_list, points_to_color)
	{
	for (i in 1:length(NMDS_results_list))
		{
		nmds_results = NMDS_results_list[[i]]
		
		# Go through each of the confs
		for (j in 1:length(nmds_results$conf))
			{
			plot_NMMDS(nmds_results$conf[j][[1]])
			plot_NMMDS_points(nmds_results$conf[j][[1]], points_to_color)
			plot_NMMDS_species_tree(nmds_results$conf[j][[1]])
			}
		}
	return()
	}


plot_NMMDS_species_tree <- function(nmds_results_conf)
	{
	points(nmds_results_conf[1,1], nmds_results_conf[1,2], pch="*", cex=3, col="black")
	return()
	}

plot_NMMDS_points <- function(nmds_results_conf, points_to_color)
	{
	rows_to_keep = points_to_color$index
	# plot each point individually
	for (i in 1:length(rows_to_keep))
		{
		rows_to_plot = gather_rows_with_indices(nmds_results_conf, rows_to_keep[i])
		points(rows_to_plot[1], rows_to_plot[2], col=points_to_color$color[i], pch=points_to_color$cog[i])
		#cat(points_to_color$color[i], points_to_color$cog[i], sep="	")
		}
	return()
	}


# do an NMMDS plot of a distance matrix, colored (heat map) to match the rank order of a list
# of numbers that is the length of nrow(distance matrix)
plot_NMMDS_list_heatcolors <- function(NMDS_results_list, value_to_colorize, title1, title2)
	{
			
	# set up a blank 3-column matrix to hold the color data
	points_to_color = data.frame(matrix(data=NA, nrow=length(value_to_colorize), ncol=3))
	names(points_to_color) = c("index", "cog", "color")
	points_to_color$index = seq(1, nrow(points_to_color), 1)
	points_to_color$cog = as.numeric(1)
	
	# set up the colors list (you could replace heat.colors for some other colors)
	colslist = heat.colors(nrow(points_to_color))
	
	# set the values to use to rank the color map
	# value_to_colorize = align_stats$avgdiffsbetw / align_stats$avgdiffswithin
	value_to_colorize = value_to_colorize
	
	# add the values to color to a sequence of indices, 1:nrow(points_to_color)
	tmptable = data.frame(cbind(value_to_colorize, seq(1, nrow(points_to_color), 1)))
	names(tmptable) = c("value_to_colorize", "index")
	
	# order by the value
	tmptable = tmptable[order(tmptable$value_to_colorize), ]

	# now that they are ordered in ascending order, 
	colortable = cbind(tmptable, colslist)
	names(colortable) = c(names(tmptable), "color")
	colortable = colortable[order(colortable$index), ]
	points_to_color$color = colortable$color
	

	# Subplots	
	par(mfrow=c(4,3))
	# Subplot margins: c(bottom, left, top, right) 
	par(mar=c(0, 0, 0, 0))
	# outer margins
	par(oma=c(4, 4, 4, 2))	

	# mtext params:
	# outer = TRUE means outer margin
	# adj = parallel outer margin adjust, 0-1
	# padj = perpendic outer marging adjust, 0-1 within outer margin

	plot_NMMDS_list(NMDS_results_list[1:4], points_to_color)
	title(title2, outer=TRUE, line=1, cex.main=1.5)
	mtext("run1", outer=TRUE, side=1, adj=0.15, padj=1)
	mtext("run2", outer=TRUE, side=1, adj=0.5, padj=1)
	mtext("run3", outer=TRUE, side=1, adj=0.85, padj=1)
	mtext("RF", outer=TRUE, side=2, adj=0.88, padj=-0.5, las=0)
	mtext("Sym", outer=TRUE, side=2, adj=0.62, padj=-0.5, las=0)
	mtext("FPN", outer=TRUE, side=2, adj=0.36, padj=-0.5, las=0)
	mtext("Euc", outer=TRUE, side=2, adj=0.11, padj=-0.5, las=0)

	plot_NMMDS_list(NMDS_results_list[5:8], points_to_color)
	title(title2, outer=TRUE, line=1, cex.main=1.5)
	mtext("run1", outer=TRUE, side=1, adj=0.15, padj=1)
	mtext("run2", outer=TRUE, side=1, adj=0.5, padj=1)
	mtext("run3", outer=TRUE, side=1, adj=0.85, padj=1)
	mtext("RF", outer=TRUE, side=2, adj=0.88, padj=-0.5, las=0)
	mtext("Sym", outer=TRUE, side=2, adj=0.62, padj=-0.5, las=0)
	mtext("FPN", outer=TRUE, side=2, adj=0.36, padj=-0.5, las=0)
	mtext("Euc", outer=TRUE, side=2, adj=0.11, padj=-0.5, las=0)

	
	return()
	}




########################################################
# TRACING THE ANNOYING R/APE PHYLO OBJECT FORMAT
########################################################

edges_of_droptip <- function (phy, tip, trim.internal = TRUE, subtree = FALSE, root.edge = 0) 
{	
	#nums = 1:dim(phy$edge)[1]
	##new_edges = phy$edge
	indlist = list()
	if (class(phy) != "phylo") 
		stop("object \"phy\" is not of class \"phylo\"")
	phy <- new2old.phylo(phy)
	phy$edge = cbind(phy$edge, 1:dim(phy$edge)[1])

	if (subtree) {
		trim.internal <- TRUE
		edge.bak <- phy$edge
	}
	tmp <- as.numeric(phy$edge)
	nb.tip <- max(tmp)
	nodes <- setdiff(tmp, 1:nb.tip)
	nobr <- is.null(phy$edge.length)
	if (is.numeric(tip)) 
		tip <- phy$tip.label[tip]
	del <- phy$tip.label %in% tip
	ind <- which(phy$edge[, 2] %in% as.character(which(del)))
	print("Saving ind1 to indlist")
	length_indlist = length(indlist) + 1
	indlist[[length_indlist]] = ind
  	phy$edge <- phy$edge[-ind, ]
  	#new_edges <- #new_edges[-ind, ]`
  	#nums <- #nums[-ind]
	if (!nobr) 
		phy$edge.length <- phy$edge.length[-ind]
	phy$tip.label <- phy$tip.label[!del]
	if (trim.internal) {
		if (root.edge) {
		    seq.nod <- list()
		    for (i in phy$edge[, 2][as.numeric(phy$edge[, 2]) > 
		        0]) {
		        vec <- i
		        j <- i
		        while (j != "-1") {
		          ind <- which(phy$edge[, 2] == j)
					print("Saving ind2 to indlist")
					length_indlist = length(indlist) + 1
					indlist[[length_indlist]] = ind

		          j <- phy$edge[ind, 1]
		          vec <- c(vec, j)
		        }
		        seq.nod[[i]] <- vec
		    }
		    sn <- lapply(seq.nod, rev)
		    i <- 1
		    x <- unlist(lapply(sn, function(x) x[i]))
		    while (length(unique(x)) == 1) {
		        x <- unlist(lapply(sn, function(x) x[i]))
		        i <- i + 1
		    }
		    MRCA <- sn[[1]][i - 2]
		    newrootedge <- if (is.null(phy$root.edge)) 
		        0
		    else phy$root.edge
		    for (i in 1:root.edge) {
		        ind <- which(phy$edge[, 2] == MRCA)
				print("Saving ind3 to indlist")
				length_indlist = length(indlist) + 1
				indlist[[length_indlist]] = ind
		        newrootedge <- newrootedge + phy$edge.length[ind]
		        MRCA <- phy$edge[ind, 1]
		        if (MRCA == "-1" && i < root.edge) {
		          newrootedge <- newrootedge
		          break
		        }
		    }
		    phy$root.edge <- newrootedge
		}
		else {
		    if (!is.null(phy$root.edge)) 
		        phy$root.edge <- NULL
		}
		while (!all(phy$edge[, 2][as.numeric(phy$edge[, 2]) < 
		    0] %in% phy$edge[, 1])) {
		    temp <- phy$edge[, 2][as.numeric(phy$edge[, 2]) < 
		        0]
		    k <- temp %in% phy$edge[, 1]
		    ind <- phy$edge[, 2] %in% temp[!k]
		    phy$edge <- phy$edge[!ind, ]
		    #nums <- #nums[-ind]

				print("Saving ind4 to indlist")
				length_indlist = length(indlist) + 1
				indlist[[length_indlist]] = ind
				#new_edges <- #new_edges[!ind, ]

		    if (!nobr) 
		        phy$edge.length <- phy$edge.length[!ind]
		}
	}
	else {
		k <- nodes %in% phy$edge[, 1]
		ind <- phy$edge[, 2] %in% nodes[!k]
		phy$edge[which(ind), 2] <- as.character(nb.tip + (1:sum(ind)))
		if (is.null(phy$node.label)) 
		    new.tip.label <- rep("NA", sum(ind))
		else new.tip.label <- phy$node.label[!k]
		phy$tip.label <- c(phy$tip.label, new.tip.label)
	}
	useless.nodes <- names(which(table(phy$edge[, 1]) == 1))
	if (subtree) {
		if (!nobr) 
		    mnbr <- mean(phy$edge.length)
		if (length(useless.nodes) == 1) 
		    n <- length(tip)
		else {
		    seq.nod <- list()
		    wh <- numeric(0)
		    for (i in as.character(which(del))) {
		        vec <- i
		        j <- i
		        while (!(j %in% useless.nodes)) {
		          ind <- which(edge.bak[, 2] == j)
		          wh <- c(wh, ind)
		          j <- edge.bak[ind, 1]
		          vec <- c(vec, j)
		        }
		        seq.nod[[i]] <- vec
		    }
		    n <- table(unlist(lapply(seq.nod, function(x) rev(x)[1])))
		}
		new.lab <- paste("[", n, "_tips]", sep = "")
		for (i in 1:length(useless.nodes)) {
		    wh <- which(phy$edge[, 1] == useless.nodes[i])
		    phy$tip.label <- c(phy$tip.label, new.lab[i])
		    if (wh == dim(phy$edge)[1]) {
		        phy$edge <- rbind(phy$edge, c(useless.nodes[i], 
		          as.character(nb.tip + i)))
		        if (!nobr) 
		          phy$edge.length <- c(phy$edge.length, mnbr)
		    }
		    else {
		        phy$edge <- rbind(phy$edge[1:wh, ], c(useless.nodes[i], 
		          as.character(nb.tip + i)), phy$edge[(wh + 1):dim(phy$edge)[1], 
		          ])
		        if (!nobr) 
		          phy$edge.length <- c(phy$edge.length[1:wh], 
		            mnbr, phy$edge.length[(wh + 1):(dim(phy$edge)[1] - 
		              1)])
		    }
		}
	}
	else {
		for (i in useless.nodes) {
		    ind1 <- which(phy$edge[, 1] == i)
		    ind2 <- which(phy$edge[, 2] == i)
		    phy$edge[ind2, 2] <- phy$edge[ind1, 2]
		    phy$edge <- phy$edge[-ind1, ]
		    #nums = #nums[-ind1]
		    if (!nobr) {
		        phy$edge.length[ind2] <- phy$edge.length[ind2] + 
		          phy$edge.length[ind1]
		        phy$edge.length <- phy$edge.length[-ind1]
		    }
		}
	}
	tmp <- as.numeric(phy$edge)
	if (!is.null(phy$node.label)) {
		x <- unique(tmp)
		x <- x[x < 0]
		phy$node.label <- phy$node.label[-x]
	}
	n <- length(tmp)
	nodes <- tmp < 0
	ind.nodes <- (1:n)[nodes]
	ind.tips <- (1:n)[!nodes]
	new.nodes <- -as.numeric(factor(-tmp[nodes]))
	new.tips <- as.numeric(factor(tmp[!nodes]))
	tmp[ind.nodes] <- new.nodes
	tmp[ind.tips] <- new.tips
	dim(tmp) <- c(n/2, 2)
	mode(tmp) <- "character"
	phy$edge <- tmp
	phy <- old2new.phylo(phy)
	if (!trim.internal || subtree) {
		S <- write.tree(phy)
		phy <- if (nobr) 
		    clado.build(S)
		else tree.build(S)
	}
	#return(list(nodes, tmp, new.nodes, new.tips, save_ind1, save_ind3, save_ind4))
	#return(indlist)
	##new_edges = cbind(#new_edges, 1:dim(#new_edges)[1])
	return(phy)
}



trace_edges <- function (phy, tip, trim.internal = TRUE, subtree = FALSE, root.edge = 0) 
{
	phy2 <- phy
	
	if (class(phy) != "phylo") 
		stop("object \"phy\" is not of class \"phylo\"")
	phy <- new2old.phylo(phy)
	phy2 <- new2old.phylo(phy2)
	phy2$edge[,2] = 1:dim(phy2$edge)[1]
	
	if (subtree) {
		trim.internal <- TRUE
		edge.bak <- phy$edge
	}
	tmp <- as.numeric(phy$edge)
	nb.tip <- max(tmp)
	nodes <- setdiff(tmp, 1:nb.tip)
	nobr <- is.null(phy$edge.length)
	if (is.numeric(tip)) 
		tip <- phy$tip.label[tip]
	del <- phy$tip.label %in% tip
	ind <- which(phy$edge[, 2] %in% as.character(which(del)))
	phy$edge <- phy$edge[-ind, ]
	phy2$edge <- phy2$edge[-ind, ]

	if (!nobr) 
		phy$edge.length <- phy$edge.length[-ind]
	phy$tip.label <- phy$tip.label[!del]
	if (trim.internal) {
		if (root.edge) {
		    seq.nod <- list()
		    for (i in phy$edge[, 2][as.numeric(phy$edge[, 2]) > 
		        0]) {
		        vec <- i
		        j <- i
		        while (j != "-1") {
		          ind <- which(phy$edge[, 2] == j)
		          j <- phy$edge[ind, 1]
		          vec <- c(vec, j)
		        }
		        seq.nod[[i]] <- vec
		    }
		    sn <- lapply(seq.nod, rev)
		    i <- 1
		    x <- unlist(lapply(sn, function(x) x[i]))
		    while (length(unique(x)) == 1) {
		        x <- unlist(lapply(sn, function(x) x[i]))
		        i <- i + 1
		    }
		    MRCA <- sn[[1]][i - 2]
		    newrootedge <- if (is.null(phy$root.edge)) 
		        0
		    else phy$root.edge
		    for (i in 1:root.edge) {
		        ind <- which(phy$edge[, 2] == MRCA)
		        newrootedge <- newrootedge + phy$edge.length[ind]
		        MRCA <- phy$edge[ind, 1]
		        if (MRCA == "-1" && i < root.edge) {
		          newrootedge <- newrootedge
		          break
		        }
		    }
		    phy$root.edge <- newrootedge
		    phy2$root.edge <- newrootedge

		}
		else {
		    if (!is.null(phy$root.edge)) 
		        phy$root.edge <- NULL
		        phy2$root.edge <- NULL

		}
		while (!all(phy$edge[, 2][as.numeric(phy$edge[, 2]) < 
		    0] %in% phy$edge[, 1])) {
		    temp <- phy$edge[, 2][as.numeric(phy$edge[, 2]) < 
		        0]
		    k <- temp %in% phy$edge[, 1]
		    ind <- phy$edge[, 2] %in% temp[!k]
		    phy$edge <- phy$edge[!ind, ]
		    phy2$edge <- phy2$edge[!ind, ]
		    if (!nobr) 
		        phy$edge.length <- phy$edge.length[!ind]
		}

	}
	else {
		k <- nodes %in% phy$edge[, 1]
		ind <- phy$edge[, 2] %in% nodes[!k]
		phy$edge[which(ind), 2] <- as.character(nb.tip + (1:sum(ind)))
		phy2$edge[which(ind), 2] <- as.character(nb.tip + (1:sum(ind)))

		if (is.null(phy$node.label)) 
		    new.tip.label <- rep("NA", sum(ind))
		else new.tip.label <- phy$node.label[!k]
		phy$tip.label <- c(phy$tip.label, new.tip.label)
	}
	useless.nodes <- names(which(table(phy$edge[, 1]) == 1))
	if (subtree) {
		if (!nobr) 
		    mnbr <- mean(phy$edge.length)
		if (length(useless.nodes) == 1) 
		    n <- length(tip)
		else {
		    seq.nod <- list()
		    wh <- numeric(0)
		    for (i in as.character(which(del))) {
		        vec <- i
		        j <- i
		        while (!(j %in% useless.nodes)) {
		          ind <- which(edge.bak[, 2] == j)
		          wh <- c(wh, ind)
		          j <- edge.bak[ind, 1]
		          vec <- c(vec, j)
		        }
		        seq.nod[[i]] <- vec
		    }
		    n <- table(unlist(lapply(seq.nod, function(x) rev(x)[1])))
		}
		new.lab <- paste("[", n, "_tips]", sep = "")
		for (i in 1:length(useless.nodes)) {
		    wh <- which(phy$edge[, 1] == useless.nodes[i])
		    phy$tip.label <- c(phy$tip.label, new.lab[i])
		    if (wh == dim(phy$edge)[1]) {
		        phy$edge <- rbind(phy$edge, c(useless.nodes[i], 
		          as.character(nb.tip + i)))
		        phy2$edge <- rbind(phy2$edge, c(useless.nodes[i], 
		          as.character(nb.tip + i)))

		        if (!nobr) 
		          phy$edge.length <- c(phy$edge.length, mnbr)
		    }
		    else {
		        phy$edge <- rbind(phy$edge[1:wh, ], c(useless.nodes[i], 
		          as.character(nb.tip + i)), phy$edge[(wh + 1):dim(phy$edge)[1], 
		          ])
		        phy2$edge <- rbind(phy2$edge[1:wh, ], c(useless.nodes[i], 
		          as.character(nb.tip + i)), phy2$edge[(wh + 1):dim(phy$edge)[1], 
		          ])
		        if (!nobr) 
		          phy$edge.length <- c(phy$edge.length[1:wh], 
		            mnbr, phy$edge.length[(wh + 1):(dim(phy$edge)[1] - 
		              1)])
		    }
		}
	}
	else {
		for (i in useless.nodes) {
		    ind1 <- which(phy$edge[, 1] == i)
		    ind2 <- which(phy$edge[, 2] == i)
		    phy$edge[ind2, 2] <- phy$edge[ind1, 2]
		    phy2$edge[ind2, 2] <- phy2$edge[ind1, 2]

		    phy$edge <- phy$edge[-ind1, ]
		    phy2$edge <- phy2$edge[-ind1, ]
		    if (!nobr) {
		        phy$edge.length[ind2] <- phy$edge.length[ind2] + 
		          phy$edge.length[ind1]
		        phy$edge.length <- phy$edge.length[-ind1]
		    }
		}
	}
	tmp <- as.numeric(phy$edge)
	tmp2 <- as.numeric(phy2$edge)

	if (!is.null(phy$node.label)) {
		x <- unique(tmp)
		x <- x[x < 0]
		phy$node.label <- phy$node.label[-x]
	}
	n <- length(tmp)
	nodes <- tmp < 0
	ind.nodes <- (1:n)[nodes]
	ind.tips <- (1:n)[!nodes]
	new.nodes <- -as.numeric(factor(-tmp[nodes]))
	new.tips <- as.numeric(factor(tmp[!nodes]))
	tmp[ind.nodes] <- new.nodes
	tmp[ind.tips] <- new.tips
	dim(tmp) <- c(n/2, 2)
	mode(tmp) <- "character"
	phy$edge <- tmp
	phy <- old2new.phylo(phy)

	

	if (!trim.internal || subtree) {
		S <- write.tree(phy)
		phy <- if (nobr) 
		    clado.build(S)
		else tree.build(S)
	}
	old_edge_nums = as.numeric(phy2$edge[,2])
	return(old_edge_nums)
}





#########################
# Tree stats
#########################
continuous_tree_NJ <- function(params, colnames_to_use, normalize=TRUE, printflag=FALSE)
	{
	
	# subset parameters
	cols_tokeep = names(params) %in% colnames_to_use == TRUE
	tmp_params = params[, cols_tokeep]
	
	prflag(tmp_params, printflag)
	
	if (normalize == TRUE)
		{
		tmp_params = normalize_by_colmax(tmp_params)
		}

	prflag(tmp_params, printflag)

	
	distmat = dist(tmp_params)
	params_tr = nj(distmat)
	
	return(params_tr)
	}


shuffle_tips_dists_to_tree <- function(reftree, params_tr, N=100, topodist=TRUE, bhvdist=TRUE, rfdist=FALSE, treedists=TRUE)
	{
	# N=1000; topodist=TRUE; bhvdist=TRUE; rfdist=FALSE; treedists=TRUE
	
	reftree = unroot(reftree)
	tmp_params_tr = params_tr
	
	# make a null hypothesis
	tiplabs = params_tr$tip.label
	len_tiplabs = length(tiplabs)
	topodist_null = c()
	BHVdist_null = c()
	rfdist_null = c()
	treedists_null = c()
	for (i in 1:N)
		{
		# randomly shuffle tips, with replacement
		tmp_params_tr = params_tr
		
		newtips = sample(tiplabs, size=len_tiplabs, replace=FALSE)
		tmp_params_tr$tip.label = newtips
		
		if (topodist == TRUE)
			{
			tmp_topodist = dist.topo(tmp_params_tr, reftree, method="PH85")
			topodist_null = c(topodist_null, tmp_topodist)
			}
		if (bhvdist == TRUE)
			{
			tmp_BHVdist = dist.topo(tmp_params_tr, reftree, method="BHV01")
			BHVdist_null = c(BHVdist_null, tmp_BHVdist)
			}
		if (rfdist == TRUE)
			{
			tmp_rfdist = RF.dist(tmp_params_tr, reftree)
			rfdist_null = c(rfdist_null, tmp_rfdist)
			}
		if (treedists == TRUE)
			{
			tmp_treedists = treedist(tmp_params_tr, reftree)
			treedists_null = rbind(treedists_null, tmp_treedists)
			}
		}
	
	# make a big table
	dists_table = cbind(topodist_null, BHVdist_null, rfdist_null, treedists_null)
	
	# blank out the row.names (which are identical)
	row.names(dists_table)=c()
	
	dists_table = adf(dists_table)

	# devise the data.frame names
	tmpnames = c()
	if (topodist == TRUE)
		{
		tmpnames = c(tmpnames, "topodist")
		}
	if (bhvdist == TRUE)
		{
		tmpnames = c(tmpnames, "bhvdist")
		}
	if (rfdist == TRUE)
		{
		tmpnames = c(tmpnames, "rfdist")
		}
	if (treedists == TRUE)
		{
		tmpnames = c(tmpnames, names(tmp_treedists))
		}

	names(dists_table) = tmpnames
	
	
	# observed distances added as the last row
	dists_observed = c()
	if (topodist == TRUE)
		{
		tmpdist = dist.topo(params_tr, reftree, method="PH85")
		dists_observed = c(dists_observed, tmpdist)
		}
	if (bhvdist == TRUE)
		{
		tmpdist = dist.topo(params_tr, reftree, method="BHV01")
		dists_observed = c(dists_observed, tmpdist)
		}
	if (rfdist == TRUE)
		{
		tmpdist = RF.dist(params_tr, unroot(reftree))
		dists_observed = c(dists_observed, tmpdist)
		}
	if (treedists == TRUE)
		{
		tmpdist = treedist(params_tr, reftree)
		dists_observed = c(dists_observed, tmpdist)
		}
	
	dists_table = rbind(dists_table, dists_observed)
	
	return(dists_table)
	}


hist_obs_null_diffs <- function(reftree, params_tr, dists_infn=NULL, dists_outfn=NULL, N=100, reftreetxt="", paramstr_txt="", datatxt="")
	{
	# Calculate the distances, if necesary

	numdists = N
	if (is.null(dists_infn))
		{
		dists_table_final = shuffle_tips_dists_to_tree(reftree, params_tr, N=numdists)
		
		# write out distances
		write_table_good(dists_table_final, dists_outfn)
		} else {
		# read in observed distances
		dists_table_final = read_table_good(dists_infn)
		}

	# the observed distances are the last row of the null differences
	dists_observed = unlist(dists_table_final[nrow(dists_table_final), ])

	# do subplots for the two trees
	par(mfrow=c(1,2))
	
	maintxt = reftreetxt
	plot(unroot(reftree), type="unrooted", lab4ut="axial")
	title(maintxt, cex=0.8)
	
	maintxt = paste(paramstr_txt, "\n", datatxt, sep="")
	plot(params_tr, type="unrooted", lab4ut="axial")
	title(maintxt, cex=0.8)
	
	# do histograms of the six different distances
	# set outer margins & number of subplots
	par(oma=c(1,1,5,1), mfrow=c(2,3))
	# histogram all the distances (rfdist = symmetric distance, so exclude)
	for (i in 1:ncol(dists_table_final))
		{
		x = dists_table_final[, i]
		observed_val = dists_observed[i]
		#pval = empirical_pval_slow(x, observed_val)
		pval = round(empirical_pval_fast(x, observed_val), 4)
		tmp_title = paste(names(dists_table_final)[i], "\nnon-parametric p-value = ", pval, sep="")
		
		h = hist(x, main=tmp_title, breaks=50, cex.main=1.2, xlab="distance")
		
		# add the arrow
		arrowtop = 0.3*max(h$counts, na.rm=TRUE)
		arrowbot = 0.03*max(h$counts, na.rm=TRUE)
		arrows(observed_val, arrowtop, observed_val, arrowbot, col="blue", lwd=4)	
		}
	maintxt = paste("Observed distance between ", reftreetxt, " and ", paramstr_txt, "\ncompared to null distribution of distances with tips randomized\n", datatxt, ", N=", N, sep="")
	mtext(maintxt, outer=TRUE)

	}




# Extract a newick string from a NEXUS newick string
extract_newickstr_from_nexusstr <- function(nexusstr, delimiter = " = ")
	{
	words = strsplit(nexusstr, split=delimiter)[[1]]
	newickstr = words[2]
	return(newickstr)
	}




# Source: https://svn.mpl.ird.fr/ape/dev/ape/R/drop.tip.R
# Fixed bug identified here:
# https://stat.ethz.ch/pipermail/r-sig-phylo/2010-November/000850.html
# 
## drop.tip.R (2010-11-24)

##   Remove Tips in a Phylogenetic Tree

## Copyright 2003-2010 Emmanuel Paradis

## This file is part of the R-package `ape'.
## See the file ../COPYING for licensing issues.

extract.clade <- function(phy, node, root.edge = 0, interactive = FALSE)
{
    Ntip <- length(phy$tip.label)
    ROOT <- Ntip + 1
    Nedge <- dim(phy$edge)[1]
    wbl <- !is.null(phy$edge.length)
    if (interactive) node <- identify(phy)$nodes else {
        if (length(node) > 1) {
            node <- node[1]
            warning("only the first value of 'node' has been considered")
        }
        if (is.character(node)) {
            if (is.null(phy$node.label))
                stop("the tree has no node labels")
            node <- which(phy$node.label %in% node) + Ntip
        }
        if (node <= Ntip)
            stop("node number must be greater than the number of tips")
    }
    if (node == ROOT) return(phy)
    phy <- reorder(phy) # insure it is in cladewise order
    root.node <- which(phy$edge[, 2] == node)
    start <- root.node + 1 # start of the clade looked for
    anc <- phy$edge[root.node, 1] # the ancestor of 'node'
    next.anc <- which(phy$edge[-(1:start), 1] <= anc) # find the next occurence of 'anc' or an 'older' node

    keep <- if (length(next.anc)) start + 0:(next.anc[1] - 1) else start:Nedge

    if (root.edge) {
        NewRootEdge <- phy$edge.length[root.node]
        root.edge <- root.edge - 1
        while (root.edge) {
            if (anc == ROOT) break
            i <- which(phy$edge[, 2] ==  anc)
            NewRootEdge <- NewRootEdge + phy$edge.length[i]
            root.edge <- root.edge - 1
            anc <- phy$edge[i, 1]
        }
        if (root.edge && !is.null(phy$root.edge))
            NewRootEdge <- NewRootEdge + phy$root.edge
        phy$root.edge <- NewRootEdge
    }

    phy$edge <- phy$edge[keep, ]
    if (wbl) phy$edge.length <- phy$edge.length[keep]
    TIPS <- phy$edge[, 2] <= Ntip
    tip <- phy$edge[TIPS, 2]
    phy$tip.label <- phy$tip.label[sort(tip)] # <- added sort to avoid shuffling of tip labels (2010-07-21)
    ## keep the ordering so no need to reorder tip.label:
    phy$edge[TIPS, 2] <- order(tip)
    if (!is.null(phy$node.label))
        phy$node.label <- phy$node.label[sort(unique(phy$edge[, 1])) - Ntip]
    Ntip <- length(phy$tip.label)
    phy$Nnode <- dim(phy$edge)[1] - Ntip + 1L
    ## The block below renumbers the nodes so that they conform
    ## to the "phylo" format -- same as in root()
    newNb <- integer(Ntip + phy$Nnode)
    newNb[node] <- Ntip + 1L
    sndcol <- phy$edge[, 2] > Ntip
    ## executed from right to left, so newNb is modified before phy$edge:
    phy$edge[sndcol, 2] <- newNb[phy$edge[sndcol, 2]] <-
        (Ntip + 2):(Ntip + phy$Nnode)
    phy$edge[, 1] <- newNb[phy$edge[, 1]]
    phy
}

drop.tip <-
    function(phy, tip, trim.internal = TRUE, subtree = FALSE,
             root.edge = 0, rooted = is.rooted(phy), interactive = FALSE)
{
    if (!inherits(phy, "phylo"))
        stop('object "phy" is not of class "phylo"')

    Ntip <- length(phy$tip.label)
    ## find the tips to drop:
    if (interactive) {
        cat("Left-click close to the tips you want to drop; right-click when finished...\n")
        xy <- locator()
        nToDrop <- length(xy$x)
        tip <- integer(nToDrop)
        lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
        for (i in 1:nToDrop) {
            d <- sqrt((xy$x[i] - lastPP$xx)^2 + (xy$y[i] - lastPP$yy)^2)
            tip[i] <- which.min(d)
        }
    } else {
        if (is.character(tip))
            tip <- which(phy$tip.label %in% tip)
    }
    if (any(tip > Ntip))
        warning("some tip numbers were higher than the number of tips")

    if (!rooted && subtree) {
        phy <- root(phy, (1:Ntip)[-tip][1])
        root.edge <- 0
    }

    phy <- reorder(phy)
    NEWROOT <- ROOT <- Ntip + 1
    Nnode <- phy$Nnode
    Nedge <- dim(phy$edge)[1]
    if (subtree) {
        trim.internal <- TRUE
        tr <- reorder(phy, "pruningwise")
        N <- .C("node_depth", as.integer(Ntip), as.integer(Nnode),
                as.integer(tr$edge[, 1]), as.integer(tr$edge[, 2]),
                as.integer(Nedge), double(Ntip + Nnode),
                DUP = FALSE, PACKAGE = "ape")[[6]]
    }
    wbl <- !is.null(phy$edge.length)
    edge1 <- phy$edge[, 1] # local copies
    edge2 <- phy$edge[, 2] #
    keep <- !logical(Nedge)

    ## delete the terminal edges given by `tip':
    keep[match(tip, edge2)] <- FALSE

    if (trim.internal) {
        ints <- edge2 > Ntip
        ## delete the internal edges that do not have anymore
        ## descendants (ie, they are in the 2nd col of `edge' but
        ## not in the 1st one)
        repeat {
            sel <- !(edge2 %in% edge1[keep]) & ints & keep
            if (!sum(sel)) break
            keep[sel] <- FALSE
        }
        if (subtree) {
            ## keep the subtending edge(s):
            subt <- edge1 %in% edge1[keep] & edge1 %in% edge1[!keep]
            keep[subt] <- TRUE
        }
        if (root.edge && wbl) {
            degree <- tabulate(edge1[keep])
            if (degree[ROOT] == 1) {
                j <- integer(0) # will store the indices of the edges below the new root
                repeat {
                    i <- which(edge1 == NEWROOT & keep)
                    j <- c(i, j)
                    NEWROOT <- edge2[i]
                    degree <- tabulate(edge1[keep])
                    if (degree[NEWROOT] > 1) break
                }
                keep[j] <- FALSE
                if (length(j) > root.edge) j <- 1:root.edge
                NewRootEdge <- sum(phy$edge.length[j])
                if (length(j) < root.edge && !is.null(phy$root.edge))
                    NewRootEdge <- NewRootEdge + phy$root.edge
                phy$root.edge <- NewRootEdge
            }
        }
    }

    if (!root.edge) phy$root.edge <- NULL

    ## drop the edges
    phy$edge <- phy$edge[keep, ]
    if (wbl) phy$edge.length <- phy$edge.length[keep]

    ## find the new terminal edges (works whatever 'subtree' and 'trim.internal'):
    TERMS <- !(phy$edge[, 2] %in% phy$edge[, 1])

    ## get the old No. of the nodes and tips that become tips:
    oldNo.ofNewTips <- phy$edge[TERMS, 2]

    ## in case some tips are dropped but kept because of 'subtree = TRUE':
    if (subtree) {
        i <- which(tip %in% oldNo.ofNewTips)
        if (length(i)) {
            phy$tip.label[tip[i]] <- "[1_tip]"
            tip <- tip[-i]
        }
    }

    n <- length(oldNo.ofNewTips) # the new number of tips in the tree

    ## the tips may not be sorted in increasing order in the
    ## 2nd col of edge, so no need to reorder $tip.label
    phy$edge[TERMS, 2] <- rank(phy$edge[TERMS, 2])
    phy$tip.label <- phy$tip.label[-tip]

    ## make new tip labels if necessary:
    if (subtree || !trim.internal) {
        ## get the numbers of the nodes that become tips:
        node2tip <- oldNo.ofNewTips[oldNo.ofNewTips > Ntip]
        new.tip.label <- if (subtree) {
            paste("[", N[node2tip], "_tips]", sep = "")
        } else {
            if (is.null(phy$node.label)) rep("NA", length(node2tip))
            else phy$node.label[node2tip - Ntip]
        }
        if (!is.null(phy$node.label))
            phy$node.label <- phy$node.label[-(node2tip - Ntip)]
        phy$tip.label <- c(phy$tip.label, new.tip.label)
    }

    ## update node.label if needed:
    if (!is.null(phy$node.label))
        phy$node.label <- phy$node.label[sort(unique(phy$edge[, 1])) - Ntip]

    phy$Nnode <- dim(phy$edge)[1] - n + 1L # update phy$Nnode

    ## The block below renumbers the nodes so that they conform
    ## to the "phylo" format -- same as in root()
    newNb <- integer(n + phy$Nnode)
    newNb[NEWROOT] <- n + 1L
    sndcol <- phy$edge[, 2] > n
    ## executed from right to left, so newNb is modified before phy$edge:
    phy$edge[sndcol, 2] <- newNb[phy$edge[sndcol, 2]] <-
        (n + 2):(n + phy$Nnode)
    phy$edge[, 1] <- newNb[phy$edge[, 1]]
    storage.mode(phy$edge) <- "integer"
    collapse.singles(phy)
}






###############################################
# Tree plotting
###############################################

plot_cont_vals_on_tree <- function(chtr2, tmptipvals, tmpnodevals, texttxt)
	{
	# Plot some continuous values on a tree in pie chart form
	#
	tree_ht = get_max_height_tree(chtr2)
	numtips = length(chtr2$tip.label)
	numnodes = chtr2$Nnode
	label_offset = 0.05*tree_ht
	plot(chtr2, show.node.label=FALSE, cex=0.9, no.margin = TRUE, x.lim=1.6*tree_ht, label.offset=label_offset)
	
	param_vals = c(tmptipvals, tmpnodevals)
	maxval = max(param_vals)
	param_vals_normed = param_vals / maxval
	
	tipvals = param_vals_normed[1:numtips]
	nodevals = param_vals_normed[(numtips+1) : (numtips+numnodes)]
	tmp_colors = c("darkgray", "white")
	tiplabels(pch = ".", pie=tipvals, piecol=tmp_colors, cex = 1) # adj = 0)
	nodelabels(pie=nodevals, cex=1, piecol=tmp_colors)
	
	legend(0, 3, legend=c("fraction of max value"), fill=tmp_colors)
	text(0, 4, labels=texttxt, pos=4)
	
	}






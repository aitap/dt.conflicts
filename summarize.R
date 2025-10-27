parse_status <- function(status)
	if (any(grepl('\t', status))) { # git diff
		ret <- strsplit(status, '\t')
		l <- lengths(ret)
		stopifnot(l %in% 2:3)
		rbind(
			data.frame(
				state = vapply(ret[l == 2], `[[`, '', 1),
				file  = vapply(ret[l == 2], `[[`, '', 2)
			),
			data.frame(
				state = as.vector(outer(c('->%s', '%s->'), vapply(ret[l == 3], `[[`, '', 1), sprintf)),
				file  = unlist(lapply(ret[l == 3], `[`, 2:3))
			)
		)
	} else { # git status --porcelain
		stopifnot(substring(status, 3, 3) == ' ')
		ret <- data.frame(
			state = substring(status, 1, 2),
			# skip space in position 3
			file  = substring(status, 4, nchar(status))
		)

		# kludge for v1 porcelain renames
		which.renames <- grepl(' -> ', ret$file)

		rbind(
			ret[!which.renames,],
			data.frame(
				state = as.vector(outer(c('->%s', '%s->'), ret$state[which.renames], sprintf)),
				file  = unlist(strsplit(ret$file[which.renames], ' -> '))
			)
		)
	}

summarize_one_merge <- function(l, pr1, pr2)
	data.frame(
		pr1 = pr1, pr2 = pr2, merge = l$merge,
		tree = if (length(l$tree)) l$tree else NA_character_
	)

render_merge_matrix <- function(mm, con = stdout(), render_conflicts = FALSE) {
	merges_to_base <- vapply(diag(mm), \(.) isTRUE(.$merge), FALSE)
	mm <- mm[merges_to_base,merges_to_base,drop=FALSE]

	if (nrow(mm) > 1) {
		mm |> vapply(\(.) if (!is.na(.$merge)) 1-.$merge else 1, 1) |>
			matrix(nrow(mm), ncol(mm)) |> as.dist() |> hclust() -> h
		mm <- mm[h$order,h$order]
	}

	writeLines('<table>', con)
	writeLines('<tr><td></td>', con)
	for (n in colnames(mm))
		cat('<th><a href="#pr', n, '">', n, '</a></th>', file = con, sep = '')
	writeLines('</tr>', con)

	for (i in seq_len(nrow(mm))) {
		writeLines('<tr>', con)
		ni <- rownames(mm)[[i]]
		cat('<th><a href="#pr', ni, '">', ni, '</a></th>', file = con, sep = '')
		for (j in seq_len(ncol(mm))) {
			nj <- colnames(mm)[[j]]

			if (ni == nj) {
				cat('<td><a href="#pr', ni, '">', ni, '</a></th>', file = con, sep = '')
				next
			}

			m <- mm[[i,j]]
			if (isTRUE(m$merge)) {
				cat('<td class="merge-ok"></td>', file = con)
			} else if (isFALSE(m$merge)) {
				cat(
					'<td class="merge-fail">',
					if (render_conflicts) paste0('<a href="#conflict-', ni, '-', nj, '">fail</a></td>'),
					file = con, sep = ''
				)
			} else { # NA
				cat('<td></td>', file = con)
			}
		}
		cat('<th><a href="#pr', ni, '">', ni, '</a></th>', file = con, sep = '')
		writeLines('</tr>', con)
	}

	# footer
	writeLines('<tr><td></td>', con)
	for (n in colnames(mm))
		cat('<th><a href="#pr', n, '">', n, '</a></th>', file = con, sep = '')
	writeLines('<td></td></tr>', con)

	writeLines('</table>', con)
}

path_to_id <- function(x, name) paste0('byfile-', name, '-', gsub('[^a-zA-Z0-9]', '_', x))

render_by_file <- function(prs, name, mm, con = stdout()) {
	cat('<h3 id="byfile-', name, '">By file</h3>', file = con, sep = '')
	prs |> do.call(what = rbind) |>
		aggregate(pr ~ file, data = _, \(.) list(sort(.))) -> byfile
	writeLines('<table><tr><th>File</th><th>PR</th></tr>', con)
	for (i in order(byfile$file)) {
		cat(
			'<tr><td><code id="', path_to_id(byfile$file[[i]], name), '">',
			byfile$file[[i]], '</code></td><td>',
			file = con, sep = ''
		)
		for (pr in byfile$pr[[i]]) {
			pr <- as.character(pr)
			cat(
				' <a',
				if (isFALSE(mm[[pr,pr]]$merge)) ' class="merge-fail"' else '',
				' href="#pr', pr, '">', pr, '</a>',
				file = con, sep = ''
			)
		}
		writeLines('</td></tr>', con)
	}
	writeLines('</table>', con)
}

get_prs <- function(mm)
	rownames(mm)[order(as.numeric(rownames(mm)))] |>
	setNames(nm = _) |> lapply(\(pr)
		parse_status(mm[[pr,pr]]$status) |> cbind(pr = as.numeric(pr))
	)

render_status <- function(pr, name, con) {
	writeLines('<table>', con)
	for (i in order(pr$file)) {
		cat(
			'<tr><td><code>', pr$state[[i]], '</code></td><td>',
			'<code><a href="#', path_to_id(pr$file[[i]], name), '">', pr$file[[i]], '</a></code></td></tr>\n',
			file = con, sep = ''
		)
	}
	writeLines('</table>', con)
}

render_by_pr <- function(prs, name, mm, con = stdout()) {
	cat('<h3 id="bypr-', name, '">By PR</h3>', file = con, sep = '')
	for (n in names(prs)) {
		cat('<h4 id="pr', n, '">', n, '</h4>\n', file = con, sep = '')
		cat('<p><a href="https://github.com/rdatatable/data.table/pull/', n, '">View on GitHub</a></p>\n', file = con, sep = '')

		if (isTRUE(mm[[n,n]]$merge)) {
			conflicts <- vapply(
				colnames(mm),
				\(pr2) isTRUE(mm[[pr2,pr2]]$merge) && !isTRUE(mm[[n,pr2]]$merge),
				TRUE
			)
			if (any(conflicts)) {
				cat('<p>Conflicts: ', file = con)
				conflicts <- sort(as.numeric(setdiff(colnames(mm)[conflicts], n)))
				cat(
					paste0('<a href="#pr', conflicts, '">', conflicts, '</a>', collapse = ', '),
					file = con
				)
				cat('</p>\n', file = con)
			}

			commutes <- vapply(
				colnames(mm),
				\(pr2) isTRUE(mm[[pr2,pr2]]$merge) && isTRUE(mm[[n,pr2]]$merge),
				TRUE
			)
			if (any(commutes)) {
				cat('<p>Merges cleanly with: ', file = con)
				commutes <- sort(as.numeric(setdiff(colnames(mm)[commutes], n)))
				cat(
					paste0('<a href="#pr', commutes, '">', commutes, '</a>', collapse = ', '),
					file = con
				)
				cat('</p>\n', file = con)
			}
		} else {
			writeLines('<p class="merge-fail">Fails to merge into base</p>', con)
		}

		pr <- prs[[n]]
		render_status(pr, name, con)
	}
}

render_by_conflict <- function(mm, name, con = stdout()) {
	cat('<h3 id="byconflict-', name, '">By conflict</h3>', file = con, sep = '')
	for (i in seq_len(nrow(mm))) {
		if (!isTRUE(mm[[i,i]]$merge)) next
		pr1 <- rownames(mm)[[i]]
		for (j in seq_len(ncol(mm))) {
			if (!isTRUE(mm[[j,j]]$merge)) next
			if (!isFALSE(mm[[i,j]]$merge)) next
			pr2 <- colnames(mm)[[j]]
			cat(
				'<h4 id="conflict-', pr1, '-', pr2, '">Conflict: ',
				pr1, ', then ', pr2, '</h4>\n',
				file = con, sep = ''
			)
			render_status(parse_status(mm[[pr1, pr2]]$status), name, con)
		}
	}
}

summarize_matrix <- function(mm, name, con = stdout(), render_conflicts = FALSE) {
	trees <- matrix(
		vapply(mm, \(.) if (length(.$tree)) .$tree else NA_character_, ''),
		ncol(mm), nrow(mm)
	)
	stopifnot('Not all pull requests commute over merging!' = identical(trees, t(trees)))

	cat('<h2 id="base-', name, '">', name, '</h2>\n', file = con, sep = '')
	writeLines(c(
		'<ul>',
		paste0('<li><a href="#byfile-', name, '">By file</a>'),
		paste0('<li><a href="#bypr-', name, '">By PR</a>'),
		if (render_conflicts) paste0('<li><a href="#byconflict-', name, '">By conflict</a>'),
		'</ul>'
	), con)

	render_merge_matrix(mm, con, render_conflicts)
	get_prs(mm) -> prs
	render_by_file(prs, name, mm, con)
	render_by_pr(prs, name, mm, con)
	if (render_conflicts) render_by_conflict(mm, name, con)
}

summarize_all <- function(l, when, con = stdout(), render_conflicts = FALSE) {
	if (is.character(con)) {
		con <- file(con, 'w')
		on.exit(close(con))
	}
	writeLines(c(
		'<!doctype html>',
		'<html><head>',
		'<title>Pull request conflicts</title>',
		'<style> .merge-ok { background: #BBDEB1; } .merge-fail { background: #FFC5D0; } </style>',
		'</head>',
		'<body><h1>Pull request conflicts</h1>',
		paste0('<p>Last updated on: ', as.Date(when), '</p>'),
		'<p>Available merge bases:</p>'
	), con)

	writeLines('<ul>', con)
	for (n in names(l)) cat(
		'<li><a href="#base-', n, '">', n, '</a>\n',
		file = con, sep = ''
	)
	writeLines('</ul>', con)

	for (n in names(l)) summarize_matrix(l[[n]], n, con, render_conflicts)

	writeLines('</body></html>', con)
}

args <- commandArgs(TRUE)
if (length(args) == 3) {
	summarize_all(readRDS(args[[1]]), file.mtime(args[[1]]), args[[2]], as.logical(args[[3]]))
} else if (length(args) != 0)
	stop("Usage: summarise.R merge.rds output.html {TRUE|FALSE}")

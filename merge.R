input <- 'open-pulls.rds'
checkout <- tempfile()
merge <- 'both'
output <- 'merge.rds'

args <- commandArgs(TRUE)
i <- 0; while ((i <- i + 1) <= length(args)) switch(args[[i]],
	'-i' = input <- args[[(i <- i + 1)]],
	'-c' = checkout <- args[[(i <- i + 1)]],
	'-m' = merge <- args[[(i <- i + 1)]],
	'-o' = output <- args[[(i <- i + 1)]],
	'-h' = {
		writeLines('Usage: merge.R [-i <open-pulls.rds>] [-c <git checkout directory>] [-m {both|second}] [-o <output.rds>]')
		q('no')
	}
)
stopifnot(merge %in% c('both', 'second'))

readRDS(input) -> pulls

if (dir.exists(checkout)) {
	o <- setwd(checkout)
	tryCatch(
		system('git fetch origin'),
		finally = setwd(o)
	)
} else {
	stopifnot(system(sprintf(
		'git clone https://github.com/Rdatatable/data.table %s',
		shQuote(checkout)
	)) == 0)
}

o <- setwd(checkout)
tryCatch(
	for (p in pulls) {
		stopifnot(system(sprintf('git fetch -f origin pull/%d/head:_pr%d_head_', p$number, p$number)) == 0)
		switch(merge,
			both = {
				# NB: refs/pull/%d/merge exists outdated for PRs that no longer merge clearly, so re-create it
				stopifnot(system(sprintf('git checkout -B _pr%d_merge_ origin/%s', p$number, p$base$ref)) == 0)
				ret <- system(sprintf('git merge --no-edit --commit --no-ff _pr%d_head_', p$number))
				# but remove the branch iif merge fails
				if (ret != 0) {
					stopifnot(system('git merge --abort') == 0)
					stopifnot(system('git switch master') == 0)
					stopifnot(system(sprintf('git branch -D _pr%d_merge_', p$number)) == 0)
				}
			},
			second = stopifnot(
				# only merge the second PR into first
				system(sprintf('git checkout -B _pr%d_merge_ _pr%d_head_', p$number, p$number)) == 0
			)
		)
	},
	finally = setwd(o)
)

merge_two <- function(first, second) {
	o <- setwd(checkout)
	on.exit(setwd(o), TRUE, FALSE)

	ret <- system(sprintf('git switch _pr%d_merge_', first$number))
	if (ret != 0) { # did not merge cleanly
		if (identical(first, second)) {
			# need to provide details for the diagonal element, so redo the work :(
			stopifnot(system(sprintf('git checkout origin/%s', first$base$ref)) == 0)
			stopifnot(system(sprintf('git merge --no-edit --no-commit --no-ff _pr%d_head_', first$number)) != 0)
			on.exit(stopifnot(system('git merge --abort') == 0), TRUE, FALSE)
			return(list(list(
				merge = FALSE,
				status = system('git status --porcelain=v1', intern = TRUE),
				tree = NA_character_
			)))
		}

		# otherwise nothing to do
		return(list(list(merge = NA, tree = NA_character_)))
	}

	# diagonal element, success case: show changes and the tree hash
	if (identical(first, second))
		return(list(list(
			merge = TRUE,
			status = system(
				sprintf(
					'git diff --name-status origin/%s..._pr%d_merge_',
					first$base$ref,
					first$number
				),
				intern = TRUE
			),
			tree = system(
				sprintf('git show -s --pretty=%%T _pr%d_merge_', first$number),
				intern = TRUE
			)
		)))

	# else merging the second branch after first is merged
	stopifnot(system(sprintf('git checkout -B _pr%d_then_%d_', first$number, second$number)) == 0)
	ret <- system(sprintf('git merge --no-edit --commit --no-ff _pr%d_head_', second$number))
	if (ret != 0) {
		on.exit(system('git merge --abort'), TRUE, FALSE)
		return(list(list(
			merge = FALSE,
			status = system('git status --porcelain=v1', intern = TRUE),
			tree = NA_character_
		)))
	}

	list(list(
		merge = TRUE,
		# return the _tree_ hash, not the commit hash, to see if two PRs commute
		tree = system(
			sprintf('git show -s --pretty=%%T _pr%d_then_%d_', first$number, second$number),
			intern = TRUE
		),
		status = system(
			sprintf('git diff --name-status origin/%s _pr%d_then_%d_', first$base$ref, first$number, second$number),
			intern = TRUE
		)
	))
}

merge_all_pairwise <- function(pulls) {
	ret <- outer(pulls, pulls, Vectorize(merge_two))
	nums <- sapply(pulls, `[[`, 'number')
	dimnames(ret) <- list(first = nums, second = nums)
	ret
}

split(pulls, sapply(pulls, \(x) x$base$label)) |>
	lapply(merge_all_pairwise) |>
	saveRDS(output)

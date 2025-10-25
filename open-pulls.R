gh::gh(
	'/repos/rdatatable/data.table/pulls', state = 'open',
	.limit = Inf, .token = ''
) |> saveRDS('open-pulls.rds')

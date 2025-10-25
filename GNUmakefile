all: report-second.html report-both.html
R = R
CHECKOUT = ~/temp/datatable.git

.PHONY: all deploy
.NOTPARALLEL:

open-pulls.rds: open-pulls.R
	$(R)script open-pulls.R

merge-%.rds: open-pulls.rds
	$(R)script merge.R -i open-pulls.rds -c '$(CHECKOUT)' -m $* -o $@

report-second.html: merge-second.rds summarize.R
	$(R)script summarize.R merge-second.rds report-second.html FALSE

report-both.html: merge-both.rds summarize.R
	$(R)script summarize.R merge-both.rds report-both.html TRUE

deploy: report-second.html report-both.html
	! git status --porcelain | grep -v '^??'
	git checkout -f --orphan deploy
	git rm --cached -r .
	git add report-*.html *.rds
	git commit -m deploy
	git push -f origin deploy:pages
	git checkout -f trunk
	git branch -D deploy

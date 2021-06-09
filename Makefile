.PHONY : clean compile run report

all:

compile:
	cabal build

run:
	cabal run

hlint:
	mv .hlint.yaml .hlint.yaml.old
	hlint . --report
	hlint . --default > .hlint.yaml

report:
	cd report && $(MAKE) report.pdf && $(MAKE) clean && mv report.pdf ..

clean:
	rm -rf dist dist-newstyle report.html report.pdf

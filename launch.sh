echo $run_what;
mkdir -p $out_dir;

if [ $run_what == "am" ] || [ $run_what == "am_data" ] ; then
	Rscript /home/repos/stonksanalysis/implement/get_data.R /home/repos/stonksanalysis
fi
if [ $run_what == "am" ] || [ $run_what == "am_out" ] ; then
	Rscript /home/repos/stonksanalysis/implement/am_regression.R && \
	Rscript /home/repos/stonksanalysis/implement/am_stocks.R && \
	Rscript /home/repos/stonksanalysis/implement/am_etfs.R /home/repos/stonksanalysis
fi
if [ $run_what == "am_news" ] ; then
	Rscript /home/repos/stonksanalysis/implement_archive/apply_news.R /home/repos/stonksanalysis
fi
if [ $run_what == "pm" ] ; then
	Rscript /home/repos/stonksanalysis/implement/pm_stocks.R /home/repos/stonksanalysis
	Rscript /home/repos/stonksanalysis/implement/pm_etfs.R /home/repos/stonksanalysis
fi

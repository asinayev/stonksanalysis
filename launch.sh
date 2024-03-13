echo $run_what;
mkdir -p $out_dir;

if [ $run_what == "am" ] || [ $run_what == "am_data" ] ; then
	Rscript /home/repos/stonksanalysis/implement/get_data.R /home/repos/stonksanalysis
fi
if [ $run_what == "am" ] || [ $run_what == "am_out" ] ; then
	Rscript /home/repos/stonksanalysis/implement/am_regression.R /home/repos/stonksanalysis && \
	Rscript /home/repos/stonksanalysis/implement/am_stocks.R /home/repos/stonksanalysis && \
	Rscript /home/repos/stonksanalysis/implement/am_etfs.R /home/repos/stonksanalysis  && \
	Rscript /home/repos/stonksanalysis/implement/am_earnings.R /home/repos/stonksanalysis 
fi
if [ $run_what == "night_snapshot" ] ; then	
	Rscript /home/repos/stonksanalysis/implement/nightmoves.R /home/repos/stonksanalysis && \
	Rscript /home/repos/stonksanalysis/implement/am_news.R /home/repos/stonksanalysis
fi
if [ $run_what == "snapshot" ] ; then
	Rscript /home/repos/stonksanalysis/implement/get_snapshot.R /home/repos/stonksanalysis
fi
if [ $run_what == "pm" ] ; then
	Rscript /home/repos/stonksanalysis/implement/pm_etfs.R /home/repos/stonksanalysis
fi

if [ $run_what == "all" || $run_what == "news" ] ; then
	Rscript /home/repos/stonksanalysis/apply_news.R /home/repos/stonksanalysis
fi
if [ $run_what == "all" || $run_what == "data" ] ; then
	Rscript /home/repos/stonksanalysis/implement_foreign/get_data.R
fi
if [ $run_what == "all" || $run_what == "pred" ] ; then
	Rscript /home/repos/stonksanalysis/implement_foreign/regression_outs.R && \
	Rscript /home/repos/stonksanalysis/implement_foreign/respond_overnight.R
fi

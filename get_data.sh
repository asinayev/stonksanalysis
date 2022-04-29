sudo docker build stonksanalysis/. -t myr && 
	sudo docker run  -e POLYGONKEY='ppoplLD3u3jWBvWknyOWvjiN4B2WfrA9' -e run_what='all' -v /tmp/:/tmp/ myr 

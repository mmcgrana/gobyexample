# Our running program shows the 9 jobs being executed by
# various workers. The program only takes about 3 seconds
# despite doing about 9 seconds of total work because
# there are 3 workers operating concurrently.
$ time go run worker-pools.go 
worker 1 processing job 1
worker 2 processing job 2
worker 3 processing job 3
worker 1 processing job 4
worker 2 processing job 5
worker 3 processing job 6
worker 1 processing job 7
worker 2 processing job 8
worker 3 processing job 9

real	0m3.149s

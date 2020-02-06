# Our running program shows the 5 jobs being executed by
# various workers. The program only takes about 2 seconds
# despite doing about 5 seconds of total work because
# there are 3 workers operating concurrently.
$ time go run worker-pools.go 
worker 3 started  job 1
worker 1 started  job 2
worker 2 started  job 3
worker 1 finished job 2
worker 1 started  job 4
worker 2 finished job 3
worker 2 started  job 5
result 4
result 6
worker 3 finished job 1
result 2
worker 2 finished job 5
result 10
worker 1 finished job 4
result 8

real	0m2.334s
user	0m0.246s
sys	  0m0.057s

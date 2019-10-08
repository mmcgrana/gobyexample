# Наша программа показывает 5 заданий, выполняемых
# различными воркерами. Программа занимает всего около
# 2 секунд, несмотря на выполнение около 5 секунд общей
# работы, потому что одновременно работают 3 воркера.
$ time go run worker-pools.go 
worker 1 started  job 1
worker 2 started  job 2
worker 3 started  job 3
worker 1 finished job 1
worker 1 started  job 4
worker 2 finished job 2
worker 2 started  job 5
worker 3 finished job 3
worker 1 finished job 4
worker 2 finished job 5

real	0m2.358s

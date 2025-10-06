$ go run channel-synchronization.go      
working...done                  

# If you removed the `<- done` line from this program,
# the program could exit before the `worker` finished
# its work, or in some cases even before it started.

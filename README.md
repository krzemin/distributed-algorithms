distributed-algorithms
======================

Implementation of several distributed algorithms in Erlang

What to do remoting
===============

First, on all computers of host <hostname_kth> (which is
simply ip address) erlang shell should be executed like that:

```erl -name machine_kth@hostname_kth -setcookie some_cookie```

Next, on every machine codebase must be compiled:

```
c(graph).
c(graph_broadcast).
```

Next, on some computer, let's say it is machine_1@hostname_1,
we set up cluster adding other computers:
 
```
net_kernel:connect_node('machine_2@hostname_2').
net_kernel:connect_node('machine_3@hostname_3').
...
net_kernel:connect_node('machine_N@hostname_N').
```

On computer 1 we are create processes accross the cluster.

```
G = graph:cycle(8).
Pids = graph_broadcast:setup_cluster(G).
```

Now we have the processes alive. Let's broadcast some
message. First we have to extract process id of some
process, let's say it is first.

```[{_, Pid}|_] = Pids.```

And send the message to this process.

```Pid ! 42```

This should begin broadcasting.
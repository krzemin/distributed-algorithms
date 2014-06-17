distributed-algorithms
======================

Implementation of several distributed algorithms in Erlang

Example session
===============

```
net_kernel:connect('maciek@192.168.1.191').
c(graph).                                  
c(graph_broadcast).
G = graph:cycle(4).
graph_broadcast:setup_cluster(G).                                                    
```
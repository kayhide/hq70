# q31

```
% stack exec -- q31 +RTS -s
100360
     227,257,792 bytes allocated in the heap
      17,710,440 bytes copied during GC
          63,888 bytes maximum residency (18 sample(s))
          29,152 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       201 colls,     0 par    0.019s   0.022s     0.0001s    0.0010s
  Gen  1        18 colls,     0 par    0.002s   0.003s     0.0002s    0.0011s

  INIT    time    0.000s  (  0.005s elapsed)
  MUT     time    0.039s  (  0.044s elapsed)
  GC      time    0.021s  (  0.025s elapsed)
  EXIT    time    0.000s  (  0.006s elapsed)
  Total   time    0.060s  (  0.079s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    5,884,001,553 bytes per MUT second

  Productivity  63.9% of total user, 54.9% of total elapsed
```

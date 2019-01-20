# q32

```
% stack exec -- q32 +RTS -s
----|--
|--||||
|--||||
----|--
14

--|----
||||--|
||||--|
--|----
14

|--|--|
|||||||
|||||||
|--|--|
14

------
|----|
||--||
||--||
|----|
15

|----|
||--||
||--||
|----|
------
15

  93,730,380,456 bytes allocated in the heap
  10,628,048,240 bytes copied during GC
          74,096 bytes maximum residency (9980 sample(s))
          29,152 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     80211 colls,     0 par   10.161s  10.286s     0.0001s    0.0015s
  Gen  1      9980 colls,     0 par    0.765s   0.783s     0.0001s    0.0032s

  INIT    time    0.000s  (  0.004s elapsed)
  MUT     time   20.826s  ( 21.027s elapsed)
  GC      time   10.926s  ( 11.069s elapsed)
  EXIT    time    0.000s  (  0.009s elapsed)
  Total   time   31.753s  ( 32.109s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    4,500,586,300 bytes per MUT second

  Productivity  65.6% of total user, 65.5% of total elapsed
```

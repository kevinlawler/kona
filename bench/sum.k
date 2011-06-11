/ sum benchmark
/ measures overhead of over and + dyad

`0:"sum benchmark: {\\t +/!_x}\n"
s:{time:. ("\\t +/!_" , 10.0$x); `0:($x)," - ",($time),"\n"}
s[1e4]
s[1e5]
s[1e6]
\\

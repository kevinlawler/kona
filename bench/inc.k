/ inc benchmark
/ measure overhead for each and curried + dyad

`0:"inc benchmark: {\\t (1+)'!_x}\n"
i:{time:. ("\\t (1+)'!_" , 10.0$x); `0:($x)," - ",($time),"\n"}
i[1e4]
i[1e5]
i[1e6]
\\

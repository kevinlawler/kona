/ tak benchmark
/ Measures the cost of recursive function calls.

/ (defun tak (x y z)
/   (if (< y x)
/       (tak (tak (- x 1) y z)
/            (tak (- y 1) z x)
/            (tak (- z 1) x y))
/       z))

tak:{:[y>x;z
       _f[_f[x-1;y;z]
          _f[y-1;z;x]
          _f[z-1;x;y]]]}

/ tak[18;12;6]
/ TODO: \t 10000 tak[18;12]/6

/ FIXME: type error

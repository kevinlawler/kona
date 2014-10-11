# [Kona](http://kona.github.io/)

![Kona](https://raw.githubusercontent.com/kevinlawler/kona/master/Kona.png)

What is Kona?
-------------

Kona is the open-source implementation of the K programming language. K is a synthesis of APL and LISP. Although many of the capabilities come from APL, the fundamental data construct is not the matrix. Like LISP, it is the list.  Also, like LISP, the K language is ASCII-based, so you don't need a special keyboard.

For many people, K is the preferred APL dialect. It tends to be popular with investment bankers, the performance obsessed, analysts with big data, and other smart people with not a lot of time. It is a demanding language.

K was originally designed by Arthur Whitney and [Kx Systems](http://kx.com/). Praise for K should be taken to refer to Kx's K. Kx sells a popular database called KDB+. People can and do create networked trading platforms in hours. If your business needs production K support, you can [evaluate KDB+ prior to purchasing from Kx](http://kx.com/software-download.php), or possibly speak with Kx consulting partner [First Derivatives](http://www.firstderivatives.com/).

Kx's KDB+ uses the Q language, and is built on top of K4. Kx used to sell a database called KDB, which used the KSQL language, and was built on top of K3, an earlier K. To the confusion of all, these terms are used interchangeably. Kx's K3 is basically no longer available. Kona is a reimplementation of that K. Kona targets K3 but includes features from K4 and elsewhere. Kona is unaffiliated with Kx.


Installation
------------

**Windows**

You can find an executable version of Kona [here](https://github.com/kevinlawler/kona/releases).

**Build from source**

For OSX, Linux, BSD and Cygwin:
Navigate to the directory you want to install Kona, then type:

    git clone https://github.com/kevinlawler/kona.git
    cd kona
    make                                #gmake on BSD

Then, while in the "kona" directory, run:

    ./k                      #./k_test for debug mode

For Windows: 
Pretty much the same process, but you will need MinGW-w64 (Mingw-builds project package).  To start up Kona from a native Widows "cmd", just type "k" when in the "kona" directory, as "./k" won't work.

Further Information
-------------------


You can find further information about Kona at [the wiki](https://github.com/kevinlawler/kona/wiki).



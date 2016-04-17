# [Kona](http://kona.github.io/) [![Build Status](https://travis-ci.org/kevinlawler/kona.svg?branch=master)](https://travis-ci.org/kevinlawler/kona)

![Kona](https://raw.githubusercontent.com/kevinlawler/kona/master/Kona.png)

What is Kona?
-------------

Kona is the open-source implementation of the K programming language. K is a synthesis of APL and LISP. Although many of the capabilities come from APL, the fundamental data construct is quite different. In APL the construct is a multi-dimensional matrix-like array, where the dimension of the array can range from 0 to some maximum (often 9). In K, like LISP, the fundamental data construct is a list.  Also, like LISP, the K language is ASCII-based, so you don't need a special keyboard.

For many people, K was the preferred APL dialect. When it was available, it tended to be popular with investment bankers, the performance obsessed, and analysts dealing with lots of data. It is a demanding language.

K was originally designed by Arthur Whitney and [Kx Systems](http://kx.com/). Praise for K should be taken to refer to Kx's K. Kx sells a popular database called KDB+. People can and do create networked trading platforms in hours. If your business needs production support, you can [evaluate KDB+ prior to purchasing from Kx](http://kx.com/software-download.php), or possibly speak with Kx consulting partner [First Derivatives](http://www.firstderivatives.com/).  The 32-bit version of KDB+ is available for free.

Kx's KDB+ uses the Q language, and is built on top of K4. Kx used to sell a database called KDB, which used the KSQL language, and was built on top of K3. Earlier, Kx sold K2 as its primary product. Before K2, UBS had a 5-year exclusive license to K1. To the confusion of all, these terms are used interchangeably. Kx's K3, K2 and K1 are basically no longer available. While you get K4 with KDB+, K4 is proprietary to Kx and no documentation is available. Kona is a reimplementation that targets K3 but includes features inferred from K4 or implemented elsewhere. Kona is unaffiliated with Kx.

Mailing Lists
-------------
[kona-user](https://groups.google.com/forum/#!forum/kona-user) is about using the Kona programming language.

[konda-dev](https://groups.google.com/forum/#!forum/kona-dev) is about developing the Kona language itself.

Installation
------------

**Windows**

You can find an executable version of Kona [here](https://github.com/kevinlawler/kona/releases).  Download k.exe.  Use Windows "Explorer" to move k.exe from the "Download" directory to another directory ... or not.  Double click on k.exe in "Explorer" to start a Kona session.  Alternatively, start a "cmd" console window, navigate to the directory containing k.exe, and key in "k".

**Build from source**

For OSX, Linux, BSD, Cygwin and Android:
Navigate to the directory you want to install Kona, then type:

    git clone https://github.com/kevinlawler/kona.git
    cd kona
    make                                #gmake on BSD

Then, while in the "kona" directory, run:

    ./k                      #./k_test for debug mode

Android builds are similar, but you must set use this command when running `make`:

    make OS=android

For Windows: 
Pretty much the same process, but you will need MinGW-w64 (Mingw-builds project package), and you will need MSYS (or MSYS2) for bash.  You can start up Kona from MSYS, or from a native Windows "cmd" session.  In MSYS, type "./k" or just type "k" when in the "kona" directory. When in "cmd" just type "k" as "./k" won't work.  You can also double-click on k.exe from Windows Explorer.

**Input Issues**

If you experience input issues with the command-line interpreter, such as visible arrow keys, try the `rlwrap` utility and see if it solves your problem.

Further Information
-------------------


You can find further information about Kona at [the wiki](https://github.com/kevinlawler/kona/wiki).



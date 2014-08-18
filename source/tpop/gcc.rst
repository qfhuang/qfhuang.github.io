C程序编译过程
=================

1.预处理(cc1预处理器)

处理预处理指令，如将＃include插入文件,选项 ``-E``

.. code-block:: sh
    
    gcc -E hello.c -o hello.i


２.编译(cc1编译器)

将代码编译生成汇编指令,选项 ``-S``

.. code-block:: sh

    gcc -S hello.i -o hello.s

或

.. code-block:: sh

    /usr/lib/gcc/i486-linux-gnu/4.1/cc1 hello.c


3.汇编(gas汇编器)

将汇编指令汇编成机器码,生成的文件叫目标文件

.. code-block:: sh

    gcc -c hello.c -o hello.o

或

.. code-block:: sh

    as hello.s -o hello.o


４.链接(ld链接器)

将多个目标文件，链接成一个可执行文件

.. code-block:: sh

    gcc hello.o -o hello

或

.. code-block:: sh

    ld a.o b.o hello.o -o hello


参考：《程序员的自我修养》

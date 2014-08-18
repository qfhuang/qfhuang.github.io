Call Graph
==============

call graph工具从维基查找下来，分２种类型，动态分析和静态分析。以下是我测试和使用过的几种。

- 动态分析：
1. gcc编译指令
2. gprof

- 静态分析：
1. codeviz
2. egypt
3. calltree.sh
4. callgrphviz

gcc -finstrument-functions option
------------------------------------

这里有一编ibm的文章，讲利用gcc指今，在编译函数的时候自动调用另一些函数打印出调用函数的符号和地址关系。

- [[https://www.ibm.com/developerworks/cn/linux/l-graphvis/][用Graphviz可视化函数调用]]
- [[http://guiquanz.me/2012/10/15/linux_c_call_trace/][巧用Ｇraphviz和pvtrace等工具可视化Ｃ函数调用]]

1.编译

.. code-block:: shell

    make CFLAGS = -o -g -finstrument-functions instrument.c
    cp pvtrace/instrument.c ~/gzip/
    make

2.执行程序

.. code-block:: shell

    ./gzip gzip.doc

生成pvtrace.txt

3.生成dot文件

.. code-block:: shell

    pvtrace gzip

通过可执行文件和pvtrace.txt生成graph.dot

4.将dot文件转换成图像格式

.. code-block:: shell

    dot -Tpng graph.dot -o graph.png

gprof & gprof2dot
====================

- 前提安装graphviz

.. code-block:: shell

    apt-get install python graphviz

- 下载 [[http://code.google.com/p/jrfonseca/wiki/Gprof2Dot][gprof2dot]]

.. code-block:: shell

    chmod 777 gprof2dot.py

- 使用

1.编译选项

.. code-block:: shell

    make CFLAGS = -o -g -gp

2.运行程序，如压缩文件

.. code-block:: shell

    ./gzip gzip.doc 

3.gprof生成调用树

.. code-block:: shell

    gprof gzip | grof2dot.py | dot -Tpng -o output.png

egypt
--------

[[http://qfhuang.github.io/files/images/24637Ong.png]]
- 安装
安装方法见http://www.gson.org/egypt/

- 使用
http://www.aireadfun.com/blog/2012/12/04/use-egypt-to-create-call-graphs/
 
1.编译

.. code-block:: shell

    ./configure

2.生成调用树

.. code-block:: shell

    egypt $(find . -name "*.expand") | dot -Grankdir=LR -Tsvg -o summary.svg

参考：
callgraphivz http://blog.nutsfactory.net/2011/03/30/callgraphviz
http://www.cppblog.com/hacrwang/archive/2007/06/30/27295.html

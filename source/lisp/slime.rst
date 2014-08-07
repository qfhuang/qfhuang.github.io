lisp运行环境
===============

emacs+slime+sbcl
-------------------

``新建文件``            C-x C-f hello.lisp
``加载文件``            C-c C-l hello.lisp (好像不行)
                        (load "hello.lisp")
``加载编译后文件``(fast load-file)
                        (load (compile-file "hello.lisp"))
``编译定义``            C-c C-c


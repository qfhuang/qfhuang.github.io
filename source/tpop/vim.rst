vim 操作
==========

分屏
-------
:sp[lit] file
:vsp[lit] file

切换窗口
-----------
C-w h[jkl]

关闭窗口
---------
q! 关闭所有窗口
C-w c 关闭当前窗口
C-w q 关闭当前窗口，如最后一个则退出


让编辑回到屏幕正中
--------------------
z Enter
zz

打开一个文件
-------------
:open file

文件切换
----------
:bn 或
:ls 
:buf n

cscope查找跳转
------------------
C-\ c[g e i f..] 查找符号
C-T 跳回
C-] 向前跳
:grep 正则表达式查找

omni自动补全
--------------
C-X C-O 自动补全
C-X C-I 头文件函数自动补全

先执行:make命令,相关quickfix命令
---------------------------------
:cc 显示详细错误
:cp 跳到上一个错误
:cn 跳到下一个错误
:cl 列出所有错误
:cw 如果有错误则列出，没有则什么也不做
:copen 打开quickfix
:cclose 关闭quickfix
:col 到一个旧的错误列表
:cnew 到一个较新的错误列表

执行shell命令
---------------
!command
:shell 进入shell界面
exit 退回vim

学习git
=========

你有一个新项目helloworld,需要上传到github。
那么先在github上添加新的respository,然后在本地：

.. code-block:: shell

    mkdir helloworld
    touch README.md
    git init
    git add README.md
    git commit -m "first commit"
    git remote add origin https://github.com/qfhuang/helloworld.git
    git push -u origin master

上传克隆自别人的项目
----------------------

上传jekyll bootstrap到空间：

.. code-block:: shell

    git clone https://github.com/plusjade/jekyll-bootstrap.git USERNAME.github.com
    cd USERNAME.github.com
    git remote set-url origin git@github.com:USERNAME/USERNAME.github.com.git
    git push origin master

克隆项目到本地
-----------------

.. code-block:: shell

    git clone https://github.com/AndreaCrotti/yasnippet-snippets.git 

添加子项目
-------------

添加别的项目到自己的项目中：

.. code-block:: shell

    git submodule add git://github.com/AndreaCrotti/yasnippet-snippets.git snippets

克隆带子模块的项目
---------------------

.. code-block:: shell

    git clone git://github.com/schacon/myproject.git
    cd myproject
    git submodule init
    git submodule update

其实克隆的时候可以加上``--recursive``选项，即可将子模块一并克隆下来:

.. code-block:: shell

    git clone --recursive git://github.com/schacon/myproject.git


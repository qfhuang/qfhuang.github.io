输入字符填充数组惯用法
=======================

.. code-block:: c

    for ( i = 0; i < MAX-1; i++)
        if((s[i] = getchar()) == '\n' || s[i] == EOF) 
            break;

    s[i] = '\0';

遍历链表释放内存
-----------------

.. code-block:: c

    for(; listp; listp = next) {
        next = listp->next;
        free(listp);
    }

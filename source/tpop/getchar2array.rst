输入字符填充数组惯用法
=======================

.. code-block:: c

    for ( i = 0; i < MAX-1; i++)
        if((s[i] = getchar()) == '\n' || s[i] == EOF) 
            break;

    s[i] = '\0';

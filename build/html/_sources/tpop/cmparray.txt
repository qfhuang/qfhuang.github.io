比较两个长度为Ｎ的字符串数组是否相同
===============================

.. code-block:: c

    for(i = 0; i < N; i++) {
        if(strcmp(arrx[i], arry[i]) != 0)
            break;
    }
    if ( i == N)
        return 1;
    return 0;

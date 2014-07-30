完成量
==================

completion
---------------

内核编程中常见的一种模式:

在当前线程之外初始化某个活动，然后等待活动结束。

.. code-block:: c

    <linux/completion.h>
    DECLARE_COMPLETION(my_completion);  /* 静态初始化 */

    struct completion my_completion;
    init_completion(&my_completion);    /* 动态初始化 */

    void wait_for_completion(struct completion *c); /* 不可中断的等待 */

    void complete(struct completion *c);    /* 唤醒一个等待线程 */
    void complete_all(struct completion *c);/* 唤醒所有等待线程 */

    INIT_COMPLETION(struct completion c);   /* 重新初始化，重复使用 */


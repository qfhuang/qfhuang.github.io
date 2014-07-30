自旋锁
==================

自旋锁
---------

信号量(遇忙等待)对互斥是非常有用的工具，但并不是唯一的这类工具。
内核中大多数互斥锁定都通过自旋锁(遇忙循环)，它可在绝对不能休眠的代码中使用，如中断处理例程。

自旋锁只能有两个值：锁定或解锁。它通常实现为某个整数值中的单个 *位* 。

希望获取某特定锁的代码，测试相关的位。

- 如果锁可用，则锁定位被设置，而代码继续。("测试并设置"必须是一个原子操作）
- 如果锁被其他进程获得，则进程进入忙循环并重复检查锁，直到锁可用为止。(自旋过程是不可中断的）

.. code-block:: c

    spinlock_t my_lock = SPIN_LOCK_UNLOCKED;    /* 静态初始化 */

    void spin_lock_init(spinlock_t *lock);      /* 动态初始化 */

    void spin_lock(spinlock_t *lock);      /* 进入临界区前，测试并上锁 */
    void spin_unlock(spinlock_t *lock);    /* 解除锁定 */
    
编写任何拥有自旋锁的代码

- 都必须是原子的。它不能休眠，不能放弃处理器。
- 禁止抢占以避免竞态。
- 在本地CPU上，禁止中断。(原因见P121第二段）
- 必须在可能最短时间内拥有。
  
假如拥有锁的代码进入休眠，或放弃处理器，另一个线程(或中断处理程序）被调度,并执行上锁，因锁已被占用，线程会在此CPU上永远自旋下去。而拥有锁的代码永远无法再释放锁。造成死锁。

.. code-block:: c

    void spin_lock_irqsave(spinlock_t *lock, unsigned long flags); /* 获得自旋锁之前，禁止中断 */
    void spin_lock_irq(spinlock_t *lock);   /* 中断已被禁用，中断标示已保存的情况下 */
    void spin_lock_bh(spinlock_t *lock);    /* 获得自旋锁之前，仅禁止软中断，不会在硬件中断例程中访问自旋锁，但可能在软件中断中访问 */

    /* 对应的释放锁函数 */
    void spin_unlock_irqstore(spinlock_t *lock, unsigned long flags);
    void spin_unlock_irq(spinlock_t *lock);
    void spin_unlock_bh(spinlock_t *lock);

    /*　成功时返回非0，否则返回0 */
    int spin_trylock(spinlock_t *lock);
    int spin_trylock_bh(spinlock_t *lock);

读写者自旋锁
---------------

自旋锁的读写者形式，允许一个写者或多个读者。

.. code-block:: c

    rwlock_t my_rwlock = RW_LOCK_UNLOCKED;  /* 静态初始化 */
    
    rwlock_t my_rwlock;
    rwlock_init(&my_rwlock);    /* 动态初始化 */

    void read_lock(rwlock_t *lock);
    void read_lock_irqsave(rwlock_t *lock, unsigned long flags);
    void read_lock_irq(rwlock_t *lock);
    void read_lock_bh(rwlock_t *lock);

    void read_unlock(rwlock_t *lock);
    void read_unlock_irqsave(rwlock_t *lock, unsigned long flags);
    void read_unlock_irq(rwlock_t *lock);
    void read_unlock_bh(rwlock_t *lock);

    void write_lock(rwlock_t *lock);
    void write_lock_irqsave(rwlock_t *lock, unsigned long flags);
    void write_lock_irq(rwlock_t *lock);
    void write_lock_bh(rwlock_t *lock);

    void write_unlock(rwlock_t *lock);
    void write_unlock_irqsave(rwlock_t *lock, unsigned long flags);
    void write_unlock_irq(rwlock_t *lock);
    void write_unlock_bh(rwlock_t *lock);

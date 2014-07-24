并发机制
===========

同步与互斥
--------------

1. 同步
   
   同步是指多个进程之间的依赖关系。如生产者消费者问题,消费者必须等待生产者把数据准备好。

2. 互斥

   互斥是指多个进程的制约关系。多任务下，在同一时刻只允许一个进程使用临界资源。多个进程要求使用时，必须等待，直到临界资源空闲。

原子操作的实现(x86)
------------------------

- #LOCK前缀锁住内存总线
- 一条指令完成加减1操作 inc dec 

信号量semaphore
------------------

信号量本质上是一个整数值，和一对down和up原子操作配合使用。   (注：原子操作是信号量的前提)

当进程希望进入临界区时，在相应的信号量上调用down操作，

- 如果信号量的值大于0，则信号量减1，进程继续执行。
- 如果信号量的值等于0（或更小），进程将等待直到其他进程释放信号量。

当进程离开临界区，进程在相应的信号量上调用up操作，释放信号量。

信号量可以用于解决生产者/消费者问题。

信号量的实现
---------------

.. code-block:: c

    <asm/semaphore.h>
    struct semaphore sem;
    void sema_init(struct semaphore *sem, int val);

    void down(struct semaphore *sem);   /* 获取信号量 */              
    int down_interruptible(struct semaphore *sem); /* 成功获取返回0,失败返回非0 */
    int down_trylock(struct semaphore *sem);       /* 戌功获取返回0,失败返回非0 */ 

- ``down`` 减小信号量的值，并在必要时 *一直等待*。 
- ``down_interruptiable`` 允许等待在某个信号量上的用户空间进程 *可被用户中断*。被中断后返回非0的值，调用者不会拥有该信号量，调用者应始终检查返回值，并做出相应的响应。
- ``down_trylock`` 如果信号量不可获得，会立即返回一个非0的值，*永远不会休眠*。

.. code-block:: c

    void up(struct semaphore *sem); /* 释放信号量 */


互斥体(mutual exclusion)
--------------------------

当信号量用于互斥时（避免进程同时在一个临界区中运行），信号量的值应初始化为1。

.. code-block:: c

    <asm/semaphore.h>
    /* 静态初始化 */
    DECLARE_MUTEX(name);
    DECLARE_MUTEX_LOCKED(name); /* 互斥体的初始状态是锁定的 */
    
    /* 运行时初始化(动态分配互斥体） */
    void init_MUTEX(struct semapore *sem);          /* #define init_MUTEX(sem)     sema_init(sem, 1) */
    void init_MUTEX_LOCKED(struct semapore *sem);   /* #define init_MUTEX_LOCKED(sem)  sema_init(sem, 0) */

读写信号量(rwsem)
----------------------

在临界区，互斥体只能允许 *一个操作者* ，而不区分进程是读还是写。

一个rwsem可允许 *一个写者* **或** *多个读者* 拥有信号量。

写入者具有更高的优先级，当某个给定的写入者试图进入临界区时，在所有写入者完成其工作之前，不会被允许读取者获得访问。（注：如果有大量的写入者竞争信号量，则这种实现会导致读取者“饿死”，即长时间拒绝读取者访问。为此，最好在很少需要写访问且写入者只会短期占用信号量的时候使用rwsem。

.. code-block:: c

    /*
     * the rw-semaphore definition
     * - if activity is 0 then there are no active readers or writers
     * - if activity is +ve then that is the number of active readers
     * - if activity is -1 then there is one active writer
     * - if wait_list is not empty, then there are processes waiting for the semaphore
     */
    struct rw_semaphore {
        __s32           activity;
        spinlock_t      wait_lock;
        struct list_head    wait_list;
    #ifdef CONFIG_DEBUG_LOCK_ALLOC
        struct lockdep_map dep_map;
    #endif
    };

.. code-block:: c

    <linux/rwsem.h>
    struct rw_semaphore sem;
    void init_rwsem(struct rw_semaphore *sem);

    /* 读者获取信号量 */
    void down_read(struct rw_semaphore *sem);
    int down_read_trylock(struct rw_semaphore *sem);    /* 不等待版本，成功时返回非0，其他情况返回0，注意内核大多数其他函数会在成功是返回0 */

    /* 写者获取信号量*/
    void down_write(struct rw_semaphore *sem);
    int down_write_trylock(struct rw_semaphore *sem);   /* 不等待版本，成功时返回非0，其他情况返回0 */
    
    void downgrade_write(struct rw_semaphore *sem); /* 用来写入完成后，允许其他读取者访问 */
    
    void up_read(struct rw_semaphore *sem);     /* 读者释放信号量 */
    void up_write(struct rw_semaphore *sem);    /* 写者释放信号量 */




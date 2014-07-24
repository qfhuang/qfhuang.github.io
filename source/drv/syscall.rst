系统调用
=========
linux 2.6.35

open的定义：
/fs/open.c

.. code-block:: c

    SYSCALL_DEFINE3(open, const char __user *, filename, int, flags, int, mode)
    {
        long ret;

        if (force_o_largefile())
        flags |= O_LARGEFILE;

        ret = do_sys_open(AT_FDCWD, filename, flags, mode);
        /* avoid REGPARM breakage on x86: */
        asmlinkage_protect(3, ret, filename, flags, mode);
        return ret;
    }

    /* /fs/read_write.c */
    SYSCALL_DEFINE3(read, unsigned int, fd, char __user *, buf, size_t, count)
    {
        struct file *file;
        ssize_t ret = -EBADF;
        int fput_needed;

        file = fget_light(fd, &fput_needed);
        if (file) {
            loff_t pos = file_pos_read(file);   /* 读取文件读写位置 */
            ret = vfs_read(file, buf, count, &pos);
            file_pos_write(file, pos);          /* 更新文件读写位置 */
            fput_light(file, fput_needed);
        }

        return ret;
    }

read过程中，前后分别调用了``file_pos_read``,``file_pos_write``

.. code-block:: c
    
    static inline loff_t file_pos_read(struct file *file)
    {
        return file->f_pos;
    }

    static inline void file_pos_write(struct file *file, loff_t pos)
    {
        file->f_pos = pos;
    }

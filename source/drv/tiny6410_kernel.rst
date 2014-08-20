内核移植到tiny6410
=======================

1. 安装交叉工具链
2. 下载源码
    ftp://ftp.kernel.org/pub/linux/kernel/v2.6/linux-2.6.38.tar.bz2

3. vi Makefile

.. code-block:: c
    
    ARCH ?= ARM
    CROSS_COMPILE = arm-linux-

一、Nandflash MLC驱动
----------------------

1. 复制友善之臂源码的部分文件

.. code-block:: sh
    
    drivers/mtd/nand/s3c_nand.c
    arch/arm/plat-samsung/include/plat/regs-nand.h
    drivers/mtd/nand/s3c_nand_mlc.fo
    drivers/mtd/nand/nand_base.c    
    drivers/mtd/nand/s3c_nand.c
    drivers/mtd/nand/Kconfig

2. vi drivers/mtd/nand/Makefile

20行添加:

.. code-block:: c

    obj-$(CONFIG_MTD_NAND_S3C)            += s3c_nand.o

末尾增加:

.. code-block:: c

    S3C_NAND_MLC_SRC = $(shell ls drivers/mtd/nand/s3c_nand_mlc.c 2>/dev/null)
    ifeq ($(S3C_NAND_MLC_SRC),)
    obj-$(CONFIG_MTD_NAND_S3C)            += s3c_nand_mlc.fo
    else
    obj-$(CONFIG_MTD_NAND_S3C)            += s3c_nand_mlc.o
    endif
    

3. cp arch/arm/configs/s3c6400_defconfig .config
4. make menuconfig

.. code-block:: c

    System Type->[*] MINI6410

    Device Drivers---> 
        <*> Memory Technology Device (MTD) support  --->
            [*]   MTD partitioning support
            [*]     Command line partition table parsing 
            <*>   Direct char device access to MTD devices 
            <*>   Caching block device access to MTD devices
            <*>   NAND Device Support  --->
                < >   NAND Flash support for Samsung S3C SoCs  去掉不要选
                <*>   NAND Flash support for S3C SoC  
                [*]     S3C NAND Hardware ECC

5. make zImage 
内核已经可以被启动，但会出现没有文件系统的出错。

二、dm9000网卡驱动和nfs加载文件系统
-------------------------------------

1. vi include/linux/dm9000.h

28行增加:

.. code-block:: c
 
    unsigned char    param_addr[6];

2 vi arch/arm/mach-s3c64xx/mach-mini6410.c 

.. code-block:: c

    if 0
    static struct resource mini6410_dm9k_resource[] = {
            [0] = {
                    .start  = S3C64XX_PA_XM0CSN1,
                    .end    = S3C64XX_PA_XM0CSN1 + 1,
                    .flags  = IORESOURCE_MEM
            },
            [1] = {
                    .start  = S3C64XX_PA_XM0CSN1 + 4,
                    .end    = S3C64XX_PA_XM0CSN1 + 5,
                    .flags  = IORESOURCE_MEM
            },
            [2] = {
                    .start  = S3C_EINT(7),
                    .end    = S3C_EINT(7),
                 .flags  = IORESOURCE_IRQ | IORESOURCE_IRQ_HIGHLEVEL
            }
    };

    static struct dm9000_plat_data mini6410_dm9k_pdata = {
            .flags          = (DM9000_PLATF_16BITONLY | DM9000_PLATF_NO_EEPROM),
    };

    static struct platform_device mini6410_device_eth = {
            .name           = "dm9000",
            .id             = -1,
            .num_resources  = ARRAY_SIZE(mini6410_dm9k_resource),
            .resource       = mini6410_dm9k_resource,
            .dev            = {
                    .platform_data  = &mini6410_dm9k_pdata,
            },
    };

    #endif

增加:

.. code-block:: c

    #define S3C64XX_PA_DM9000       (0x18000000)
    #define S3C64XX_SZ_DM9000       SZ_1M
    #define S3C64XX_VA_DM9000       S3C_ADDR(0x03b00300)

    static struct resource dm9000_resources[] = {
            [0] = {
                    .start          = S3C64XX_PA_DM9000,
                    .end            = S3C64XX_PA_DM9000 + 3,
                    .flags          = IORESOURCE_MEM,
            },
            [1] = {
                    .start          = S3C64XX_PA_DM9000 + 4,
                    .end            = S3C64XX_PA_DM9000 + S3C64XX_SZ_DM9000 - 1,
                    .flags          = IORESOURCE_MEM,
            },
            [2] = {
                    .start          = IRQ_EINT(7),
                    .end            = IRQ_EINT(7),
                    .flags          = IORESOURCE_IRQ | IRQF_TRIGGER_HIGH,
            },
    };

    static struct dm9000_plat_data dm9000_setup = {
            .flags                  = DM9000_PLATF_16BITONLY,
            .dev_addr               = { 0x08, 0x90, 0x00, 0xa0, 0x90, 0x90 },
    };

    static struct platform_device s3c_device_dm9000 = {
            .name                   = "dm9000",
            .id                             = 0,
            .num_resources  = ARRAY_SIZE(dm9000_resources),
            .resource               = dm9000_resources,
            .dev                    = {
                    .platform_data = &dm9000_setup,
            }
    };

如果不嫌麻烦，也可以直接在原代码上改，S3C64XX_PA_XM0CSN1的值也为0x18000000的。

增加mac地址设置函数:

.. code-block:: c

    static int __init dm9000_set_mac(char *str) {
            unsigned char addr[6];
            unsigned int val;
            int idx = 0;
            char *p = str, *end;
            while (*p && idx < 6) {
                    val = simple_strtoul(p, &end, 16);
                    if (end <= p) {
                            /* convert failed */
                            break;
                    } else {
                            addr[idx++] = val;
                            p = end;
                            if (*p == ':'|| *p == '-') {
                                    p++;
                            } else {
                                    break;
                            }
                    }
            }
             if (idx == 6) {
                    printk("Setup ethernet address to %pM\n", addr);
                    memcpy(dm9000_setup.param_addr, addr, 6);
            }

            return 1;
    }

    __setup("ethmac=", dm9000_set_mac);

增加映射结构体:

.. code-block:: c

    static struct map_desc mini6410_iodesc[] = {
    #ifdef CONFIG_DM9000
            {
                    .virtual        = (u32)S3C64XX_VA_DM9000,
                    .pfn            = __phys_to_pfn(S3C64XX_PA_DM9000),
                    .length         = S3C64XX_SZ_DM9000,
                    .type           = MT_DEVICE,
            },
    #endif
    };

然后在static void __init mini6410_map_io(void)函数中将

.. code-block:: c

    s3c64xx_init_io(NULL, 0);

改成

.. code-block:: c

  s3c64xx_init_io(mini6410_iodesc, ARRAY_SIZE(mini6410_iodesc));

最后在

.. code-block:: c

    static struct platform_device *mini6410_devices[] __initdata 
    
的初始化设备中将

.. code-block:: c

    &mini6410_device_eth,

改成

.. code-block:: c

    &s3c_device_dm9000,

3、vi drivers/net/dm9000.c

大概1586行将:

.. code-block:: c

    mac_src = "platform data";
    memcpy(ndev->dev_addr, pdata->dev_addr, 6);

改成:

.. code-block:: c

  mac_src = "param data";
  memcpy(ndev->dev_addr, pdata->param_addr, 6);

大概1598行增加:

.. code-block:: c

   if (!is_valid_ether_addr(ndev->dev_addr) && pdata != NULL) {
                mac_src = "platform data";
                memcpy(ndev->dev_addr, pdata->dev_addr, 6);
        }


4、make menuconfig

.. code-block:: c

        [*] Networking support  ---> 
                Networking options  --->  
               <*> Packet socket                                                
                 <*> Unix domain sockets  
                [*] TCP/IP networking 
                  [*]   IP: kernel level autoconfiguration                       
                      [*]     IP: DHCP support                                        
                          [*]     IP: BOOTP support                                        
                      [*]     IP: RARP support 
    Device Drivers  --->
        [*] Network device support  ---> 
            [*]   Ethernet (10 or 100Mbit)  --->
                 <*>   DM9000 support  
            [ ]   Ethernet (1000 Mbit)  --->   去掉

其他的默认

编译出来，在引导信息中就能看到dm9000网卡信息了。

.. code-block:: sh

    dm9000 Ethernet Driver, V1.31
    eth0: dm9000a at d0930000,d0e00004 IRQ 108 MAC: 08:90:90:90:

5、增加NFS挂载功能

make menuconfig

.. code-block:: c

    File systems  ---> 
        Network File Systems  ---> 
            <*>   NFS client support  
             [*]   Root file system on NFS   这个一点要选
            <*>   NFS server support

make zImage

之后编译出来的内核就能挂载nfs文件系统了

.. code-block:: sh

    set bootargs root=nfs nfsroot=192.168.1.21:/opt/rootfs console=ttySAC0,11520 ip=192.168.1.230 

参考：http://www.linuxidc.com/Linux/2012-01/52086p2.htm

参数处理
==============

.. code-block:: c

    for (i =1; i < argc; i++) {
        if(argv[i][0] != '-')
            break;
        switch (argv[i][1]) {
        case 'o':
            outname = &argv[i][2];
            break;
        case 'f':
            form = atoi(&argv[i][2]);
            break;
        case 't':
            to = atoi(&argv[i][2]);
            break;
        ...
        }
    }

; Example JVM assembly
; Tested with JasminXT 2.4

.bytecode 49.0
.source HelloWorld.java
.class public final enum HelloWorld
.super java/lang/Object
.implements java/io/Serializable
.signature "Ljava/lang/Object;Ljava/io/Serializable;"
.enclosing method hw/jasmin.HelloWorldRunner.run()V
.deprecated
.annotation visible HelloWorld
    I I = 0
.end annotation
.debug "Happy debugging!"

.inner interface public InnerInterface inner 'HelloWorld$InnerInterface' outer HelloWorld
.inner class public InnerClass inner HelloWorld$InnerClass outer 'HelloWorld'

.field public volatile transient I I
.field static protected final serialVersionUID 'J' signature "TJ;" = 2147483648
.field annotation protected 'protected' [[[Lcom/oracle/util/Checksums;
	.deprecated
	.signature "[[[Lcom/oracle/util/Checksums;"
	.attribute foo "foo.txt"
	.attribute 'foo' "foo.txt"
.end field
.field public newline I
.field public static defaultString 'Ljava/lang/String;'

.method public <init>()V
    .limit stack 3
.line 7
    .var 0 is self LHelloWorld; from 0 to 1
    aload_0
    invokenonvirtual java/lang/Object/<init>()V
    return
.end method

.method static public main([Ljava/lang/String;)V
    .limit locals 7
    .limit stack 10
    .throws java.lang/RuntimeException
    .catch java/lang.ClassCastException from cast to 'extra_l' using /extra
    .signature "([Ljava/lang/String;)V"
    .stack
        offset /Input
        locals Object java/lang/String
        locals Uninitialized 'End'
        locals Uninitialized 0
        locals Top
        locals Integer
        locals Float
        locals Long
        locals Double
        locals Null
        locals UninitializedThis
        stack Object java/lang/String
        stack Uninitialized End
        stack 'Uninitialized' 0
        stack 'Top'
        stack Integer
        stack Float
        stack Long
        stack Double
        stack Null
        stack UninitializedThis
    .end stack
    .stack use 1 locals
        offset 'extra'
    .end stack
    .stack use locals
    .end stack
.line 0xd
    .var 0 is args [Ljava/lang/String;
    aload_w 0
    arraylength
    ifne /Input
    iconst_1
    anewarray java/lang/String
    checkcast [Ljava/lang/String;
    astore_0
    aload_0
    iconst_0
    ldc "World"
    dup
    putstatic HelloWorld.defaultString Ljava/lang/String;
    aastore
/Input:
    iconst_2
    iconst_3
    multianewarray [[C 2
    astore_1
    aload_1
    iconst_0
    aaload
    astore_2
    aload_1
    iconst_1
    aaload
    astore_3

<<o:
    aload_3
    iconst_0
    invokestatic HelloWorld/int()I
    castore

<<\u0020:
    aload_3
    dconst_1
    dconst_0
    dsub
    d2i
    invokestatic HelloWorld/double()D
    d2i
    castore

<<!:
    aload_3
    lconst_0
    dup2
    lxor
    lconst_1
    dup2
    ladd
    lsub
    lneg
    l2i
    invokestatic HelloWorld/long()J
    l2i
    castore

<<H:
    aload_2
    fconst_0
    fconst_1
    fconst_2
    dup_x2
    fdiv
    fmul
    f2l
    l2i
    swap
    invokestatic HelloWorld/float(F)F
    f2i
    castore

<<e	:
    aload_2
    iconst_1
    i2s
    i2c
    i2b
    iconst_1
    newarray short
    dup
    iconst_0
    iconst_1
    newarray byte
    dup
    iconst_0
    sipush 0x65
    bastore
    iconst_0
    baload
    sastore
    iconst_0
    saload
    int2short
    int2char
    int2byte
    castore

 <<l :
    aload_2
    iconst_2
    bipush 0x1b
*2:
    iconst_1
    ishl
    dup
    lookupswitch
        0: '/lookupswitch'
        0x6c: /lookupswitch
        default: *2
/lookupswitch:
    castore

    ldc2_w 2
    dup2
    lcmp
    .set i 4
    .set 'j' 5
    .var 4 is i I from 'i++' to End
    .var 5 is j I signature "I" from i++ to End
    istore 4
    goto 1
i++:
    iinc 4 1
1:  iconst_0
    istore_w 5
    goto_w 2
j++:
    iinc_w 5 1
2: getstatic java/lang/System/out Ljava/io/PrintStream;
    aload_1
    iload 4
    aaload
    iload_w 5
    caload
    invokevirtual java/io/PrintStream/print(C)V
    iload 5
    iconst_1
    if_icmpne $+6
    jsr extra
    iload 5
    iconst_2
    if_icmplt j++
    iconst_1
    iload 4
    if_icmpgt i++

<<\u00a0:
    getstatic java/lang/System/out Ljava/io/PrintStream;
    invokestatic HelloWorld/get"example()LHelloWorld;
    getfield HelloWorld/newline I
    invokevirtual java/io/PrintStream/print(C)V
End:
    return

extra:
    astore 6
    iload 4
    tableswitch 0 1
        extra_l
        extra_string
        default: 'End'
    nop
extra_string:
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    iconst_0
    aaload
    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
cast:
    ldc java/lang/String
    checkcast java/lang/Class
    pop
    ldc Ljava/lang/String;
    checkcast Ljava/lang/Class;
    pop
    iconst_1
    dup
    newarray boolean
    checkcast [Z
    pop
    newarray 'int'
    checkcast HelloWorld
    checkcast LHelloWorld;
    pop
extra_l:
    getstatic java/lang/System/out Ljava/io/PrintStream;
    dup
    ldc "\123.\456.\u006c.\n.\r.\t.\f.\b.\".\'.\\"
    iconst_5
    invokeinterface java/lang/CharSequence/charAt(I)C 2
    invokevirtual java/io/PrintStream/print(C)V
/extra:
    pop
    ret 6
.end method

.method private static get"example()LHelloWorld;
    .limit locals 3
    .limit stack 4
    .catch all from 7 to 53 using 59
    aconst_null
    dup
    dup
    astore_w 0
try:
    goto $+0x11
finally:
    astore_w 2
    putfield HelloWorld/newline I
    ret_w 2
    nop
    aload_0
    areturn
    ifnonnull $-2
    ifnull $+3
    new HelloWorld
    dup
    dup
    invokespecial HelloWorld/<init>()V
    astore 0
    aload 0
    monitorenter
    monitorexit
    new java/lang/RuntimeException
    dup
    invokespecial java/lang/RuntimeException/<init>()V
    athrow
    aconst_null
/try:
    dup
    aconst_null
    if_acmpeq $+3
    areturn
catch:
    jsr $+10
    aload_0
    dup
    aconst_null
    if_acmpne /try
    areturn
    astore_1
    aload_0
    ldc 10
    jsr_w finally
    ret 1
'single\u0020quoted\u0020label': ; Messes up [@ below if lexed sloppily
.end method

.method varargs private static int()I
    .annotation invisible HelloWorld
        [@ [@ WhatIsThis??? = .annotation ; name, type, exttype
            I I = 1 ; name, type
            another-I I = 2
            Enum e Ljava/util/logging/Level; = FINE
        .end annotation
        .annotation
            s s = "foo"
            another-s s = "bar"
            Enum [e Ljava/util/logging/Level; = FINE FINE 'FINE' FINE
        .end annotation
        float F = 123.456
    .end annotation
    .annotation visibleparam 1 LHelloWorld;
        x [I = 0x01 0x02 0x03
        y I = 2
    .end annotation
    .annotation invisibleparam 255 HelloWorld
        a F = 1.2
        b D = 3.4
    .end annotation
    .annotation default
        I = 0
    .end annotation
    .limit locals 4
    .limit stack 20
    iconst_1
    newarray int
    dup
    dup
    instanceof [Z
    bipush 0x9
    bipush 0xB
    iand
    iconst_5
    iconst_4
    dup_x1
    iconst_m1
    iadd
    bipush +-111
    ineg
    swap
    idiv
    dup_x2
    dup
    ishr
    ishl
    imul
    ior
    bipush -73
    ixor
    isub
    dup
    iconst_1
    iadd
    irem
    iastore
    iconst_0
    iaload
    istore_0
    iload_0
    istore_1
    iload_1
    istore_2
    iload_2
    istore_3
    iload_3
    dup
    dup
    dup2_x1
    if_icmpeq $+33
    dup
    dup
    if_icmpge $+28
    dup
    dup
    if_icmple $+23
    dup
    ifle $+19
    dup
    ifeq $+15
    dup
    iflt $+11
    dup
    ifgt $+7
    dup
    ifge $+3
    ireturn
.end method

.method static private fpstrict double()D
    .limit locals 7
    .limit stack 11
    dconst_1
    dconst_0
    dcmpg
    newarray double
    dup
    dconst_0
    dup2
    dcmpl
    ldc2_w 128.
    ldc2_w -240.221d
    dneg
    ldc2_w 158.d
    dup2
    dadd
    dup2_x2
    drem
    ddiv
    pop2
    dconst_1
    dmul
    d2f
    f2d
    d2l
    l2i
    iconst_2
    iushr
    i2d
    dastore
    iconst_0
    daload
    dstore_0
    dload_0
    dstore_1
    dload_1
    dstore_2
    dload_2
    dstore_3
    dload_3
    dstore 4
    dload 4
    dstore_w 5
    dload_w 5
    dreturn
.end method

.method static long()J
    .limit locals 7
    .limit stack 11
    iconst_1
    newarray long
    dup
    iconst_0
    ldc2_w 5718613688
    ldc2_w 3143486100
    ldc2_w 0x3
    ldiv
    lmul
    ldc2_w -10000000000
    lrem
    ldc_w 0x60
    i2l
    lor
    ldc 0x33
    i2l
    land
    dup2
    iconst_1
    lshl
    iconst_3
    lshr
    iconst_3
    lushr
    ladd
    l2d
    d2l
    l2f
    f2l
    lastore
    iconst_0
    laload
    lstore_0
    lload_0
    lstore_1
    lload_1
    lstore_2
    lload_2
    lstore_3
    lload_3
    lstore 4
    lload 4
    lstore_w 5
    lload_w 5
    lreturn
.end method

.method private static float(F)F
    .limit locals 6
    .limit stack 9
    iconst_1
    newarray float
    dup
    fload_0
    dup
    fcmpg
    fload_0
    dup
    dup
    dup
    dup2_x2
    fadd
    fsub
    fneg
    frem
    ldc 70
    i2f
    fadd
    fadd
    swap
    pop
    fastore
    fload_0
    dup
    fcmpl
    faload
    fstore_0
    fload_0
    fstore_1
    fload_1
    fstore_2
    fload_2
    fstore_3
    fload_3
    fstore 4
    fload 4
    fstore_w 5
    fload_w 5
    freturn
.end method

.method abstract bridge synthetic 'acc1()V'
    breakpoint
.end method

.method native synchronized acc2()V
.end method

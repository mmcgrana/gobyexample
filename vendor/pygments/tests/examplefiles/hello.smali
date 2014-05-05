# To Recreate:
#
# echo -e 'class hello {\n  public static void main(String[] args) {\n
# System.out.println("hi");\n  }\n}\n' > hello.java
# javac -target 1.4 -source 1.4 hello.java
# dx --dex --output=hello.dex hello.class
# baksmali hello.dex
# cat out/hello.smali

.class Lhello;
.super Ljava/lang/Object;
.source "hello.java"


# direct methods
.method constructor <init>()V
    .registers 1

    .prologue
    .line 1
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method

.method public static main([Ljava/lang/String;)V
    .registers 3
    .parameter

    .prologue
    .line 3
    sget-object v0, Ljava/lang/System;->out:Ljava/io/PrintStream;

    const-string v1, "hi"

    invoke-virtual {v0, v1}, Ljava/io/PrintStream;->println(Ljava/lang/String;)V

    .line 4
    return-void
.end method

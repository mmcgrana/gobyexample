user@host:~/path$ ls -a
.  ..  a  b  c
user@host:~/path$ diff -u a b
--- a   2008-07-26 17:10:07.000000000 -0700
+++ b   2008-07-26 17:10:10.000000000 -0700
@@ -1,3 +1,3 @@
 a
-b
+x
 c
user@host:~/path$ echo \
> a
a
user@host:~/path$ su
root@host:~#
sh-3.1$ # on hardy
sh$ # on etch
(virtualenv-name)user@host:~$ ls -a


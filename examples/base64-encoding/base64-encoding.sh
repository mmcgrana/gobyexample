# Строка кодируется в слегка отличающиеся значения с
# помощью стандартных и URL-совместимые base64
# (`+` vs `-`), но они оба декодируются в исходную
# строку по желанию.
$ go run base64-encoding.go
YWJjMTIzIT8kKiYoKSctPUB+
abc123!?$*&()'-=@~

YWJjMTIzIT8kKiYoKSctPUB-
abc123!?$*&()'-=@~

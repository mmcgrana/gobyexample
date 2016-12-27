; Note, clojure lexer is here (and is a good deal more liberal than the language spec:
; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/LispReader.java#L62

(defn valid [#^java.lang.reflect.Method meth]
  [:keyword :#initial-hash :h#sh-in-middle :hash-at-end# #js {:keyword "value"}])

(ns enigma.utils)

(defn kv-map [ks vs] (zipmap (map keyword (map str ks)) vs))
(defn char-get [c, m] ((keyword (str c)) m))
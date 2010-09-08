(ns enum.core)

(defmacro defenum [type def-prefix name1-or-map & more-names]
  (let [[to-id-sym from-id-sym] (map (comp symbol str)
				     (repeat type)
				     ["-id-from-text" "-text-from-id"])
	name-hash (if (map? name1-or-map)
		    name1-or-map
		    (zipmap (list* name1-or-map more-names) (range)))
	names (keys name-hash)
	name-strs (map str names)
	values (vals name-hash)]
    `(do
       ~@(when def-prefix
	   (map #(list
		  'def
		  (->> % key (str def-prefix) symbol)
		  (val %))
		name-hash))
       (def ~to-id-sym ~(zipmap name-strs
				values))
       (def ~from-id-sym ~(zipmap values name-strs)))))



(ns linked-list.core)

(defn fetch-of
  [nodes node k]
  (->
    node
    k
    nodes))

(defn right-of
  [nodes node]
  (fetch-of nodes node :r))

(defn left-of
  [nodes node]
  (fetch-of nodes node :l))

(defn value-of
  [node]
  (:val node))

(defn push-onto
  [nodes node k v]
  (let [k<- (get {:l :r :r :l} k)
        new-node {:val v
                  k<- (value-of node)
                  k (node k)}
        o-node (-> k node nodes)
        o-node (assoc o-node k<- v)
        node (cond-> (assoc node k v)
               (= (value-of o-node) (value-of node)) (assoc k<- v))
        nodes (assoc
                nodes
                (value-of new-node) new-node
                (value-of o-node) o-node
                (value-of node) node)]
    [nodes new-node]))

(defn push-right
  [nodes node v]
  (push-onto nodes node :r v))

(defn push-left
  [nodes node v]
  (push-onto nodes node :l v))

(defn ->list
  [nodes]
  (loop [seen #{}
         node (-> nodes first second)
         rs []]
    (if (or (nil? node) (contains? seen (:val node)))
      rs
      (let [v (value-of node)
            nxt (right-of nodes node)
            rs' (conj rs v)]
        (recur
          (conj seen v)
          nxt
          rs')))))

(defn move
  [nodes node dir cnt]
  (if (zero? cnt)
    node
    (recur
      nodes
      (fetch-of nodes node dir)
      dir
      (dec cnt))))

(defn move-right
  [nodes node cnt]
  (move nodes node :r cnt))

(defn move-left
  [nodes node cnt]
  (move nodes node :l cnt))

(defn remove
  [nodes node]
  (let [v (:val node)
        l (left-of nodes node)
        r (right-of nodes node)
        l (if (= (:val l) (:val r))
            (assoc
              l
              :r (:val r)
              :l (:val r))
            (assoc l :r (:val r)))
        r (assoc r :l (:val l))
        nodes (->
                nodes
                (dissoc v)
                (assoc
                  (:val r) r
                  (:val l) l))]
    nodes))

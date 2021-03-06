(ns quil3d.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn vector [^Double x ^Double y ^Double z]
  [x y z 1.0])

(defn triangle [v1 v2 v3]
  {:vertices [v1 v2 v3]
   :color    [200 200 200]
   :visible? true})

(defn process-line [{:keys [vertices] :as state} line]
  (let [[letter d1 d2 d3] (clojure.string/split line #" ")]
    (condp = letter
      "v" (update state :vertices (fnil conj []) (vector (Double/parseDouble d1) (Double/parseDouble d2) (Double/parseDouble d3)))
      "f" (update state :mesh (fnil conj []) (triangle (nth vertices (dec (Integer/parseInt d1)))
                                                       (nth vertices (dec (Integer/parseInt d2)))
                                                       (nth vertices (dec (Integer/parseInt d3)))))
      state)))

(defn load-mesh-from-file [file]
  (let [seq (line-seq (clojure.java.io/reader file))
        loaded (reduce process-line {} seq)]
    (println "Loaded" (-> loaded :vertices count) "vertices from" file ", resulting in" (-> loaded :mesh count) "triangles")
    (:mesh loaded)))

;; mesh is a vector of triangles
(def unit-cube
  [;; south
   (triangle (vector 0.0 0.0 0.0)
             (vector 0.0 1.0 0.0)
             (vector 1.0 1.0 0.0))
   (triangle (vector 0.0 0.0 0.0)
             (vector 1.0 1.0 0.0)
             (vector 1.0 0.0 0.0))
   ;; north
   (triangle (vector 1.0 0.0 1.0)
             (vector 0.0 1.0 1.0)
             (vector 0.0 0.0 1.0))
   (triangle (vector 1.0 0.0 1.0)
             (vector 1.0 1.0 1.0)
             (vector 0.0 1.0 1.0))
   ;; east
   (triangle (vector 1.0 0.0 1.0)
             (vector 1.0 0.0 0.0)
             (vector 1.0 1.0 0.0))
   (triangle (vector 1.0 0.0 1.0)
             (vector 1.0 1.0 0.0)
             (vector 1.0 1.0 1.0))
   ;; west
   (triangle (vector 0.0 0.0 1.0)
             (vector 0.0 1.0 0.0)
             (vector 0.0 0.0 0.0))
   (triangle (vector 0.0 0.0 1.0)
             (vector 0.0 1.0 1.0)
             (vector 0.0 1.0 0.0))
   ;; top
   (triangle (vector 0.0 1.0 1.0)
             (vector 1.0 1.0 0.0)
             (vector 0.0 1.0 0.0))
   (triangle (vector 0.0 1.0 1.0)
             (vector 1.0 1.0 1.0)
             (vector 1.0 1.0 0.0))
   ;; bottom
   (triangle (vector 0.0 0.0 0.0)
             (vector 1.0 0.0 0.0)
             (vector 1.0 0.0 1.0))
   (triangle (vector 0.0 0.0 0.0)
             (vector 1.0 0.0 1.0)
             (vector 0.0 0.0 1.0))])

(def screen-width 1024)
(def screen-height 768)

(def near 0.1)
(def far 1000.0)
(def aspect-ratio (/ ^Double screen-height ^Double screen-width))
(def fov 90.0)
(def fov-rad (/ -1.0 (Math/tan (* fov 0.5))))

(def projection-matrix
  [[(* aspect-ratio fov-rad) 0.0 0.0 0.0]
   [0.0 fov-rad 0.0 0.0]
   [0.0 0.0 (/ far (- far near)) 1.0]
   [0.0 0.0 (/ (* -1 far near)
               (- far near)) 0.0]])

(defn x-rotation-matrix [^Double theta]
  (let [cos-theta (Math/cos theta)
        sin-theta (Math/sin theta)]
    [[1.0 0.0 0.0 0.0]
     [0.0 cos-theta sin-theta 0.0]
     [0.0 (* -1.0 sin-theta) cos-theta 0.0]
     [0.0 0.0 0.0 1.0]]))

(defn y-rotation-matrix [^Double theta]
  (let [cos-theta (Math/cos theta)
        sin-theta (Math/sin theta)]
    [[cos-theta 0.0 sin-theta 0.0]
     [0.0 1.0 0.0 0.0]
     [(* -1.0 sin-theta) 0.0 cos-theta 0.0]
     [0.0 0.0 0.0 1.0]]))

(defn z-rotation-matrix [^Double theta]
  (let [cos-theta (Math/cos theta)
        sin-theta (Math/sin theta)]
    [[cos-theta sin-theta 0.0 0.0]
     [(* -1.0 sin-theta) cos-theta 0.0 0.0]
     [0.0 0.0 1.0 0.0]
     [0.0 0.0 0.0 1.0]]))

(def identity-matrix
  [[1.0 0.0 0.0 0.0]
   [0.0 1.0 0.0 0.0]
   [0.0 0.0 1.0 0.0]
   [0.0 0.0 0.0 1.0]])

(defn translation-matrix [^Double x ^Double y ^Double z]
  [[1.0 0.0 0.0 0.0]
   [0.0 1.0 0.0 0.0]
   [0.0 0.0 1.0 0.0]
   [x y z 1.0]])

(defn vector-matrix-mult [[vx vy vz vw] [[m00 m10 m20 m30]
                                         [m01 m11 m21 m31]
                                         [m02 m12 m22 m32]
                                         [m03 m13 m23 m33]]]
  (let [x (+ (* vx m00)
             (* vy m01)
             (* vz m02)
             (* vw m03))
        y (+ (* vx m10)
             (* vy m11)
             (* vz m12)
             (* vw m13))
        z (+ (* vx m20)
             (* vy m21)
             (* vz m22)
             (* vw m23))
        w (+ (* vx m30)
             (* vy m31)
             (* vz m32)
             (* vw m33))]
    [x y z w]))


(defn- row-times-column [[r0 r1 r2 r3] [c0 c1 c2 c3]]
  (+ (* r0 c0)
     (* r1 c1)
     (* r2 c2)
     (* r3 c3)))

(defn- get-nth-column [matrix n]
  (reduce (fn [res row] (conj res (nth row n)))
          [] matrix))

(defn- reduce-row [row-num m n]
  (reduce (fn [row column] (conj row (row-times-column (nth m row-num) (get-nth-column n column))))
          [] (range 0 4)))

(defn multiply-matrix [m n]
  (reduce (fn [matrix row-num] (conj matrix (reduce-row row-num m n))) [] (range 0 4)))

(defn vector-add [[ax ay az] [bx by bz]]
  (vector (+ ax bx)
          (+ ay by)
          (+ az bz)))

(defn vector-subtract [[ax ay az] [bx by bz]]
  (vector (- ax bx)
          (- ay by)
          (- az bz)))


(defn cross [[ax ay az] [bx by bz]]
  (vector (- (* ay bz) (* az by))
          (- (* az bx) (* ax bz))
          (- (* ax by) (* ay bx))))


(defn dot-product [[ax ay az] [bx by bz]]
  (+ (* ax bx)
     (* ay by)
     (* az bz)))

(defn vector-scale [[x y z] scale]
  (vector (* x scale)
          (* y scale)
          (* z scale)))

(def vector-mul vector-scale)

(defn vector-div [[x y z] d]
  (vector (/ x d)
          (/ y d)
          (/ z d)))

(defn vector-length [[x y z]]
  (Math/sqrt (+ (* x x)
                (* y y)
                (* z z))))

(defn normalize-vector [v]
  (vector-scale v (/ 1.0 (vector-length v))))

(defn vector-intersect-plane [point-in-plane plane-normal line-start line-end]
  (let [plane-normal (normalize-vector plane-normal)
        plane-dot-product (* -1.0 (dot-product plane-normal point-in-plane))
        ad (dot-product line-start plane-normal)
        bd (dot-product line-end plane-normal)
        t (/ (- (* -1.0 plane-dot-product) ad)
             (- bd ad))
        line-start-to-end (vector-subtract line-end line-start)
        line-to-intersect (vector-mul line-start-to-end t)]
    (vector-add line-start line-to-intersect)))

(comment
  (vector-intersect-plane (vector 10 0 0) (vector 1 0 0) (vector -10 0 0) (vector 10 0 0)))


(defn clip-against-plane
  "Clip a triangle against a plane given by a point in that plane and the normal of the plane,
   returns a vector of either 0, 1 or 2 triangles."
  [point-in-plane, plane-normal, {:keys [vertices] :as triangle}]
  (let [[nx ny nz :as plane-normal] (normalize-vector plane-normal)
        plane-intersect (partial vector-intersect-plane point-in-plane plane-normal)
        dist (fn [[x y z :as point]]
               (let [res (- (+ (* nx x)
                               (* ny y)
                               (* nz z))
                            (dot-product plane-normal point-in-plane))]
                 ;(println "distance from" point "to" plane-normal "=" res)
                 res))
        {:keys [inside-points outside-points]} (reduce (fn [res point]
                                                         (let [d (dist point)]
                                                           (if (>= d 0)
                                                             (update res :inside-points (fnil conj []) point)
                                                             (update res :outside-points (fnil conj []) point))))
                                                       {} vertices)
        inside-count (count inside-points)
        outside-count (count outside-points)]
    (cond
      (= 0 inside-count) (do ;(println "all out")
                           [])
      (= 3 inside-count) [triangle]
      (and (= 1 inside-count)
           (= 2 outside-count)) (do
                                  ;(if (= plane-normal (vector -1.0 0.0 0.0))
                                   ; (println "testing " triangle " against right sizes, 2 out")
                                  [(-> triangle
                                    (assoc :color [0 0 255])
                                    (assoc :vertices [(first inside-points)
                                                      (plane-intersect (first inside-points) (first outside-points))
                                                      (plane-intersect (first inside-points) (second outside-points))]))])
      (and (= 2 inside-count)
           (= 1 outside-count)) (do
                                        ;(println "1 out")
                                        (let [new-triangle-1 (-> triangle
                                                                 (assoc :color [255 0 0])
                                                                 (assoc :vertices [(first inside-points)
                                                                                   (second inside-points)
                                                                                   (plane-intersect (first inside-points) (first outside-points))]))

                                              new-triangle-2 (-> triangle
                                                                 (assoc :color [0 255 0])
                                                                 (assoc :vertices [(second inside-points)
                                                                                   (last (:vertices new-triangle-1))
                                                                                   (plane-intersect (second inside-points) (first outside-points))]))]
                                          [new-triangle-1 new-triangle-2])))))


(defn point-at-matrix [pos target up]
  (let [new-forward (normalize-vector (vector-subtract target pos))
        ;; calculate new up direction
        a (vector-mul up (dot-product up new-forward))
        new-up (normalize-vector (vector-subtract up a))
        new-right (cross new-up new-forward)
        [fx fy fz] new-forward
        [ux uy uz] new-up
        [rx ry rz] new-right
        [px py pz] pos]
    [[rx ry rz 0.0]
     [ux uy uz 0.0]
     [fx fy fz 0.0]
     [px py pz 1.0]]))


(defn matrix-quick-inverse [[[m00 m01 m02 _]
                             [m10 m11 m12 _]
                             [m20 m21 m22 _]
                             [m30 m31 m32 _]]]
  (let [imx (* -1.0 (+ (* m30 m00)
                       (* m31 m10)
                       (* m32 m20)))
        imy (* -1.0 (+ (* m30 m01)
                       (* m31 m11)
                       (* m32 m21)))
        imz (* -1.0 (+ (* m30 m02)
                       (* m31 m12)
                       (* m32 m22)))]
    [[m00 m10 m20 0.0]
     [m01 m11 m21 0.0]
     [m02 m12 m22 0.0]
     [imx imy imz 1.0]]))


(defn project-triangle [triangle]
  (update triangle :vertices #(into [] (map (fn [vertex]
                                              (let [v (vector-matrix-mult vertex projection-matrix)]
                                                (vector-div v (last v))))
                                            %))))

(defn scale-triangle [triangle]
  (update triangle :vertices #(into [] (map (fn [[x y z]]
                                              [(-> x (+ 1.0) (* 0.5 ^Double screen-width))
                                               (-> y (+ 1.0) (* 0.5 ^Double screen-height))
                                               z])
                                            %))))

(def scale-to-view-transducer
  (map scale-triangle))

(defn apply-transformation [triangle transformation-matrix]
  (update triangle :vertices #(map (fn [v]
                                     (let [new-v (vector-matrix-mult v transformation-matrix)]
                                       (vector-div new-v (last new-v))))
                                   %)))



(defn calculate-normals [{:keys [vertices] :as triangle}]
  (let [line1 (vector-subtract (second vertices) (first vertices))
        line2 (vector-subtract (nth vertices 2) (first vertices))]
    (assoc triangle :normal (normalize-vector (cross line1 line2)))))

(defn determine-visibility [{:keys [normal vertices] :as triangle} camera]
  (let [view-direction (vector-subtract (first vertices)
                                        camera)]
    (assoc triangle :visible? (neg? (dot-product normal view-direction)))))

(defn shade-triangle [{:keys [normal] :as triangle} light-direction]
  (let [shade (dot-product normal light-direction)]
    (update triangle :color (fn [[r g b]] [(* shade r)
                                           (* shade g)
                                           (* shade b)]))))

(def project-transducer
  (map project-triangle))

(defn modify-mesh-transducer [t distance camera look-direction yaw]
  (let [translation (translation-matrix 0.0 0.0 distance)
        rot-x (x-rotation-matrix (* t 0.5))
        rot-z (z-rotation-matrix t)

        up-vector (vector 0.0 1.0 0.0)

        ;target-vector (vector 0.0 0.0 1.0)
        ;camera-rotation-matrix (y-rotation-matrix yaw)
        ;look-direction (vector-matrix-mult target-vector camera-rotation-matrix)
        ;look-direction (vector 0.0 0.0 1.0)
        target-vector (vector-add camera look-direction)

        camera-matrix (point-at-matrix camera target-vector up-vector)
        view-matrix (matrix-quick-inverse camera-matrix)
        transform-matrix (-> rot-z
                             (multiply-matrix rot-x)
                             (multiply-matrix translation)
                             (multiply-matrix view-matrix))]
    (map #(apply-transformation % transform-matrix))))

(def calculate-normals-transducer
  (map calculate-normals))

(defn culling-transducer [camera]
  (comp (map #(determine-visibility % camera))
        (filter :visible?)))

(defn shading-transducer [light-direction]
  (map #(shade-triangle % light-direction)))


(def print-transducer
  (map #(do (println %)
            %)))

(defn clip-to-near-plane-reducer [res triangle]
  (apply conj res (clip-against-plane (vector 0.0 0.0 near) (vector 0.0 0.0 1.0) triangle)))

(defn process-planes [n [t & rest :as tris] [[point normal :as plane] & other-planes :as planes]]
  ;(println "called with" tris)
  (if (empty? planes)
    (do

      tris)
    (let [new-tris (clip-against-plane point normal t)
          tris (apply conj rest new-tris)]
      (if (> n 0)
        (recur (dec n) tris other-planes)
        (recur (count new-tris) tris other-planes)))))

(comment
  (clip-against-plane (vector 490.0 0.0 0.0) (vector -1.0 0.0 0.0) {:vertices [[512.0 384.0 0.9875987598759876] [482.36625805838935 354.36625805838935 0.9875987598759876] [482.36625805838935 384.0 0.9875987598759876]], :color [200.0 200.0 200.0], :visible? true, :normal [0.0 0.0 -1.0 1.0]}))

(def screen-planes
  [[(vector 0.0 0.0 0.0) (vector 0.0 1.0 0.0)]
   [(vector 0.0 (double (dec screen-height)) 0.0) (vector 0.0 -1.0 0.0)]
   [(vector 0.0 0.0 0.0) (vector 1.0 0.0 0.0)]
   [(vector (double (dec screen-width)) 0.0 0.0) (vector -1.0 0.0 0.0)]])

(defn clip-to-view-reducer [res {:keys [vertices] :as triangle}]
  ;(if (> (ffirst vertices) 500)
  ;  (println "Should be clipped: " triangle))
  (let [processed (process-planes 1 (list triangle) screen-planes)]
    ;(println processed)
    (apply conj res processed)))

(defn modification-pipeline [{:keys [t camera light-direction look-direction yaw] :as _state}]
  (comp
    (modify-mesh-transducer 0 8.0 camera look-direction yaw)
    calculate-normals-transducer
    ;print-transducer
    (culling-transducer camera)
    (shading-transducer light-direction)))
;print-transducer
;project-transducer
;scale-to-view-transducer))

(def projection-pipeline
  (comp project-transducer
        scale-to-view-transducer))


(defn draw-triangle [x1 y1 x2 y2 x3 y3 color]
  (apply q/stroke color)
  (apply q/fill color)
  (q/triangle x1 y1 x2 y2 x3 y3))

(defn draw-mesh [mesh]
  (let [[{:keys [vertices color visible?] :as triangle} & rest] mesh]
    (if (and triangle visible?)
      (let [[[x1 y1 _] [x2 y2 _] [x3 y3 _]] vertices]
        (draw-triangle x1 y1 x2 y2 x3 y3 color)))
    (if (seq rest)
      (recur rest))))

(defn current-t []
  (System/currentTimeMillis))

(defn update-state [{:keys [mesh yaw t last-t fps] :as state}]
  (let [look-direction (let [target-vector (vector 0.0 0.0 1.0)
                             camera-rotation-matrix (y-rotation-matrix yaw)
                             look-dir (vector-matrix-mult target-vector camera-rotation-matrix)]
                         (vector-div look-dir (last look-dir)))]

    (-> (assoc state :t (/ (- (current-t) (or last-t 0))
                           (/ 100000 fps)))

        (assoc :look-direction look-direction)
        (assoc :to-render (sort-by
                            (fn [{:keys [vertices]}]
                              (let [[v1 v2 v3] vertices]
                                ;; take the average z-value of each vertex
                                (* -1.0 (/ (+ (nth v1 2) (nth v2 2) (nth v3 2))
                                           3.0))))
                            (->> (into [] (modification-pipeline state)
                                       mesh)
                                 (reduce clip-to-near-plane-reducer [])
                                 (into [] projection-pipeline)
                                 (reduce clip-to-view-reducer []))))
        (assoc :last-t (current-t)))))

(defn draw-state [{:keys [to-render camera look-direction] :as _state}]
  ; Clear the sketch by filling it with black color.
  ;(println look-direction)
  ;(println camera)
  (q/background 0)
  (q/fill 255 255 255)
  (draw-mesh to-render))


(defn setup []
  (let [fps 60]
    ; Set frame rate to 30 frames per second.
    (q/frame-rate fps)
    ; Set color mode to HSB (HSV) instead of default RGB.
    (q/color-mode :rgb)
    ; setup function returns initial state. It contains
    ; circle color and position.
    {:mesh            (load-mesh-from-file "resources/axis.obj") ;(load-mesh-from-file "resources/VideoShip.obj");(load-mesh-from-file "resources/teapot.obj");
     :to-render       []
     :camera          (vector 0.0 0.0 0.0)
     :yaw             0.0
     :look-direction  (vector 0.0 0.0 1.0)
     :light-direction (vector 0.0 1.0 -1.0)
     :last-t          0
     :t               0
     :fps      fps}))

(defn handle-key-press [{:keys [look-direction t] :as state} {:keys [key] :as _event}]
  (let [;t (/ t 1000)
        forward-vector (vector-mul look-direction (* 5.0 t))]
    (condp = key
      :up (update state :camera (fn [[x y z]] [x
                                               (+ y (* 8.0 t))
                                               z]))
      :down (update state :camera (fn [[x y z]] [x
                                                 (- y (* 8.0 t))
                                                 z]))
      :left (update state :camera (fn [[x y z]] [(+ x (* 8.0 t))
                                                 y
                                                 z]))
      :right (update state :camera (fn [[x y z]] [(- x (* 8.0 t))
                                                  y
                                                  z]))
      :w (update state :camera #(vector-add % forward-vector))
      :s (update state :camera #(vector-subtract % forward-vector))
      :a (update state :yaw #(- % t))
      :d (update state :yaw #(+ % t))
      state)))

(q/defsketch quil3d
             :title "Let's spin a cube"
             :size [screen-width screen-height]
             ; setup function called only once, during sketch initialization.
             :setup setup
             ; update-state is called on each iteration before draw-state.
             :update update-state
             :draw draw-state
             :features [:keep-on-top]
             ; This sketch uses functional-mode middleware.
             ; Check quil wiki for more info about middlewares and particularly
             ; fun-mode.
             :middleware [m/fun-mode]
             :key-pressed handle-key-press)

(ns emulators.intel8080)

;; a library for emulating the Intel 8080 CPU from the 70s.
;; basically takes a state object consiting or registers, memory and
;; and performs the desired action on it.


(def init-registers
  "Returns a map of the CPU registers at startup (or after a reset"
  {
   :A 0 :FL 0
   :B 0 :C 0
   :D 0 :E 0
   :H 0 :L 0
   :SP 0
   :PC 0
   })

(def register-map { 07 :A 00 :B 01 :C 02 :D 03 :E 04 :H 05 :L } )

(def register-pair-map { 00 :BC 01 :DE 02 :HL 03 :SP })



(defn- ddd-bits [instr] (bit-shift-right (bit-and 2r00111000 instr) 3))
(defn- sss-bits [instr] (bit-and 2r00000111 instr))


;;------------------------------
;; Data Transfer Group
;;------------------------------
(defn- mov-r1-r2 [ register mem io ]
  (let [ instr (mem (register :PC))
        ddd   (register-map (ddd-bits instr))
        sss   (register-map (sss-bits instr)) ]
    (list (assoc register ddd (register sss)) mem io)))


(defn- error-func [a b c] "Something went bad")

(defn instruction-dispatcher
  "Returns the funciton appropriate for processing instruction."
  [instr]
  (cond
    (and (= 0x40 (bit-and 0x40 instr)) (not (= 0x30 (bit-and 0x30 instr))) (not (= 0x06 (bit-and 0x06 instr)))) mov-r1-r2
     :else error-func))


  
(defn step
  "Executes the current instruction on the CPU returning new register, memory and io states"
  [ registers mem io ]
  (let [instruction (mem (registers :PC))]
    ((instruction-dispatcher instruction) registers mem io)))

;; collection of instructions




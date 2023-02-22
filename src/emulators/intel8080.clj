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


;;--------------------------------------------------------------------
;; Helper Functions
;;--------------------------------------------------------------------

(defn- ddd-reg [instr] (register-map (bit-shift-right (bit-and 2r00111000 instr) 3)))
(defn- sss-reg [instr] (register-map (bit-and 2r00000111 instr)))


(defn- reg-op [register-set register op value]
  (assoc register-set register (op value (register-set register))))


(defn- get-long [register labelH labelL ]
  (+ (* 256 (register labelH)) (register labelL)))


(defn- incr-pc
  "Increments the value of the program counter"
  ( [regs val]
   (reg-op regs :PC + val))

  ( [regs]
   (incr-pc regs 1))

)

;;------------------------------
;; Data Transfer Group
;;------------------------------
(defn- mov-r1-r2 [ regs mem io ]
  (let [ instr (mem (regs :PC))
        ddd   (ddd-reg instr)
        sss   (sss-reg instr)]
    (list (assoc regs ddd (regs sss) :PC (+ 1 (regs :PC)) mem io))))


(defn- mov-r-n [regs mem io]
  (let [ addr (get-long regs :H :L)
         ddd  (ddd-reg (mem (regs :PC)))]
    (list
     (assoc regs ddd (mem addr) :PC (+ 1 (regs :PC)))
     mem
     io)))

(defn- mov-n-r [regs mem io]
  (let [ addr (get-long regs :H :L)
        sss  (sss-reg (mem (regs :PC)))
        ]
    (list
     (incr-pc regs)
     (assoc mem addr (regs sss))
     io)))


(defn- mvi-r-d [regs mem io]
  (let [ ddd (ddd-reg (mem (regs :PC)))
        val (mem (+ 1 (regs :PC)))
        ]
    (list
     (assoc regs ddd val :PC (+ 2 (regs :PC)))
     mem
     io)))
        
(defn- mvi-m-d [regs mem io]
  (let [ addr (get-long regs :H :L)
        val  (mem (+ 1 (regs :PC)))
        ]
    (list
     (incr-pc regs 2)
     (assoc mem addr val)
     io)))
                            
(defn- error-func [regs mem io]
  (throw (IllegalArgumentException. (str "Unrecognized instruction " (mem (regs :PC))))))


(defn instruction-dispatcher
  "Returns the function appropriate for processing instruction."
  [instr]
  (cond
    (and (= 0x40 (bit-and 0xC0 instr)) (not (= 0x30 (bit-and 0x38 instr))) (not (= 0x06 (bit-and 0x07 instr)))) mov-r1-r2
    (and (= 0x46 (bit-and 0xC7 instr)) (not (= 0x30 (bit-and 0x38 instr)))) mov-r-n
    (and (= 0x70 (bit-and 0xF8 instr)) (not (= 0x06 (bit-and 0x07 instr)))) mov-n-r
    (and (= 0x06 (bit-and 0xC7 instr)) (not (= 0x30 (bit-and 0x38 instr)))) mvi-r-d
    (= 0x36 instr) mvi-m-d
    :else error-func))


  
(defn step
  "Executes the current instruction on the CPU returning new register, memory and io states"
  [ regs mem io ]
  (let [instruction (mem (regs :PC))]
    ((instruction-dispatcher instruction) regs mem io)))

;; collection of instructions




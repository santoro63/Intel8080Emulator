(ns emulators.intel8080)

;; a library for emulating the Intel 8080 CPU from the 70s.
;; basically takes a state object consiting or registers, memory and
;; and performs the desired action on it.


;; initial value complex structures
(def init-registers { :A 0 :B 0 :C 0 :D 0 :E 0 :H 0 :L 0 :SP 0 :PC 0 })
(def init-flags { :Z 0 :S 0 :P 0 :CY 0 :AC 0 })


;; maps for translating instruction codes to register/pairs
(def register-map { 07 :A 00 :B 01 :C 02 :D 03 :E 04 :H 05 :L } )
(def register-pair-map { 00 [:B :C] 01 [:D :E] 02 [:H :L] 03 [:SP] })


;;--------------------------------------------------------------------
;; Helper Functions
;;--------------------------------------------------------------------

(defn- ddd-reg [instr] (register-map (bit-shift-right (bit-and 2r00111000 instr) 3)))
(defn- sss-reg [instr] (register-map (bit-and 2r00000111 instr)))
(defn- get-rp  [instr]  (register-pair-map (bit-shift-right (bit-and 0x30 instr) 4)))

(defn- reg-op [register-set register op value]
  (assoc register-set register (op value (register-set register))))


(defn- to-long [hi-byte low-byte] (+ (* 256 hi-byte) low-byte))
(defn- get-long [register labelH labelL ]
  (to-long (register labelH) (register labelL)))


(defn- instr3 [mem addr]
  [ (mem addr) (mem (+ 1 addr)) (mem (+ 2 addr)) ])


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
(defn- mov-r1-r2 [ regs flags mem io ]
  (let [ instr (mem (regs :PC))
        ddd   (ddd-reg instr)
        sss   (sss-reg instr)]
    (list
     (assoc regs ddd (regs sss) :PC (+ 1 (regs :PC)))
     flags
     mem
     io)))


(defn- mov-r-n [regs flags mem io]
  (let [ addr (get-long regs :H :L)
         ddd  (ddd-reg (mem (regs :PC)))]
    (list
     (assoc regs ddd (mem addr) :PC (+ 1 (regs :PC)))
     flags
     mem
     io)))

(defn- mov-n-r [regs flags mem io]
  (let [ addr (get-long regs :H :L)
        sss  (sss-reg (mem (regs :PC)))
        ]
    (list
     (incr-pc regs)
     flags
     (assoc mem addr (regs sss))
     io)))


(defn- mvi-r-d [regs flags mem io]
  (let [ ddd (ddd-reg (mem (regs :PC)))
        val (mem (+ 1 (regs :PC)))
        ]
    (list
     (assoc regs ddd val :PC (+ 2 (regs :PC)))
     flags
     mem
     io)))
        
(defn- mvi-m-d [regs flags mem io]
  (let [ addr (get-long regs :H :L)
        val  (mem (+ 1 (regs :PC)))
        ]
    (list
     (incr-pc regs 2)
     flags
     (assoc mem addr val)
     io)))


(defn- lxi-rp-d [regs flags mem io]
  (let [ addr (regs :PC)
        pair (get-rp (mem addr))
        low-byte (mem (+ 1 addr))
        hi-byte  (mem (+ 2 addr))]
    (if (= [:SP] pair)
      (list
       (assoc regs :SP (to-long hi-byte low-byte) :PC (+ 3 addr))
       flags
       mem
       io)
      (list
       (assoc regs (first pair) hi-byte (second pair) low-byte :PC (+ 3 addr))
       flags
       mem
       io)
      )))


(defn- lda-m [regs flags mem io]
  (let [ addr (regs :PC)
        mem-addr (to-long (mem (+ 2 addr)) (mem (+ 1 addr))) ]
    (list
     (assoc regs :A (mem mem-addr) :PC (+ 3 addr))
     flags
     mem
     io)))

(defn- sta-m [regs flags mem io]
  (let [ [instr low-byte high-byte] (instr3 mem (regs :PC)) ]
    (list
     (incr-pc regs 3)
     flags
     (assoc mem (to-long high-byte low-byte) (regs :A))
     io)))

(defn- lhld [regs flags mem io]
  (let [ [instr low-byte high-byte ] (instr3 mem (regs :PC))
        addr (to-long high-byte low-byte) ]
    (list
     (assoc regs :L (mem addr) :H (mem (+ 1 addr)) :PC (+ 3 (regs :PC)))
     flags
     mem
     io)))

(defn- shld [regs flags mem io]
  (let [ [instr low-byte high-byte] (instr3 mem (regs :PC))
        mem-addr (to-long high-byte low-byte) ]
    (list
     (incr-pc regs 3)
     flags
     (assoc mem mem-addr (regs :L) (+ 1 mem-addr) (regs :H))
     io)
    ))

(defn- ldax [regs flags mem io]
  (let [ [labelH labelL] (get-rp (mem (regs :PC)))
        mem-addr (get-long regs labelH labelL) ]
    (list
     (assoc regs :A (mem mem-addr) :PC (+ 1 (regs :PC)))
     flags
     mem
     io)))

(defn- stax [regs flags mem io]
  (let [ [labelH labelL] (get-rp (mem (regs :PC)))
        mem-addr (get-long regs labelH labelL) ]
    (list
     (incr-pc regs 1)
     flags
     (assoc mem mem-addr (regs :A))
     io)))
                                             
(defn- xchg [regs flags mem io]
  (list
   (assoc regs :H (regs :D) :D (regs :H) :L (regs :E) :E (regs :L) :PC (+ 1 (regs :PC)))
   flags
   mem
   io))

;; arithmetich group instructions

(defn- addr [regs flags mem io]
  (let [ reg-label (sss-reg (mem (regs :PC))) ]
    (list
     (assoc regs :A (+ (regs :A) (regs reg-label)) :PC (+ 1 (regs :PC)))
     flags
     mem
     io)))
;; logical group instructions

;; branch group instructions

;; stack, io, machine control group instructions




(defn- error-func [regs flags mem io]
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
    (= 0x01 (bit-and 0xCF instr)) lxi-rp-d
    (= 0x3A instr) lda-m
    (= 0x32 instr) sta-m
    (= 0x2A instr) lhld
    (= 0x22 instr) shld
    (or (= 0x0A instr) (= 0x1A instr)) ldax
    (or (= 0x02 instr) (= 0x12 instr)) stax
    (= 0xEB instr) xchg
    (and (= 0x80 (bit-and 0xF8 instr)) (not (= 0x86 (instr)))) addr
    :else error-func))


  
(defn step
  "Executes the current instruction on the CPU returning new register, memory and io states"
  [ regs flags mem io ]
  (let [instruction (mem (regs :PC))]
    ((instruction-dispatcher instruction) regs flags mem io)))

;; collection of instructions


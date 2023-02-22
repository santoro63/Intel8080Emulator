(ns emulators.intel8080-test
  (:require [emulators.intel8080 :as sut]
            [clojure.test :as t]))


(t/deftest data-movement-tests

  (t/testing "register-to-register"
    (let [ r (assoc sut/init-registers :B 34)
          m [ 0x48 0x00 0x00]
          io []
          next-state (sut/step r m io)]
      (t/is (= 34 ((first next-state) :C)))
      (t/is (= 01 ((first next-state) :PC)))
      ))

  (t/testing "memory-to-register"
    (let [ r (assoc sut/init-registers :H 0x00 :L 0x05)
          m [ 0x56 0x00 0x02 0x03 0x04 0x05 ]
          io [ ]
          next-state (sut/step r m io) ]
      (t/is (= 0x05 ( (first next-state) :D)))
      (t/is (= 0x01 ((first next-state) :PC)))
      ))

  (t/testing "register to memory"
    (let [ r (assoc sut/init-registers :E 22 :H 0x00 :L 0x05)
          m [0x73 0x00 0x00 0x00 0x00 0x00 0x00]
          io []
          next-state (sut/step r m io) ]
      (t/is (= 22 ((second next-state) 0x05)))
      (t/is (= 0x01 ((first next-state) :PC)))
      ))

  (t/testing "immediate to register"
    (let [ r sut/init-registers
          m [ 0x3E 0xFF 0x01 0x02 ]
          io []
          next-state (sut/step r m io) ]
      (t/is (= 255 ((first next-state) :A)))
      (t/is (= 0x02 ((first next-state) :PC)))
      ))

  (t/testing "immediate to memory"
    (let [ r (assoc sut/init-registers :H 0x00 :L 0x04)
          m [ 0x36 0xFF 0x01 0x02 0x03]
          io []
          next-state (sut/step r m io) ]
      (t/is (= 255 ((second  next-state) 0x04)))
      (t/is (= 0x02 ((first next-state) :PC)))
      ))

  (t/testing "load register pair"
    (let [ r sut/init-registers
          m [ 0x21 0x44 0x21 0x00 ]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 0x03 (r-out :PC)))
      (t/is (= 0x44 (r-out :L)))
      (t/is (= 0x21 (r-out :H)))
      )
    (let [ r sut/init-registers
          m [ 0x31 0x44 0x21 0x00 ]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 0x03 (r-out :PC)))
      (t/is (= 0x2144 (r-out :SP)))
      ))
  
  
  )        

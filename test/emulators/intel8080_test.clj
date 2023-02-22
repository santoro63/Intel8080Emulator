(ns emulators.intel8080-test
  (:require [emulators.intel8080 :as sut]
            [clojure.test :as t]))


(t/deftest data-movement-tests

  (t/testing "register-to-register"
    (let [ r (assoc sut/init-registers :B 34)
          m [ 0x48 0x00 0x00]
          io []
          [r-out mem-out io-out] (sut/step r m io)]
      (t/is (= 34 (r-out :C)))
      (t/is (= 01 (r-out :PC)))
      ))

  (t/testing "memory-to-register"
    (let [ r (assoc sut/init-registers :H 0x00 :L 0x05)
          m [ 0x56 0x00 0x02 0x03 0x04 0x05 ]
          io [ ]
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 0x05 (r-out :D)))
      (t/is (= 0x01 (r-out :PC)))
      ))

  (t/testing "register to memory"
    (let [ r (assoc sut/init-registers :E 22 :H 0x00 :L 0x05)
          m [0x73 0x00 0x00 0x00 0x00 0x00 0x00]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 22 (mem-out 0x05)))
      (t/is (= 0x01 (r-out :PC)))
      ))

  (t/testing "immediate to register"
    (let [ r sut/init-registers
          m [ 0x3E 0xFF 0x01 0x02 ]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 255 (r-out :A)))
      (t/is (= 0x02 (r-out :PC)))
      ))

  (t/testing "immediate to memory"
    (let [ r (assoc sut/init-registers :H 0x00 :L 0x04)
          m [ 0x36 0xFF 0x01 0x02 0x03]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 255 (mem-out 0x04)))
      (t/is (= 0x02 (r-out :PC)))
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

  (t/testing "load accumulator from memory"
    (let [ r sut/init-registers
          m [ 0x3A 0x05 0x00 0x00 0x00 0x14 0x00]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 0x03 (r-out :PC)))
      (t/is (= 0x14 (r-out :A)))
      ))

  (t/testing "load memory from accumulator"
    (let [ r (assoc sut/init-registers :A 0x57)
          m [ 0x32 0x05 0x00 0x00 0x00 0x14 0x00]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 0x03 (r-out :PC)))
      (t/is (= 0x57 (mem-out 0x05)))
      ))

  (t/testing "load HL direct"
    (let [ r sut/init-registers
          m [ 0x2A 0x05 0x00 0x00 0x00 0x18 0x71 ]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 0x03 (r-out :PC)))
      (t/is (= 0x18 (r-out :L)))
      (t/is (= 0x71 (r-out :H)))
      ))

  (t/testing "load mem from HL"
    (let [ r (assoc sut/init-registers :L 0x33 :H 0x22)
          m [ 0x22 0x04 0x00 0x00 0x00 0x00]
          io []
          [r-out mem-out io-out] (sut/step r m io) ]
      (t/is (= 0x03 (r-out :PC)))
      (t/is (= 0x33 (mem-out 0x04)))
      (t/is (= 0x22 (mem-out 0x05)))
      ))
  
  )        

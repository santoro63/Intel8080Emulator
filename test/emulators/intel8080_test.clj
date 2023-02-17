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
  
  )        

(ns emulators.intel8080-test
  (:require [emulators.intel8080 :as sut]
            [clojure.test :as t]))


(t/deftest data-movement-tests

  (t/testing "register-to-register"
    (let [ r (assoc sut/init-registers :B 34)
          m [ 0x48 0x00 0x00]
          io [] ]
      (t/is (= 34 ((first (sut/step r m io)) :C)))))
    
)        

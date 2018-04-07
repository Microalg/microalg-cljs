(ns microalg.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [microalg.parser-test]
            [microalg.interp-test]))

(enable-console-print!)

(doo-tests
  'microalg.parser-test
  'microalg.interp-test)

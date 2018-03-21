(ns microalg.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [microalg.core-test]))

(enable-console-print!)

(doo-tests 'microalg.core-test)

(defsystem "dodgeball"
  :name "dodgeball"
  :version "0.0.1"
  :author "Maris Orbidans"
  :licence "Public Domain"
  :serial t
  :components ((:module "src"
		:serial t
		:components ((:file "dodgeball"))))
  :in-order-to ((test-op (test-op "dodgeball/tests"))))

(defsystem "dodgeball/tests"
  :licence "Public Domain"
  :depends-on (:dodgeball
	       :alexandria
	       :check-it
	       :fiasco)
  :serial t
  :components ((:module "tests"
		:components ((:file "dodgeball-tests"))))
  :perform (test-op (o c) (symbol-call 'fiasco 'all-tests)))

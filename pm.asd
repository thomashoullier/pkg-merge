(defsystem pm
  :name "pm"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Package-merge implementations"
  :depends-on ("binheap")
  :components ((:file "package")
    	       (:file "common" :depends-on ("package"))))